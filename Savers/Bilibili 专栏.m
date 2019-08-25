(* ::Package:: *)

(* ::Section:: *)
(*Settings*)


Clear["Global`*"]
SetDirectory@NotebookDirectory[];


(* ::Section:: *)
(*Handler*)


Handler["p", {}, text_] := StringJoin@text;
Handler["b", {}, text_] := StringTemplate["**`1`**"][StringJoin@text];
Handler["i", {}, text_] := StringTemplate["*`1`*"][StringJoin@text];
Handler["br", __] := "\n";
Handler["figcaption", __] := Nothing;
Handler["figure", __] := Nothing;
Handler["img", p_, {}] := Module[
	{proto = Association@p, alt, src},
	If[!MissingQ@proto["data-src"],
		Return[StringTemplate["![](`data-src`)"][proto]]
	];
];


(* ::Section:: *)
(*Saver*)


Saver[l_List] := StringRiffle[Saver /@ l, "\n"];
Saver[s_String] := Saver[ToExpression@Last@StringSplit[s, {"/", "cv"}]];
Saver[i_Integer] := Module[
	{raw, xml, ans},
	raw = Import["https://www.bilibili.com/read/cv" <> ToString@i, {"HTML", "XMLObject"}];
	xml = FirstCase[raw, XMLElement["div", {"class" -> "article-holder"}, a___] :> a, {}, Infinity];
	CopyToClipboard[ans = StringRiffle[xml /. XMLElement -> Handler, "\n\n"]];
	Return@ans
];
