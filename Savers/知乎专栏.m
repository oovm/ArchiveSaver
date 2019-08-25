(* ::Package:: *)

(* ::Section:: *)
(*Settings*)


Clear["Global`*"]
SetDirectory@NotebookDirectory[];


(* ::Section:: *)
(*Handler*)


Handler["p", {}, text_] := StringJoin@text;
Handler["p", {"class" -> "ztext-empty-paragraph"}, any_] := "\n"
Handler["b", {}, text_] := StringTemplate["**`1`**"][StringJoin@text]
Handler["i", {}, text_] := StringTemplate["*`1`*"][StringJoin@text]
Handler["figure", _, inner_] := Last@inner
Handler["img", p_, {}] := Module[
	{proto = Association@p, alt, src},
	alt = proto["alt"];
	If[!MissingQ@alt,
		If[alt != "[\:516c\:5f0f]", Return[StringTemplate["$`1`$"][alt]]]
	];
	If[!MissingQ@proto["data-actualsrc"],
		Return[StringTemplate["![](`data-actualsrc`)"][proto]]
	];
];
Handler["sup", p_, _] := Module[
	{proto = Association@p},
	StringTemplate["[ref: [`data-numero`]](`data-url`)"][proto]
];


(* ::Section:: *)
(*Saver*)


Saver[l_List] := StringRiffle[Saver /@ l, "\n"];
Saver[s_String] := Saver[ToExpression@Last@StringSplit[s, {"/"}]];
Saver[i_Integer] := Module[
	{raw, xml, ans},
	raw = Import["https://zhuanlan.zhihu.com/p/" <> ToString@i, {"HTML", "XMLObject"}];
	xml = FirstCase[raw, XMLElement["div", {"class" -> "RichText ztext Post-RichText"}, a___] :> a, {}, Infinity];
	CopyToClipboard[ans = StringRiffle[xml /. XMLElement -> Handler, "\n\n"]];
	Return@ans
];
