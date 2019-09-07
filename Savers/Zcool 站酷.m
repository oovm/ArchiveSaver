(* ::Package:: *)

(* ::Section:: *)
(*Settings*)


Clear["Global`*"];
SetDirectory@NotebookDirectory[];


(* ::Section:: *)
(*Handler*)


Handler["div", {"class" -> "atricle-text"}, text_] := StringJoin[text];
Handler["div", {"class" -> "video-content-wrap"}, text_] := "Vidio:" <> ToString[text];
Handler["div", {"class" -> "work-show-box"}, text_] := StringJoin[text];
Handler["div", {"class" -> "reveal-work-wrap text-center", "data-index" -> i_}, content_] := Module[
	{data},
	data = <|
		"n" -> ToExpression@i + 1,
		"img" -> content[[2]],
		"text" -> content[[4]]
	|>;
	If[data["text"] == "", Return[data["img"]]];
	StringTemplate["---\n\n(`n`) `text`\n\n`img`"][data]
]
Handler["p", {"class" -> "packaging-competition"}, text_] := StringJoin[text];
Handler["p", {"class" -> "txt-con"}, text_] := StringJoin[text];
Handler["p", {}, text_] := StringRiffle[text,"\n\n"];
Handler["br", __] := "\n\n";
Handler["input", __] := Nothing;
Handler["img", p_, {}] := Module[
	{proto = Association@p},
	If[!MissingQ@proto["src"],
		Return[StringTemplate["![](`1`)"][First@StringSplit[proto["src"], "@"]]]
	];
];



(* ::Section:: *)
(*Saver*)


Saver[l_List] := Module[
	{ans = StringRiffle[Saver /@ l, "\n"]},
	CopyToClipboard@ans;
	Return@ans
];
Saver[url_String] := Module[
	{raw, xml, ans},
	raw = Import[url, {"HTML", "XMLObject"}];
	xml = Switch[
		URLParse[url]["Path"][[2]],
		"work", FirstCase[raw, XMLElement["div", {"class" -> "work-center-con"}, s_] :> s, "", Infinity],
		"article", FirstCase[raw, XMLElement["div", {"class" -> "article-content-wraper"}, s_] :> s, "", Infinity],
		_, Handler["div", {"class" -> "txt-con"}, {"nothing"}]
	];
	ans = StringReplace[
		StringTrim@StringJoin[xml /. XMLElement -> Handler],
		RegularExpression["\\n\\s+"] :> "\n\n"
	];
	CopyToClipboard[ans];
	Return[ans]
];
