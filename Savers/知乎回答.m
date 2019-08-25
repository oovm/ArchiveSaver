(* ::Package:: *)

(* ::Section:: *)
(*Settings*)


Clear["Global`*"]
SetDirectory@NotebookDirectory[];


(* ::Section:: *)
(*Handler*)


Handler["p", {}, text_] := StringJoin@text;
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
	{raw, text, ans},
	raw = Import["https://www.zhihu.com/api/v4/answers/" <> ToString@i <> "?include=data[*].content", "RawJSON"];
	text = ImportString["<div>" <> raw["content"] <> "</div>", "XML"][[2, -1]] /. XMLElement -> Handler;
	CopyToClipboard[ans = StringRiffle[text, "\n\n"]];
	Return@ans
];
