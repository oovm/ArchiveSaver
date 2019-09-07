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
Handler["br", __] := "\n\n";
Handler["input", __] := Nothing;
Handler["img", p_, {}] := Module[
	{proto = Association@p},
	If[!MissingQ@proto["src"],
		Return[StringTemplate["![](`1`)"][First@StringSplit[proto["src"], "@"]]]
	];
];
