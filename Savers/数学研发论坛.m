(* ::Package:: *)

(* ::Section:: *)
(*Settings*)


Clear["Global`*"];
SetDirectory@NotebookDirectory[];


(* ::Section:: *)
(*Handler*)


Handler["p", {}, text_] := StringJoin@text;
Handler["div", {"class" -> "attach_nopermission attach_tips"}, _] := Nothing;
Handler["br", __] := Nothing;
Handler["a", p_, {t_}] := Module[
	{proto = Association@p},
	StringTemplate["[`1`](`2`)"][t, proto["href"]]
];
Handler["font", p_, {t_}] := Module[
	{proto = Association@p},
	Switch[
		ToExpression@proto["size"],
		5, "## " <> t,
		_, t
	]
];


(* ::Section:: *)
(*Saver*)


CheckMeta[a_, t_] := Module[
	{id = a["id"]},
	If[MissingQ@id, Return@Nothing];
	Return[t]
];
Saver[l_List] := Module[
	{ans = StringRiffle[Saver /@ l, "\n"]},
	CopyToClipboard@ans;
	Return@ans
];
Saver[s_String] := Module[
	{raw, xml, ans},
	raw = Import[s, "Text", CharacterEncoding -> "CP936"];
	xml = Flatten@Cases[
		ImportString[raw, {"HTML", "XMLObject"}],
		XMLElement["td", meta_, t_] :> CheckMeta[Association@meta, t],
		Infinity
	];
	CopyToClipboard[ans = StringJoin[xml /. XMLElement -> Handler]];
	Return@ans
];
