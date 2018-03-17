(* ::Package:: *)

Diagnose[filepath_String]:=Diagnose[Parse[filepath]];
Diagnose[rawData_List]:=Block[
	{
		output={},sectionOutput
	},
	Do[
		sectionOutput={};
		Do[
			If[trackData[["Meta","Warnings"]]!={},
				AppendTo[sectionOutput,OpenerView[{
					"Track: "<>trackData[["ID"]],
					Column[Row[{
						#Err,": ",#Arg
					}]&/@trackData[["Meta","Warnings"]]]
				},True,Method->"Active"]]
			],
		{trackData,sectionData[["Tracks"]]}];
		AppendTo[output,OpenerView[{
			"Section",
			Column[sectionOutput]
		},TrueQ[sectionOutput!={}],Method->"Active"]],
	{sectionData,rawData}];
	If[output!={},Print@Column[output]];
];


(* ::Input:: *)
(*Parse[localPath<>"Songs/Touhou/TH15-Kanjuden/Pandemonic_Planet.sml"][[All,"Warnings"]]*)
