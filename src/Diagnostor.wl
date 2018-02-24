(* ::Package:: *)

Diagnose[rawData_]:=Block[
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
						#ErrID,": ",#Args
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
