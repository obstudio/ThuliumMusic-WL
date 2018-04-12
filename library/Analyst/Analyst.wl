(* ::Package:: *)

ParsePitch[token_]:=Block[
	{
		PitchOpDict=<|"#"->1,"b"->-1,"'"->12,","->-12|>
	},
	Null
];
AnalizeTonality[tokens_]:=Block[
	{
		enumerator=ConstantArray[0,7]
	},
	Do[
		Switch[token[["Type"]],
			"Note",
				Null
		],
	{token,tokens}];
];
