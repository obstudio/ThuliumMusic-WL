(* ::Package:: *)

keyDictR=<|-2->"bB",-1->"B",0->"C",1->"#C",2->"D",3->"bE",4->"E",5->"F",6->"#F",7->"G",8->"#G",9->"A"|>;
pitDictR[sd_]:=Switch[sd,
	-1,"%",
	10,"x",
	_,ToString[sd]
];
schemes={
	FontSlant->ToExpression[#Slant],FontWeight->ToExpression[#Weight],FontColor->RGBColor[#Color]
}&@Association@#&/@#&/@Association/@Association@Import[localPath<>"Lib\\QYS\\Color.json"];


detoken[tokenizer_,scheme_]:=Module[
	{
		argument,tokBox,boxes={}
	},
	Do[
		Switch[token[["Type"]],
			"FunctionToken",
				tokBox={StyleBox["\"<\"",scheme[["Function"]]]};
				argument=Association@token[["Argument"]];
				If[token[["Simplified"]]==True,
					Switch[Keys@argument,
						{"Key","Oct"},
							AppendTo[tokBox,StyleBox["\"1=\"",scheme[["FuncName"]]]];
							AppendTo[tokBox,StyleBox[Key[argument[["Key"]]][keyDictR],scheme[["FuncArg"]]]];
							AppendTo[tokBox,StyleBox[Switch[argument[["Oct"]],
								_?Positive,StringRepeat["'",argument[["Oct"]]],
								_?Negative,StringRepeat[",",-argument[["Oct"]]],
								0,""
							],scheme[["FuncArg"]]]],
						{"Bar","Beat"},
							AppendTo[tokBox,StyleBox[argument[["Bar"]],scheme[["FuncArg"]]]];
							AppendTo[tokBox,StyleBox["/",scheme[["FuncName"]]]];
							AppendTo[tokBox,StyleBox[argument[["Beat"]],scheme[["FuncArg"]]]],
						{"Instr","Volume"},
							tokBox=tokBox~Join~Flatten@Riffle[Table[{
								StyleBox[argument[["Instr",i]],scheme[["FuncArg"]]],
								If[argument[["Volume",i]]!=1,{
									StyleBox["(",scheme[["FuncName"]]],
									StyleBox[argument[["Volume",i]],scheme[["FuncArg"]]],
									StyleBox[")",scheme[["FuncName"]]]
								},{Nothing}]},{i,Length@argument[["Instr"]]}],
							StyleBox[",",scheme[["Function"]]]],
						{"Volume"},
							tokBox=tokBox~Join~Riffle[StyleBox[
								If[StringPart[#,-1]==".",#<>"0",#]&@ToString[#],scheme[["FuncArg"]]
							]&/@argument[["Volume"]],
							StyleBox[",",scheme[["Function"]]]],
						{"Instr"},
							tokBox=tokBox~Join~Riffle[
								StyleBox[#,scheme[["FuncArg"]]]&/@argument[["Instr"]],
							StyleBox[",",scheme[["Function"]]]],
						{"Speed"},
							AppendTo[tokBox,StyleBox[argument[["Speed"]],scheme[["FuncArg"]]]];
					],
					Do[
						AppendTo[tokBox,StyleBox[function,scheme[["FuncName"]]]];
						AppendTo[tokBox,StyleBox[":",scheme[["Function"]]]];
						Switch[function,
							"Chord"|"Instr",
								tokBox=tokBox~Join~Riffle[
									StyleBox[#,scheme[["FuncArg"]]]&/@argument[[function]],
								StyleBox[",",scheme[["Function"]]]],
							"Volume",
								tokBox=tokBox~Join~Riffle[StyleBox[
									If[StringPart[#,-1]==".",#<>"0",#]&@ToString[#],scheme[["FuncArg"]]
								]&/@argument[["Volume"]],
								StyleBox[",",scheme[["Function"]]]],
							_,
								AppendTo[tokBox,StyleBox[argument[[function]],scheme[["FuncArg"]]]];
						],
					{function,Keys@argument}];
				];
				AppendTo[tokBox,StyleBox["\">\"",scheme[["Function"]]]],
			"Note",
				tokBox=TemplateBox[{
					StyleBox[Switch[#,-1,"%",10,"x",_,ToString[#]]&[#ScaleDegree],scheme[["Pitch"]]],
					StyleBox[Switch[#SemitonesCount,
						_?Positive,StringRepeat["#",#SemitonesCount],
						_?Negative,StringRepeat["b",-#SemitonesCount],
						0,"\"\""
					],scheme[["PitOp"]]],
					StyleBox[Switch[#OctavesCount,
						_?Positive,StringRepeat["'",#OctavesCount],
						_?Negative,StringRepeat[",",-#OctavesCount],
						0,"\"\""
					],scheme[["PitOp"]]],
					StyleBox["\""<>#ChordSymbol<>"\"",scheme[["PitOp"]]]
				},"RowDefault"]&/@Association/@token[["Pitches"]];
				If[Length@token[["Pitches"]]>1,
					PrependTo[tokBox,StyleBox["\"[\"",scheme[["Chord"]]]];
					AppendTo[tokBox,StyleBox["\"]\"",scheme[["Chord"]]]];
				];
		];
		AppendTo[boxes,TemplateBox[tokBox,"RowDefault"]],
	{token,Association/@tokenizer}];
	Return[TemplateBox[boxes,"RowDefault"]];
];


(* ::Input:: *)
(*Pane[*)
(*	Style[*)
(*		detoken[QYS`getTrack["<1=bD'><Chord:0,12>[1'm2]b`x.<1.0>"],schemes[["Default"]]]//DisplayForm,*)
(*	LineBreakWithin->False],*)
(*	ImageSize->{400,300},Scrollbars->True,*)
(*	BaseStyle->Background->styleColor[["Background"]]*)
(*]*)


(* ::Input:: *)
(*%//CreateDialog;*)
