(* ::Package:: *)

(* ::Input:: *)
(*pitchTok["1Mu2,34ii"]*)


BeginPackage["SMML`Tokenizer`"];

preOperator="$"|"";
postOperator="``"|"`"|"";
volOperator=(">"|":")...;
pitOperator=("#"|"b"|"'"|",")...;
durOperator=("."|"-"|"_"|"=")...;
scaleDegree="0"|"1"|"2"|"3"|"4"|"5"|"6"|"7"|"%"|"x";
chordNotation=""|Alternatives@@{"M","m","a","d","p"};
chordOperator=Alternatives@@{"o","u","i","j"}...;

pitch=scaleDegree~~pitOperator~~chordNotation~~chordOperator;
pitches="["~~pitch..~~"]"~~pitOperator;
note=preOperator~~pitch|pitches~~volOperator~~durOperator~~postOperator;
subtrack=Nest[(("{"~~#~~"}")|Except["{"|"}"])...&,Except["{"|"}"]...,8];

pitchTok[str_]:=StringCases[str,
	StringExpression[
		sd:scaleDegree,
		po:pitOperator,
		cn:chordNotation,
		co:chordOperator
	]:>{
		"ScaleDegree"->sd,
		"PitchOperators"->po,
		"ChordNotations"->cn,
		"ChordOperators"->co
	}
];

include[SMLPath_]:=Module[
	{
		data={},context,command,
		messages={},
		tokenData=<|"ChordNotation"-><||>,"ChordInversion"-><||>|>
	},
	
	If[FileExistsQ[SMLPath],
		data=Import[SMLPath,"List"],
		AppendTo[messages,<|"Type"->"FileNotFound","Arguments"->SMLPath|>]
	];
	Do[
		Switch[line,
			_?(StringStartsQ["!"]),
				command=Cases[StringSplit[StringDrop[line,1]," "..],Except[""]];
				Switch[command[[1]],
					"SMML",
						Switch[command[[2]],
							"Version",,
							_,AppendTo[messages,<|"Type"->"InvalidCommand","Arguments"->line|>]
						],
					"Chord",
						Switch[command[[2]],
							"Notation",context="ChordNotation",
							"Inversion",context="ChordInversion"
						],
					_,
						AppendTo[messages,<|"Type"->"InvalidCommand","Arguments"->line|>]
				],
			_?(StringStartsQ[Except["#"]]),
				AppendTo[tokenData[[context]],Switch[context,
					"ChordNotation",
						#[[1]]->ToExpression/@StringSplit[#[[3]],","~~" "...]&,
					"ChordInversion",
						#[[1]]->ToExpression/@StringSplit[#[[3]],","~~" "...]&
				][StringSplit[line,"\t"..]]]
		],
	{line,data}];
	
	Return[<|
		"Data"->data,
		"Messages"->messages,
		"TokenData"->tokenData
	|>];
];

trackTok[str_]:=StringCases[str,{
	pre:preOperator~~"["~~pts:pitch..~~"]"~~pit:pitOperator~~vol:volOperator~~dur:durOperator~~pst:postOperator:>{
		"Type"->"Note",
		"Pitches"->pitchTok[pts],
		"PitchOperators"->pit,
		"DurationOperators"->dur,
		"VolumeOperators"->vol,
		"Staccato"->StringContainsQ[pre,"$"],
		"Arpeggio"->StringCount[pst,"`"]
	},
	pre:preOperator~~pt:pitch~~vol:volOperator~~dur:durOperator~~pst:postOperator:>{
		"Type"->"Note",
		"Pitches"->pitchTok[pt],
		"PitchOperators"->"",
		"DurationOperators"->dur,
		"VolumeOperators"->vol,
		"Staccato"->StringContainsQ[pre,"$"],
		"Arpeggio"->StringCount[pst,"`"]
	},
	space:Whitespace:>{"Type"->"Whitespace","Content"->space},
	undef__:>{"Type"->"Undefined","Content"->undef}
}];

EndPackage[];


(* ::Input:: *)
(*Contexts["SMML`*"]*)


(* ::Input:: *)
(*trackTok["[231#],-.``12"]*)
