(* ::Package:: *)

Begin["SMML`Tokenizer`"];

TrackTokenize[syntax_]:=Module[
	{
		chordNotation,chordOperator,
		pitch,pitches,note,pitchTok,
		objectPatt,objectTok,objPadded,
		functionPatt,functionTok,
		funcName=Alternatives@@syntax[["FunctionList"]],
		trackTok
	},
	
	chordNotation=""|Alternatives@@syntax[["ChordNotation"]];
	chordOperator=Alternatives@@syntax[["ChordOperator"]]...;
	pitch=scaleDegree~~pitOperator~~chordNotation~~chordOperator...;
	pitches="["~~pitch..~~"]"~~pitOperator;
	note=preOperator~~pitch|pitches~~volOperator~~durOperator~~postOperator;
	pitchTok=StringCases[
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
	
	(* object *)
	objectPatt=("{"~~""|(unsigned~~"*")~~subtrack~~"}")|note;
	objectTok=StringCases[{
		"{"~~n:unsigned~~"*"~~sub:subtrack~~"}":>
			{"Type"->"Subtrack","Content"->trackTok[sub],"Repeat"->-ToExpression@n},
		"{"~~sub:subtrack~~"}":>
			{"Type"->"Subtrack","Content"->trackTok[sub],"Repeat"->Max[-1,
				StringCases[sub,"/"~~i:orderList~~":":>orderTok[i]]
			]},
		StringExpression[
			pre:preOperator,
			""|"["~~pts:pitch..~~"]"|"",
			pit:pitOperator,
			vol:volOperator,
			dur:durOperator,
			pst:postOperator
		]:>{
			"Type"->"Note",
			"Pitches"->pitchTok[pts],
			"PitchOperators"->pit,
			"DurationOperators"->dur,
			"VolumeOperators"->vol,
			"Staccato"->StringCount[pst,"`"],
			"Arpeggio"->StringContainsQ[pre,"$"]
		}
	}];
	
	(* function *)
	objPadded=notationPatt...~~objectPatt~~notationPatt...;
	functionPatt=Alternatives[
		"("~~funcName~~":"~~rep[expression]~~")",
		funcName~~"("~~rep[argument]~~")"
	];
	functionTok=StringCases[{
		"("~~name:funcName~~":"~~arg:rep[expression]~~")":>{
			"Type"->"FUNCTION",
			"Name"->name,
			"Simplified"->True,
			"Argument"->({"Type"->"Expression","Content"->#}&)/@StringSplit[arg,","~~WhitespaceCharacter...]
		}
	}];
	
	trackTok=StringCases[{
		func:functionPatt:>functionTok[func][[1]],
		objt:objectPatt:>objectTok[objt][[1]],
		nota:notationPatt:>notationTok[nota][[1]],
		und_:>{"Type"->"Undefined","Content"->und}
	}];
	
	Return[trackTok];
	
];

End[];

