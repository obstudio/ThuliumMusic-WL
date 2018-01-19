(* ::Package:: *)

Begin["qym`"];
simitoneOp=Alternatives[Characters["b#"]]...;
pitOp=Alternatives[Characters["adMmop$,'"]]...;
durOp=Alternatives[Characters["-_."]]...;
pitch=simitoneOp~~"x"|DigitCharacter~~pitOp;

getPitch[pitches_]:=StringCases[pitches,
	simitoneOps:simitoneOp~~pitSd:("x"|DigitCharacter)~~pitOps:pitOp:>{
		"ScaleDegree"->Switch[pitSd,
			"x",
				10,
			_,
				ToExpression[pitSd]
		],
		"SemitonesCount"->StringCount[simitoneOps,"#"]-StringCount[simitoneOps,"b"],
		"OctavesCount"->StringCount[pitOps,"'"]-StringCount[pitOps,","],
		"ChordSymbol"->StringDelete[pitOps,"'"|","]
	}
];

trackTokenizer[track_]:=StringCases[track,{
	(* Function Token *)
	"<1="~~tonality:(LetterCharacter|","|"'"|"#")..~~">":>{
		"Type"->"FunctionToken",
		"Simplified"->True,
		"Argument"->{
			"Key"->tonalityDict[[StringDelete[tonality,","|"'"]]],
			"Oct"->StringCount[tonality,"'"]-StringCount[tonality,","]
		}
	},
	"<"~~bar:int~~"/"~~beat:int~~">":>{
		"Type"->"FunctionToken",
		"Simplified"->True,
		"Argument"->{
			"Bar"->ToExpression[bar],
			"Beat"->ToExpression[beat]
		}
	},
	"<"~~speed:int~~">":>{
		"Type"->"FunctionToken",
		"Simplified"->True,
		"Argument"->{
			"Speed"->ToExpression[speed]
		}
	},
	"<"~~volume:int~~"%>":>{
		"Type"->"FunctionToken",
		"Simplified"->True,
		"Argument"->{
			"Volume"->ToExpression[volume]
		}
	},
	(* Instrument *)
	"{"~~instr:WordCharacter..~~"}":>{
		"Type"->"FunctionToken",
		"Simplified"->True,
		"Argument"->{
			"Instr"->instr
		}
	},
	(* Tuplet *)
	"("~~n:int~~"~)":>{
		"Type"->"Tuplet",
		"NotesCount"->ToExpression[n]
	},
	(* Appoggiatura *)
	"("~~pitches:pitch..~~"^)":>{
		"Type"->"Appoggiatura",
		"Pitches"->getPitch[pitches]
	},
	(* Portamento *)
	"~":>{
		"Type"->"Portamento"
	},
	(* Tie *)
	"^":>{
		"Type"->"Tie"
	},
	(* Note *)
	pitches:(pitch~~"&")...~~pitch~~durOp:durOp:>{
		"Type"->"Note",
		"Pitches"->getPitch[StringDelete[pitches,"&"]],
		"SemitonesCount"->0,
		"OctavesCount"->0,
		"Staccato"->False,
		"Arpeggio"->False,
		"DurationOperators"->durOp
	},
	(* Barline *)
	"|":>{
		"Type"->"BarLine",
		"Newline"->False
	},
	(* Space *)
	" "..:>Nothing,
	(* Undefined *)
	undef__:>{
		"Type"->"Undefined",
		"Content"->undef
	}
}];

tokenizer[filename_]:=Module[
	{
		(* Define variables *)
		i,
		content,
		globalcomments,sections,
		comments,globalsettings,tracks
	},
	(* Read file *)
	If[!FileExistsQ[filename],
		MessageDialog[TextCell["File not found!"],WindowTitle->"Error"];
		Return[False];
	];
	content=Import[filename,"Lines"];
	(* Global comments *)
	i=1;
	globalcomments={};
	While[i<=Length[content] && StringLength[content[[i]]]>=2 && StringTake[content[[i]],2]=="//",
		AppendTo[globalcomments,StringDrop[content[[i]],2]];
		i++;
	];
	(* Sections *)
	sections={};
	While[i<=Length[content],
		(* Blank lines *)
		While[i<=Length[content] && content[[i]]=="",
			i++;
		];
		(* Section comments *)
		comments={};
		While[i<=Length[content] && StringLength[content[[i]]]>=2 && StringTake[content[[i]],2]=="//",
			AppendTo[comments,StringDrop[content[[i]],2]];
			i++;
		];
		(* Global settings *)
		globalsettings={};
		If[i<=Length[content] && content[[i]]!="" && StringTake[content[[i]],-1]==">",
			globalsettings=trackTokenizer[content[[i]]];
			i++;
		];
		(* Tracks *)
		tracks={};
		While[i<=Length[content] && content[[i]]!="" && (StringLength[content[[i]]]<2 || StringTake[content[[i]],2]!="//"),
			AppendTo[tracks,trackTokenizer[content[[i]]]];
			i++;
		];
		(* Construct section *)
		If[tracks!={} || comments!={} || globalsettings!={},
			AppendTo[sections,{
				"Comments"->comments,
				"GlobalSettings"->globalsettings,
				"Tracks"->tracks
			}];
		];
	];
	(* Return tokenized data *)
	Return[{
		"Comments"->globalcomments,
		"Sections"->sections
	}];
];
End[];


(* ::Input:: *)
(*ExportString[tokenizer[NotebookDirectory[]<>"Songs\\Frozen\\Let_It_Go.qym"],"JSON"]*)
