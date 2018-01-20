(* ::Package:: *)

Begin["QYM`"];
simitoneOp=Alternatives[Characters["b#"]]...;
pitOp=Alternatives[Characters["adMmop$,'"]]...;
durOp=Alternatives[Characters["-_."]]...;
pitch=simitoneOp~~"x"|DigitCharacter~~pitOp;
repeatcount=0;

getOrder[ord_]:=Block[
	{
		order
	},
	order=StringCases[ord,{
		n:int~~".":>ToExpression@n
	}];
	repeatcount=Max[repeatcount,order];
	Return[order];
];

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
	(* Repeat *)
	(StartOfString|"|:")~~(subtrack__/;!StringContainsQ[subtrack,"|:"])~~lastvolta:(":||["~~(int~~".")..~~"]"):>{
		"Type"->"Track",
		"Contents"->trackTokenizer[StringDelete[subtrack<>lastvolta,":|"]],
		"Repeat"->If[repeatcount==0,2,repeatcount]
	},
	(StartOfString|"|:")~~(subtrack__/;!StringContainsQ[subtrack,"|:"])~~":|":>{
		"Type"->"Track",
		"Contents"->trackTokenizer[StringDelete[subtrack,":|"]],
		"Repeat"->If[repeatcount==0,2,repeatcount]
	},
	StartOfString~~(subtrack__/;!StringContainsQ[subtrack,"*|"])~~"*|":>{
		"Type"->"Track",
		"Contents"->trackTokenizer[subtrack],
		"Repeat"->-2
	},
	"|["~~order:(int~~".")..~~"]":>{
		"Type"->"BarLine",
		"Newline"->False,
		"Skip"->False,
		"Order"->getOrder[order]
	},
	"|*":>{
		"Type"->"BarLine",
		"Newline"->False,
		"Skip"->True,
		"Order"->{0}
	},
	(* Barline *)
	"|":>{
		"Type"->"BarLine",
		"Newline"->False,
		"Skip"->False,
		"Order"->{0}
	},
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
	"("~~n:int~~")":>{
		"Type"->"Tuplet",
		"NotesCount"->ToExpression[n]
	},
	(* Tremolo *)
	"("~~n:expr~~"-)":>{
		"Type"->"Tremolo1",
		"StrokesCount"->ToExpression[n]
	},
	"("~~n:expr~~"=)":>{
		"Type"->"Tremolo2",
		"StrokesCount"->ToExpression[n]
	},
	(* Fermata *)
	"(.)":>{
		"Type"->"Fermata",
		"Ratio"->2
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
	pitches:((pitch~~"&")...~~pitch)~~durOp:durOp:>{
		"Type"->"Note",
		"Pitches"->getPitch[StringDelete[pitches,"&"]],
		"SemitonesCount"->0,
		"OctavesCount"->0,
		"Staccato"->False,
		"Arpeggio"->False,
		"DurationOperators"->durOp
	},
	(* Space *)
	" "..:>Nothing,
	(* Undefined *)
	undef__:>{
		"Type"->"Undefined",
		"Content"->undef
	}
}];

tokenizer[filename_]:=Block[
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
	(* Initialization *)
	repeatcount=0;
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
(*ExportString[tokenizer[NotebookDirectory[]<>"Songs\\Sunny_Light.qym"],"JSON"]*)
