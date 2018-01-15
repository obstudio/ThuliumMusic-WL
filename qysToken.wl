(* ::Package:: *)

getPitchOp[score_,pos_]:=Module[
	{
		i=pos,char,
		semitones=0,octaves=0,chordSymbol=""
	},
	While[i<=StringLength@score && MemberQ[pitchOpList,StringPart[score,i]],
		char=StringPart[score,i];
		Switch[char,
			"#",semitones++,
			"b",semitones--,
			"'",octaves++,
			",",octaves--,
			_,chordSymbol=char
		];
		i++;
	];
	Return[{{
		"SemitonesCount"->semitones,
		"OctavesCount"->octaves,
		"ChordSymbol"->chordSymbol
	},i}];
];


QYSTrackTokenize[score_]:=Module[
	{
		tokens={},i=1,j,
		char,match,content,
		function,argument,position,
		notes,pitchOpData,pitches,
		pitchOperators,
		staccato,arpeggio,
		semitones,octaves,
		durOperators,chordSymbol
	},
	While[i<=StringLength[score],
		char=StringPart[score,i];
		Switch[char,
			"|"|"\\",
				AppendTo[tokens,{
					"Type"->"Barline",
					"Newline"->(char=="\\")
				}];
				i++,
			"<",
				match=Select[Transpose[StringPosition[score,">"]][[1]],#>i&][[1]];
				content=StringTake[score,{i+1,match-1}];
				Which[
					StringContainsQ[content,":"],            (* function *)
						position=StringPosition[content,":"][[1,1]];
						function=StringTake[content,position-1];
						argument=toArgument@StringDrop[content,position];
						AppendTo[tokens,{
							"Type"->"FunctionToken",
							"Name"->function,
							"Argument"->argument
						}],
					StringContainsQ[content,"="],            (* key&oct *)
						AppendTo[tokens,{
							"Type"->"FunctionSimplified",
							"Argument"->{
								"Key"->tonalityDict[[StringDelete[StringTake[content,{3,StringLength@content}],","|"'"]]],
								"Oct"->StringCount[content,"'"]-StringCount[content,","]
							}
						}],
					StringContainsQ[content,"/"],            (* bar&beat *)
						position=StringPosition[content,"/"][[1,1]];
						AppendTo[tokens,{
							"Type"->"FunctionSimplified",
							"Argument"->{
								"Bar"->ToExpression[StringTake[content,position-1]],
								"Beat"->ToExpression[StringDrop[content,position]]
							}
						}],
					StringContainsQ[content,"."],            (* volume *)
						AppendTo[tokens,{
							"Type"->"FunctionSimplified",
							"Argument"->{"Volume"->ToExpression[content]}
						}],
					StringMatchQ[content,NumberString],      (* speed *)
						AppendTo[tokens,{
							"Type"->"FunctionSimplified",
							"Argument"->{"Speed"->ToExpression[content]}
						}],
					True,                                    (* instrument *)
						AppendTo[tokens,{
							"Type"->"FunctionSimplified",
							"Argument"->{"Instr"->content}
						}]
				];
				i=match+1,
			"(",
				match=Select[Transpose[StringPosition[score,")"]][[1]],#>i&][[1]];
				content=StringTake[score,{i+1,match-2}];
				Switch[StringTake[score,{match-1}],
					"~",                            (* tuplet *)
						AppendTo[tokens,{
							"Type"->"Tuplet",
							"NoteCount"->ToExpression[content]
						}],
					"-",                            (* single tremolo *)
						AppendTo[tokens,{
							"Type"->"Tremolo1",
							"StrokeCount"->ToExpression[content]
						}],
					"=",                            (* double tremolo *)
						AppendTo[tokens,{
							"Type"->"Tremolo2",
							"StrokeCount"->ToExpression[content]
						}],
					"^",                            (* appoggiatura *)
						j=1;
						pitches={};
						While[j<=StringLength[content] && DigitQ@StringPart[content,j],
							pitchOpData=getPitchOp[content,j+1];
							pitchOperators=pitchOpData[[1]];
							AppendTo[pitches,Prepend[
								pitchOperators,
								"ScaleDegree"->ToExpression@StringPart[content,j]
							]];
							j=pitchOpData[[2]];
						];
						AppendTo[tokens,{
							"Type"->"Appoggiatura",
							"Notes"->pitches
						}]
				];
				i=match+1,
			"~",                               (* portamento *)
				AppendTo[tokens,{"Type"->"Portamento"}];
				i++,
			"^",                               (* tie *)
				AppendTo[tokens,{"Type"->"Tie"}];
				i++,
			_,
				arpeggio=False;
				staccato=False;
				If[char=="[",
					(* a list of pitches *)
					match=Select[Transpose[StringPosition[score,"]"]][[1]],#>i&][[1]];
					content=StringTake[score,{i+1,match-1}];
					arpeggio=StringContainsQ[content,"^"];
					content=StringDelete[content,"^"];
					j=1;
					pitches={};
					While[j<=StringLength[content] && DigitQ@StringPart[content,j],
						pitchOpData=getPitchOp[content,j+1];
						pitchOperators=pitchOpData[[1]];
						AppendTo[pitches,Prepend[pitchOperators,"ScaleDegree"->ToExpression@StringPart[content,j]]];
						j=pitchOpData[[2]];
					];
					i=match+1;
					pitchOpData=getPitchOp[score,i];
					pitchOperators=pitchOpData[[1]];
					i=pitchOpData[[2]],
					(* one pitch *)
					pitchOpData=getPitchOp[score,i+1];
					pitchOperators=pitchOpData[[1]];
					i=pitchOpData[[2]];
					pitches={Prepend[
						pitchOperators,
						"ScaleDegree"->Switch[char,
							"x",10,
							"%",-1,
							_,ToExpression@char
						]
					]};
					pitchOperators=pitchOpDefault;
				];
				durOperators={};
				While[i<=StringLength[score] && MemberQ[{"-","_",".","`"},StringPart[score,i]],
					char=StringPart[score,i];
					If[char=="`",
						staccato=True,
						AppendTo[durOperators,char]
					];
					i++;
				];
				AppendTo[tokens,{
					"Type"->"Note",
					"Pitches"->pitches,
					pitchOperators[[1]],
					pitchOperators[[2]],
					"Staccato"->staccato,
					"Arpeggio"->arpeggio,
					"DurationOperators"->durOperators
				}];
		];
	];
	Return[tokens];
];


(* ::Input:: *)
(*ExportString[QYSTrackTokenize["<90><Oct:1>"],"JSON"]*)


QYSTokenize[filename_]:=Module[
	{
		i,data,tokenizer={},
		songComments={},
		comments={},
		sections={},
		sectionInfo={},
		tracks={},
		trackToken,
		score=""
	},
	data=Import[filename,"Lines"];
	Do[
		Which[
			line=="",
				If[sectionInfo=={}&&songComments=={},
					songComments=comments;
					comments={}
				],
			StringTake[line,2]=="//",
				AppendTo[comments,StringDrop[line,2]],
			True,
				score=score<>line;
				If[StringPart[line,-1]=="\\",Continue[]];
				trackToken=QYSTrackTokenize[StringDelete[score,Whitespace]];
				If[MemberQ[Association[#][["Type"]]&/@trackToken,"Note"],     (* empty track *)
					AppendTo[tracks,trackToken],
					If[sectionInfo!={},
						AppendTo[sections,Append[sectionInfo,"Tracks"->tracks]];
						tracks={};
					];
					sectionInfo={"Comments"->comments,"GlobalSettings"->trackToken};
					comments={};
				];
				score=""
		],
	{line,data}];
	If[sectionInfo!={},AppendTo[sections,Append[sectionInfo,"Tracks"->tracks]]];
	Return[{
		"Comments"->songComments,
		"Sections"->sections
	}];
];


(* ::Input:: *)
(*Export["E:\\test-Tokenizer.json",QYSTokenize[path<>"Songs\\test.qys"]];*)


(* ::Input:: *)
(*ExportString[QYSTokenize[path<>"Songs\\test.qys"],"JSON"]*)
