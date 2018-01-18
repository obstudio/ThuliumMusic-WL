(* ::Package:: *)

Begin["qym`"];
trackTokenizer[track_]:=Block[
	{
		match,
		notations,
		argument
	},
	notations={};
	While[track!="",
		Switch[track[[1]],
		"<",
			Which[
			(match=StringCases[track[[1]],RegularExpression["^<1=[#b]?[A-G]>"],1])!={},
				argument=keyAndOctTokenizer[match[[1]]],
			(match=StringCases[track[[1]],RegularExpression["^<\\d+/\\d+>"],1])!={},
				argument=barAndBeatTokenizer[match[[1]]],
			(match=StringCases[track[[1]],RegularExpression["^<\\d+>"],1])!={},
				argument={"Speed"->ToExpression[StringTake[match[[1]],{2,StringLength[match[[1]]]-1}]]},
			True,
				MessageDialog[TextCell["Undefined token found at "<>track],WindowTitle->"Error"];
				Return[False];
			];
			AppendTo[notations,{
				"Type"->"FunctionToken",
				"Simplified"->True,
				"Argument"->argument
			}];
			,
		"{",
			,
		"(",
			,
		"[",
			,
		_,
			
		];
		track=StringDrop[track,StringLength[match[[1]]]];
	];
	
	Return[{}];
];

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
(*ExportString[qym`tokenizer[NotebookDirectory[]<>"Songs\\Frozen\\Let_It_Go.qym"],"JSON"]*)
