(* ::Package:: *)

qymTrackTokenizer[track_]:=Module[
	{
		
	},
	Return[{}];
];

qymTokenizer[filename_]:=Module[
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
		Return[];
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
			globalsettings=qymTrackTokenizer[content[[i]]];
			i++;
		];
		(* Tracks *)
		tracks={};
		While[i<=Length[content] && content[[i]]!="" && (StringLength[content[[i]]]<2 || StringTake[content[[i]],2]!="//"),
			AppendTo[tracks,qymTrackTokenizer[content[[i]]]];
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


(* ::Input:: *)
(*ExportString[qymTokenizer[NotebookDirectory[]<>"Songs\\Frozen\\Let_It_Go.qym"],"JSON"]*)
