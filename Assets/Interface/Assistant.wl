(* ::Package:: *)

FileSelecter := Block[{candidates},
DynamicModule[{songName},
	SetDirectory[localPath];
	candidates = Select[StringTake[StringReplace["\\"->"/"]/@FileNames["*.tm", "Songs", Infinity], {7, -4}],
		!StringMatchQ[#, Alternatives @@ Join[songs, ignoreList], IgnoreCase -> True]&
	];
	CreateDialog[Column[{
		Spacer[{360,20}],
		SetterBar[Dynamic[songName], candidates, Appearance -> "Vertical"],
		Button["Confirm", DialogReturn[Dynamic[songName]]],
		Spacer[{360,20}]
	}, Alignment -> Center], WindowSize -> All]
]];


(* ::Input:: *)
(*FileSelecter*)


Assistant::exteval = "Assistant requires an external evaluator.";

ignoreList = {"test"};
Assistant := Block[
	{
		candidates,
		songPath,
		songName = ""
	},
	If[Length[ExternalSessions[]] == 0,
		Message[Assistant::exteval];
		Return[];
	];
	SetOptions[$FrontEnd, NotebookBrowseDirectory -> localPath <> "Songs"];
	CreateDialog[{
		Cell[Row[{FileNameSetter[songPath, Appearance -> buttonDisplay["Search"]]}]]
	}, WindowSize -> {800, 600}, WindowTitle -> text["Assistant"]];
];


(* ::Input:: *)
(*Assistant*)


(* ::Input:: *)
(*Main*)
