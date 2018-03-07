(* ::Package:: *)

Assistant::exteval = "Assistant requires an external evaluator.";
Assistant::index = "Assistant requires a song index.";

ignoreList = {"test"};
Assistant := Block[
	{
		candidates, songName = ""
	},
	If[Length[ExternalSessions[]] == 0,
		Message[Assistant::exteval];
		Return[];
	];
	If[!NameQ["songs"], refresh];
	Options[$FrontEnd, NotebookBrowseDirectory] = localPath<>"Songs";
	candidates = Select[StringTake[FileNames["*.tm", "Songs", Infinity], {7, -4}],
		!StringMatchQ[#, Alternatives @@ Join[songs, ignoreList], IgnoreCase -> True]&
	];
	CreateDialog[{
		Cell[Row[{}]]
	}, WindowSize -> {800, 600}, WindowTitle -> text["Assistant"]];
];


(* ::Input:: *)
(*Assistant*)


Options@$FrontEnd
