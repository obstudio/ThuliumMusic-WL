(* ::Package:: *)

BeginPackage["document`"];

$StyleSheet = Notebook[
	{
		Cell[StyleData["_Title_"],
			CellMargins -> {{40,22},{20,40}},
			FontFamily -> "Source Sans Pro Semibold",
			FontSize -> 40,
			FontWeight -> "DemiBold",
			FontColor -> RGBColor["#111111"]
		],
		Cell[StyleData["_Text_"],
			CellMargins -> {{30,15},{4,8}},
			FontFamily -> "Calibri",
			FontSize -> 20,
			FontWeight -> "Plain",
			FontColor -> RGBColor["#111111"]
		]
	},
	Visible -> False
];

MakeDocument::nfound = "Cannot find file `1`.";

MakeDocument[filepath_String] := Block[
	{
		rawData,
		i=1,line,
		cells = {},
		context = "_Text_"
	},
	
	If[FileExistsQ[filepath],
		rawData = Import[filepath, "List"],
		Message[MakeDocument::nfound, filepath];
		Return[];
	];
	
	While[i <= Length[rawData],
		line = rawData[[i]];
		i += 1;
		If[StringLength[line] == 0, Continue[]];
		Switch[StringTake[line, 1],
			"#",
				AppendTo[cells, Cell[StringDelete[line,RegularExpression["^# *"]],"_Title_"]],
			_,
				AppendTo[cells, Cell[line,"_Text_"]];
		];
	];
	Quiet@CreateDocument[cells,
		System`ClosingSaveDialog -> False,
		WindowTitle -> "testfile",
		Background -> RGBColor["#F7F7F7"],
		WindowSize -> {1024,768},
		WindowMargins -> {{80,Automatic},{Automatic,60}},
		WindowElements -> {"VerticalScrollBar"},
		StyleDefinitions -> $StyleSheet,
		Saveable -> False,
		Editable -> False,
		ShowCellBracket -> False
	];
];

EndPackage[];


(* ::Input:: *)
(*document`MakeDocument[localPath<>"docs/test.tmd"];*)
