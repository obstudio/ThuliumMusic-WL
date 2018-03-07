(* ::Package:: *)

BeginPackage["document`"];

$StyleSheet = Notebook[
	{
		Cell[StyleData["Title"],
			CellMargins -> {{72,72},{20,40}},
			FontFamily -> "Source Sans Pro Semibold",
			FontSize -> 60,
			FontWeight -> "DemiBold",
			FontColor -> RGBColor["#111111"]
		],
		Cell[StyleData["Subtitle",
			StyleDefinitions -> StyleData["Title"]],
			FontSize -> 48
		],
		Cell[StyleData["Text"],
			CellMargins -> {{48,15},{4,8}},
			FontFamily -> "Calibri",
			FontSize -> 28,
			FontWeight -> "Plain",
			FontColor -> RGBColor["#111111"]
		],
		Cell[StyleData["Separator1"],
			CellFrame -> {{0, 0}, {0, 2}},
			CellMargins -> {{40, 40}, {1, 1}},
			CellFrameColor -> RGBColor["#777777"],
			CellSize -> {Inherited, 4},
			Selectable -> False
		],
		Cell[StyleData["Separator2"],
			CellFrame -> {{0, 0}, {0, 4}},
			CellMargins -> {{40, 40}, {1, 1}},
			CellFrameColor -> RGBColor["#999999"],
			CellSize -> {Inherited, 4},
			Selectable -> False
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
		markCount,
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
		Which[
			StringMatchQ[line, RegularExpression["-{3,} *"]],
				AppendTo[cells, Cell[" ", "Separator1"]],
			StringMatchQ[line, RegularExpression["={3,} *"]],
				AppendTo[cells, Cell[" ", "Separator2"]],
			StringStartsQ[line, RegularExpression["#"]],
				markCount = StringLength @ StringCases[line, RegularExpression["^#+"]][[1]];
				If[markCount <= 2,
					AppendTo[cells, Cell[
						StringDelete[line, RegularExpression["^#+ *| *#*$"]],
						If[markCount == 1, "Title", "Subtitle"],
						TextAlignment -> If[StringEndsQ[line, RegularExpression["# *"]], Center, Left]
					]]
				],
			True,
				AppendTo[cells, Cell[line, "Text"]];
		];
	];
	
	CreateDocument[cells,
		System`ClosingSaveDialog -> False,
		WindowTitle -> StringSplit[filepath,"/"|"\\"][[-1]],
		Background -> RGBColor["#F7F7F7"],
		WindowSize -> {1024,768},
		WindowMargins -> {{80,Automatic},{Automatic,60}},
		WindowElements -> {"VerticalScrollBar"},
		StyleDefinitions -> $StyleSheet,
		Saveable -> False,
		Editable -> False,
		Copyable -> False,
		ShowCellBracket -> False
	];
];

EndPackage[];


(* ::Input:: *)
(*document`MakeDocument[localPath<>"docs/test.tmd"];*)
