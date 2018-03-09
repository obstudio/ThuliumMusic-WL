(* ::Package:: *)

StyleSheet["Documemt"] = Notebook[
	{
		Cell[StyleData["Title"],
			FontFamily -> "Source Sans Pro Semibold",
			FontWeight -> "DemiBold"
		],
		Cell[StyleData["Title-chs"],
			FontFamily -> "\:9ed1\:4f53"
		],
		Cell[StyleData["Usage"],
			CellMargins -> {{0, 0}, {0, 0}},
			CellFrame -> {{0, 0}, {1, 1}},
			CellFrameColor -> RGBColor["#77BBFF"],
			Background -> RGBColor["#DDEEFF"],
			FontFamily -> "Cambria",
			FontSize -> 24
		],
		Cell[StyleData["Text"],
			CellMargins -> {{48, 15}, {4, 8}},
			FontFamily -> "Calibri",
			FontSize -> 24
		],
		Cell[StyleData["Separator1"],
			(* CellFrame: {{0, 0}, {0, 2}} *)
			(* CellFrameColor: #777777 *)
			CellMargins -> {{40, 40}, {1, 1}},
			CellSize -> {Inherited, 4},
			Selectable -> False
		],
		Cell[StyleData["Separator2"],
			(* CellFrame: {{0, 0}, {0, 2}} *)
			CellFrameColor -> RGBColor["#77BBFF"],
			CellMargins -> {{0, 0}, {0, 0}},
			CellSize -> {Inherited, 4},
			Selectable -> False
		],
		Cell[StyleData["Section"],
			(* CellMargins: {{48, 48}, {10, 18}} *)
			ShowGroupOpener -> True,
			FontFamily -> "Corbel",
			FontSize -> 32,
			FontColor -> RGBColor["#772200"]
		]
	},
	Visible -> False
];
