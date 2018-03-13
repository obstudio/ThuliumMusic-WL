(* ::Package:: *)

build`InitializeThulium := Hold[
	localPath = StringReplace[NotebookDirectory[], "\\"->"/"];
	SetDirectory[localPath];
	Scan[Get, FileNames["*.wl", "library", Infinity]];
	Scan[Get, FileNames["*.wl", "package", Infinity]];
	Scan[Get, FileNames["*.wl", "assets", Infinity]];
	Get[localPath <> "Preload.wl"];
];

With[{InitializeThulium = build`InitializeThulium},
	Export[localPath <> "Thulium.nb", CreateDocument[
		{
			Cell["Thulium Music Player v2.1", "Thulium-Title"],
			Cell[BoxData @ RowBox[{
				TemplateBox[{
					StyleBox["Initialize Thulium", "Thulium-TextButton-Content"],
					StyleBox["Click to initialize the kernel.", "Thulium-TextButton-Tooltip"],
					InitializeThulium
				}, "Thulium-TextButton"],
				TemplateBox[{4}, "Spacer1"],
				TemplateBox[{
					StyleBox["Initialize Parser", "Thulium-TextButton-Content"],
					StyleBox["Click to initialize the Parser.", "Thulium-TextButton-Tooltip"],
					Hold @ InitializeParser
				}, "Thulium-TextButton"],
				TemplateBox[{4}, "Spacer1"],
				TemplateBox[{
					StyleBox["Start Front End", "Thulium-TextButton-Content"],
					StyleBox["Click to run the front end.", "Thulium-TextButton-Tooltip"],
					Hold @ Main
				}, "Thulium-TextButton"]
			}], "Thulium-Controls"],
			SpacerCell[{24, 16}, 2, CellFrameColor -> GrayLevel[0.7], CellTags -> "Bottom-Of-Head"],
			Cell[BoxData @ MakeBoxes[
				AudioStop[];
			], "Input"]
		},
		StyleDefinitions -> StyleSheet["Thulium"],
		CellGrouping -> Manual,
		WindowTitle -> "Thulium",
		WindowElements -> {"VerticalScrollBar"},
		WindowSize -> {1024, 768},
		Magnification -> 1.5,
		Saveable -> False
	]];
]

