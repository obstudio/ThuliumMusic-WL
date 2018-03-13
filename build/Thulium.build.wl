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
			Cell["Thulium Music Player v2.1", "Thulium-Title", CellTags -> "$title"],
			Cell[BoxData @ RowBox[{
				TemplateBox[{
					StyleBox["Initialize Thulium", "Thulium-TextButton-Content"],
					StyleBox["Click to initialize the kernel.", "Thulium-TextButton-Tooltip"],
					InitializeThulium
				}, "Thulium-TextButton"],
				TemplateBox[{4}, "Spacer1"],
				TemplateBox[{
					StyleBox["Initialize Parser", "Thulium-TextButton-Content"],
					StyleBox["Click to initialize the parser.", "Thulium-TextButton-Tooltip"],
					Hold[
						NotebookLocate["$init"];
						NotebookWrite[EvaluationNotebook[], Cell[
							BoxData @ MakeBoxes[InitializeParser],
							"Thulium-Initialization", CellTags -> "$init"
						], All];
						SelectionEvaluate[EvaluationNotebook[]];
						NotebookLocate["$title"];
					]
				}, "Thulium-TextButton"],
				TemplateBox[{4}, "Spacer1"],
				TemplateBox[{
					StyleBox["Initialize Songs", "Thulium-TextButton-Content"],
					StyleBox["Click to initialize the songs.", "Thulium-TextButton-Tooltip"],
					Hold @ update
				}, "Thulium-TextButton"],
				TemplateBox[{4}, "Spacer1"],
				TemplateBox[{
					StyleBox["Start Front End", "Thulium-TextButton-Content"],
					StyleBox["Click to run the front end.", "Thulium-TextButton-Tooltip"],
					Hold @ Main
				}, "Thulium-TextButton"]
			}], "Thulium-Controls"],
			Cell[BoxData @ MakeBoxes @ Null, "Thulium-Initialization", CellTags -> "$init"],
			Cell[BoxData @ MakeBoxes[
				AudioStop[];
			], "Input"]
		},
		StyleDefinitions -> StyleSheet["Thulium"],
		CellGrouping -> Manual,
		WindowTitle -> "Thulium",
		WindowElements -> {"VerticalScrollBar"},
		WindowSize -> {1440, 768},
		Magnification -> 1.5,
		Saveable -> False
	]];
]

