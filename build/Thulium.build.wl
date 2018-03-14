(* ::Package:: *)

Get[NotebookDirectory[] <> "Thulium.style.wl"];
localPath = ParentDirectory @ NotebookDirectory[]<> "\\";


build`InitializeThulium := Hold[
	localPath = StringReplace[NotebookDirectory[], "\\"->"/"];
	SetDirectory[localPath];
	Scan[Get, FileNames["*.wl", "library", Infinity]];
	Scan[Get, FileNames["*.wl", "package", Infinity]];
	Scan[Get, FileNames["*.wl", "assets", Infinity]];
	Get[localPath <> "Preload.wl"];
];


With[{InitializeThulium = build`InitializeThulium},
	build`TemporaryNotebook = CreateDocument[
		{
			Cell["Thulium Music Player v2.1", "Thulium-Title", CellTags -> "$title"],
			Cell[BoxData @ RowBox[{
				TemplateBox[{
					"Initialize Thulium",
					"Click to initialize the kernel.",
					InitializeThulium
				}, "Thulium-TextButton"],
				TemplateBox[{4}, "Spacer1"],
				TemplateBox[{
					"Initialize Parser",
					"Click to initialize the parser.",
					Unevaluated @ InitializeParser
				}, "Thulium-TextButton-Monitored"],
				TemplateBox[{4}, "Spacer1"],
				TemplateBox[{
					"Initialize Songs",
					"Click to initialize the songs.",
					Unevaluated @ update
				}, "Thulium-TextButton-Monitored"],
				TemplateBox[{4}, "Spacer1"],
				TemplateBox[{
					"Start Front End",
					"Click to run the front end.",
					Hold @ homepage
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
	];
	NotebookSave[build`TemporaryNotebook, localPath <> "Thulium.nb"];
	NotebookClose[build`TemporaryNotebook];
	NotebookOpen[localPath <> "Thulium.nb"];
]

