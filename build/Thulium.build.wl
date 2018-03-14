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
			Cell[
				BoxData @ RowBox[{
					StyleBox["Thulium Music Player", "$TitleText"],
					TemplateBox[{4}, "Spacer1"],
					TemplateBox[{"\"v2.1\""}, "$TitleVersion"]
				}],
				"$Title", CellTags -> "$title"
			],
			Cell[BoxData @ RowBox[{
				TemplateBox[{
					"Initialize Thulium",
					"Click to initialize the kernel.",
					InitializeThulium
				}, "$TextButton"],
				TemplateBox[{4}, "Spacer1"],
				TemplateBox[{
					"Initialize Parser",
					"Click to initialize the parser.",
					Unevaluated @ InitializeParser
				}, "$TextButtonMonitored"],
				TemplateBox[{4}, "Spacer1"],
				TemplateBox[{
					"Initialize Songs",
					"Click to initialize the songs.",
					Unevaluated @ update
				}, "$TextButtonMonitored"],
				TemplateBox[{4}, "Spacer1"],
				TemplateBox[{
					"Start Front End",
					"Click to run the front end.",
					Hold @ homepage
				}, "$TextButton"]
			}], "$Controls"],
			Cell[BoxData @ MakeBoxes @ Null, "$Initialization", CellTags -> "$init"],
			Cell[BoxData @ MakeBoxes[
				AudioStop[];
			], "Input"]
		},
		StyleDefinitions -> StyleSheet["Thulium"],
		CellGrouping -> Manual,
		WindowTitle -> "Thulium",
		WindowElements -> {"VerticalScrollBar"},
		WindowSize -> {1440, 768},
		Magnification -> 2,
		Saveable -> False
	];
	NotebookSave[build`TemporaryNotebook, localPath <> "Thulium.nb"];
	NotebookClose[build`TemporaryNotebook];
	NotebookOpen[localPath <> "Thulium.nb"];
]

