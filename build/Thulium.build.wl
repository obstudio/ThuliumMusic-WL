(* ::Package:: *)

build`InitializeThulium := Hold[
	localPath = StringReplace[NotebookDirectory[], "\\"->"/"];
	SetDirectory[localPath];
	Scan[Get, FileNames["*.wl", "library", Infinity]];
	Scan[Get, FileNames["*.wl", "package", Infinity]];
	Scan[Get, FileNames["*.wl", "assets", Infinity]];
	Get[localPath <> "Preload.wl"];
	NotebookLocate["$msg:init"];
	NotebookDelete[];
	NotebookLocate["$init"];
	SelectionMove[EvaluationNotebook[], After, Cell, AutoScroll -> False];
	NotebookWrite[EvaluationNotebook[], ReleaseHold @ WorkBenchTemplate];
	NotebookLocate["$title"];
];


With[{InitializeThulium = build`InitializeThulium},
	build`TemporaryNotebook = CreateDocument[
		{
			Cell[
				BoxData @ RowBox[{
					StyleBox["Thulium Music Player", "TitleText"],
					TemplateBox[{1}, "Spacer1"],
					TemplateBox[{"\"v2.1\""}, "TitleVersion"]
				}],
				"Title", CellTags -> "$title"
			],
			Cell[BoxData @ RowBox[{
				TemplateBox[{
					"Initialize Thulium",
					"Click to initialize the kernel.",
					InitializeThulium
				}, "TextButton"],
				TemplateBox[{4}, "Spacer1"],
				TemplateBox[{
					"Initialize Parser",
					"Click to initialize the parser.",
					Unevaluated @ InitializeParser
				}, "TextButtonMonitored"],
				TemplateBox[{4}, "Spacer1"],
				TemplateBox[{
					"Initialize Songs",
					"Click to initialize the songs.",
					Unevaluated @ update
				}, "TextButtonMonitored"],
				TemplateBox[{4}, "Spacer1"],
				TemplateBox[{
					"Start Front End",
					"Click to run the front end.",
					Hold @ homepage
				}, "TextButton"]
			}], "Controls"],
			Cell[BoxData @ MakeBoxes @ Null, "Initialization", CellTags -> "$init"],
			Cell[BoxData @ TemplateBox[{
				"Please click on the \"Initialize\" buttons in sequence to get started."
			}, "Tip"], "Tip", CellTags -> "$msg:init"]
		},
		StyleDefinitions -> StyleSheet["Thulium"],
		CellGrouping -> Manual,
		Background -> RGBColor["#FFFFFF"],
		WindowTitle -> "Thulium Music Player",
		WindowElements -> {"VerticalScrollBar"},
		WindowSize -> {1440, 768},
		WindowFrame -> "ModelessDialog",
		Magnification -> 2,
		Saveable -> False
	];
	NotebookSave[build`TemporaryNotebook, localPath <> "Thulium.nb"];
	NotebookClose[build`TemporaryNotebook];
	NotebookOpen[localPath <> "Thulium.nb"];
]

