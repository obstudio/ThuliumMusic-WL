(* ::Package:: *)

build`InitializeThulium := Hold[
	localPath = StringReplace[NotebookDirectory[], "\\"->"/"];
	SetDirectory[localPath];
	Scan[Get, FileNames["*.wl", "library", Infinity]];
	Scan[Get, FileNames["*.wl", "package", Infinity]];
	Scan[Get, FileNames["*.wl", "assets", Infinity]];
	Get[localPath <> "Preload.wl"];
	NotebookDelete[Cells[CellTags -> "$msg:init"]];
	NotebookDelete[Cells[CellStyle -> {"Input", "Output", "Music"}]];
	build`benchData = First @ Import[userPath <> "WorkBench.nb", "Notebook"];
	SelectionMove[First @ Cells[CellTags -> "$init"], After, Cell, AutoScroll -> False];
	NotebookWrite[EvaluationNotebook[], build`benchData];
];


With[{InitializeThulium = build`InitializeThulium},
	build`TemporaryNotebook = CreateDocument[
		{
			Cell[
				BoxData @ TemplateBox[{"Thulium Music Player", "\"v2.2\""}, "Title"],
				"Title", CellTags -> "$title"
			],
			Cell[BoxData @ GridBox[{
				{TemplateBox[{
					"Initialize Thulium",
					"Click to initialize the kernel.",
					InitializeThulium
				}, "TextButton"],
				TemplateBox[{
					"Initialize Parser",
					"Click to initialize the parser.",
					Unevaluated @ InitializeParser
				}, "TextButtonMonitored"],
				TemplateBox[{
					"Initialize Songs",
					"Click to initialize the songs.",
					Unevaluated @ update
				}, "TextButtonMonitored"]},
				{TemplateBox[{
					"Save WorkBench",
					"Save current notebook.",
					Unevaluated @ SaveWorkBench
				}, "TextButton"],
				TemplateBox[{
					"Load WorkBench",
					"Load your notebook.",
					Unevaluated @ LoadWorkBench
				}, "TextButton"],
				TemplateBox[{
					"Start Front End",
					"Click to run the front end.",
					Hold @ homepage
				}, "TextButton"]}
			}, ColumnSpacings -> 1], "Controls"],
			Cell[BoxData @ MakeBoxes @ Null, "Initialization", CellTags -> "$init"],
			Cell[BoxData @ TemplateBox[{
				"Please click on the \"Initialize\" buttons in sequence to get started."
			}, "Tip"], "Tip", CellTags -> "$msg:init"]
		},
		StyleDefinitions -> StyleSheet["Thulium"],
		ShowCellLabel -> False,
		ShowCellTags -> False,
		ShowCellBracket -> False,
		CellGrouping -> Manual,
		Background -> RGBColor["#FFFFFF"],
		WindowTitle -> "Thulium Music Player",
		WindowElements -> {"VerticalScrollBar"},
		WindowFrameElements -> {"CloseBox", "MinimizeBox", "ZoomBox"},
		ScrollingOptions -> {"VerticalScrollRange" -> Fit, "HorizontalScrollRange" -> Fit},
		WindowSize -> {1440, 768},
		WindowFrame -> "ModelessDialog",
		Magnification -> 2,
		Saveable -> False,
		Evaluatable -> True,
		Editable -> True
	];
	NotebookSave[build`TemporaryNotebook, localPath <> "Thulium.nb"];
	NotebookClose[build`TemporaryNotebook];
	NotebookOpen[localPath <> "Thulium.nb"];
]

