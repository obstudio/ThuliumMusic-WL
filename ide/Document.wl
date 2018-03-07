(* ::Package:: *)

BeginPackage["document`"];

Options[RenderText] = {"Indent" -> 0};
RenderText[line_String, OptionsPattern[]] := Block[{output},
	output = StringCases[line, {
		"**"~~text:Except["*"]..~~"**" :> StyleBox[text, FontWeight -> Bold],
		"*"~~text:Except["*"]..~~"*" :> StyleBox[text, FontSlant -> Italic],
		"~~"~~text:Except["*"]..~~"~~" :> StyleBox[text, FontVariations -> {"StrikeThrough" -> True}],
		"_"~~text:Except["*"]..~~"_" :> StyleBox[text, FontVariations -> {"Underline" -> True}],
		(*"\n" \[RuleDelayed] Sequence["\n",TemplateBox[{OptionValue["Indent"]},"Spacer1"]],*)
		text:RegularExpression["[^_~\\*]+"] :> text
	}];
	Return[output];
];

$StyleSheet = Notebook[
	{
		Cell[StyleData["Title"],
			CellMargins -> {{72, 72}, {20, 40}},
			FontFamily -> "Source Sans Pro Semibold",
			(* FontSize: 60 *)
			(* TextAlignment: Left *)
			FontWeight -> "DemiBold",
			FontColor -> RGBColor["#111111"]
		],
		Cell[StyleData["Usage"],
			CellMargins -> {{0, 0}, {0, 0}},
			CellFrame -> {{0, 0}, {1, 1}},
			CellFrameColor -> RGBColor["#77BBFF"],
			Background -> RGBColor["#DDEEFF"],
			FontFamily -> "Cambria",
			FontSize -> 24,
			FontColor -> RGBColor["#000000"]
		],
		Cell[StyleData["Text"],
			CellMargins -> {{48, 15}, {4, 8}},
			FontFamily -> "Calibri",
			FontSize -> 24,
			FontWeight -> "Plain",
			FontColor -> RGBColor["#111111"]
		],
		Cell[StyleData["Separator"],
			(* CellFrame: {{0, 0}, {0, 2}} *)
			(* CellFrameColor: #777777 *)
			CellMargins -> {{40, 40}, {1, 1}},
			CellSize -> {Inherited, 4},
			Selectable -> False
		],
		Cell[StyleData["UsageSeparator"],
			(* CellFrame: {{0, 0}, {0, 2}} *)
			CellFrameColor -> RGBColor["#77BBFF"],
			CellMargins -> {{0, 0}, {0, 0}},
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
		line, lineCount = 1,
		cells = {},
		tmpcells, tmpboxes,
		markCount
	},
	
	If[FileExistsQ[filepath],
		rawData = Import[filepath, "List"],
		Message[MakeDocument::nfound, filepath];
		Return[];
	];
	
	While[lineCount <= Length[rawData],
		line = rawData[[lineCount]];
		Which[
			StringMatchQ[line, RegularExpression["\\-{3,} *"]],
				AppendTo[cells, Cell[" ", "Separator",
					CellFrame -> {{0, 0}, {0, 2}},
					CellFrameColor -> RGBColor["#777777"]
				]],
			StringMatchQ[line, RegularExpression["={3,} *"]],
				AppendTo[cells, Cell[" ", "Separator",
					CellFrame -> {{0, 0}, {0, 4}},
					CellFrameColor -> RGBColor["#999999"]
				]],
			StringStartsQ[line, RegularExpression["#"]],
				markCount = StringLength @ StringCases[line, RegularExpression["^#+"]][[1]];
				If[markCount <= 2,
					AppendTo[cells, Cell[
						StringDelete[line, RegularExpression["^#+ *| *#*$"]],
						"Title",
						FontSize -> If[markCount == 1, 60, 48],
						TextAlignment -> If[StringEndsQ[line, RegularExpression["# *"]], Center, Left]
					]]
				],
			StringStartsQ[line, RegularExpression["-*\\?"]],
				tmpcells = {};
				tmpboxes = {};
				While[StringStartsQ[line, RegularExpression["-*\\?"]] && lineCount <= Length[rawData],
					markCount = StringLength @ StringCases[line, RegularExpression["^-*"]][[1]];
					If[tmpboxes != {},
						If[markCount == 0,
							AppendTo[tmpcells, Cell[BoxData[RowBox @ tmpboxes], "Usage"]];
							tmpboxes = {},
							AppendTo[tmpboxes, "\n"];
						];
					];
					tmpboxes = Join[tmpboxes,
						{TemplateBox[{48 + markCount * 24}, "Spacer1"]},
						RenderText @ StringDelete[line, RegularExpression["^-*\\? *"]]
					];
					lineCount += 1;
					line = rawData[[lineCount]];
				];
				If[tmpboxes != {},
					AppendTo[tmpcells, Cell[BoxData[RowBox @ tmpboxes], "Usage"]];
				];
				tmpboxes = {};
				lineCount -= 1;
				AppendTo[cells, Cell[CellGroupData[{
					Cell[" ", "UsageSeparator", CellFrame -> {{0, 0}, {2, 0}}],
					Sequence @@ tmpcells,
					Cell[" ", "UsageSeparator", CellFrame -> {{0, 0}, {0, 2}}]
				}]]],
			!StringMatchQ[line, RegularExpression["\\s*"]],
				AppendTo[cells, Cell[BoxData[RowBox @ RenderText @ line], "Text"]];
		];
		lineCount += 1;
	];
	
	CreateDocument[cells,
		WindowTitle -> StringSplit[filepath,"/"|"\\"][[-1]],
		WindowMargins -> {{80, Automatic}, {Automatic, 60}},
		WindowElements -> {"VerticalScrollBar"},
		Background -> RGBColor["#F7F7F7"],
		WindowSize -> {1024, 768},
		System`ClosingSaveDialog -> False,
		StyleDefinitions -> $StyleSheet,
		ShowCellBracket -> False,
		Saveable -> False,
		Editable -> False,
		Copyable -> False,
		Enabled -> False
	];
];

EndPackage[];


(* ::Input:: *)
(*document`MakeDocument[localPath<>"docs/test.tmd"];*)
