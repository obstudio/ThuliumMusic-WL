(* ::Package:: *)

RenderText[line_String] := Block[{output},
	output = StringCases[line, {
		"**"~~text:Except["*"]..~~"**" :> StyleBox[text, FontWeight -> Bold],
		"*"~~text:Except["*"]..~~"*" :> StyleBox[text, FontSlant -> Italic],
		"~~"~~text:Except["*"]..~~"~~" :> StyleBox[text, FontVariations -> {"StrikeThrough" -> True}],
		"_"~~text:Except["*"]..~~"_" :> StyleBox[text, FontVariations -> {"Underline" -> True}],
		text:RegularExpression["[^_~\\*]+"] :> text
	}];
	Return[output];
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
				AppendTo[cells, Cell[" ", "Separator1",
					CellFrame -> {{0, 0}, {0, 2}},
					CellFrameColor -> RGBColor["#777777"]
				]],
			StringMatchQ[line, RegularExpression["={3,} *"]],
				AppendTo[cells, Cell[" ", "Separator1",
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
					Cell[" ", "Separator2", CellFrame -> {{0, 0}, {2, 0}}],
					Sequence @@ tmpcells,
					Cell[" ", "Separator2", CellFrame -> {{0, 0}, {0, 2}}]
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
		ClosingSaveDialog -> False,
		StyleDefinitions -> StyleSheet["Documemt"],
		ShowCellBracket -> False,
		Saveable -> False,
		Editable -> False,
		Copyable -> False,
		Enabled -> False
	];
];



(* ::Input:: *)
(*MakeDocument[localPath<>"docs/test.tmd"];*)
