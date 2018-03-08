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

RenderContent[rawData_List] := Block[
	{
		line, lineCount = 1,
		cells = {},
		markCount, markCount1, markCount2,
		tmpCells, tmpBoxes,
		tmpCell, tmpData
	},
	
	While[lineCount <= Length[rawData],
		line = rawData[[lineCount]];
		Which[
			StringMatchQ[line, RegularExpression["\\-{3,} *"]],
				AppendTo[cells, Cell[" ", "Separator1",
					CellFrame -> {{0, 0}, {0, 2}},
					CellFrameColor -> RGBColor["#777777"]
				]];
				lineCount += 1,
			StringMatchQ[line, RegularExpression["={3,} *"]],
				AppendTo[cells, Cell[" ", "Separator1",
					CellFrame -> {{0, 0}, {0, 4}},
					CellFrameColor -> RGBColor["#999999"]
				]];
				lineCount += 1,
			StringStartsQ[line, RegularExpression["#"]],
				markCount = StringLength @ StringCases[line, RegularExpression["^#+"]][[1]];
				AppendTo[cells, Cell[
					StringDelete[line, RegularExpression["^#+ *| *#*$"]],
					"Title",
					FontSize -> 72 - markCount * 12,
					TextAlignment -> If[StringEndsQ[line, RegularExpression["# *"]], Center, Left]
				]];
				lineCount += 1,
			StringStartsQ[line, RegularExpression["\\-*\\?"]],
				tmpCells = {};
				tmpBoxes = {};
				While[StringStartsQ[line, RegularExpression["-*\\?"]] && lineCount <= Length[rawData],
					markCount = StringLength @ StringCases[line, RegularExpression["^-*"]][[1]];
					If[tmpBoxes != {},
						If[markCount == 0,
							AppendTo[tmpCells, Cell[BoxData[RowBox @ tmpBoxes], "Usage"]];
							tmpBoxes = {},
							AppendTo[tmpBoxes, "\n"];
						];
					];
					tmpBoxes = Join[tmpBoxes,
						{TemplateBox[{48 + markCount * 24}, "Spacer1"]},
						RenderText @ StringDelete[line, RegularExpression["^-*\\? *"]]
					];
					lineCount += 1;
					line = rawData[[lineCount]];
				];
				If[tmpBoxes != {},
					AppendTo[tmpCells, Cell[BoxData[RowBox @ tmpBoxes], "Usage"]];
				];
				tmpBoxes = {};
				AppendTo[cells, Cell[CellGroupData[{
					Cell[" ", "Separator2", CellFrame -> {{0, 0}, {2, 0}}],
					Sequence @@ tmpCells,
					Cell[" ", "Separator2", CellFrame -> {{0, 0}, {0, 2}}]
				}]]],
			StringStartsQ[line, RegularExpression["\\-*\\^+"]],
				markCount = StringLength @ StringCases[line, RegularExpression["^\\-*\\^+"]][[1]];
				markCount1 = StringLength @ StringCases[line, RegularExpression["^\\-*"]][[1]];
				markCount2 = markCount - markCount1;
				tmpCell = Cell[
					StringDelete[line, RegularExpression["^\\-*\\^+ *"]],
					"Section",
					CellMargins -> {{48, 48} + markCount * 12, {10, 18}},
					FontSize -> 48 - markCount2 * 12
				];
				tmpData = {};
				lineCount += 1;
				While[And[
					lineCount <= Length[rawData],
					With[{line = rawData[[lineCount]]},Nor[
						StringStartsQ[line, Repeated["#", markCount]],
						And[
							StringStartsQ[line, RegularExpression["\\-*\\^+"]],
							StringLength @ StringCases[line, RegularExpression["^\\-*\\^+"]][[1]] <= markCount
						],
						StringMatchQ[line, RegularExpression["={3,} *"]]
					]]],
					AppendTo[tmpData, rawData[[lineCount]]];
					lineCount += 1;
				];
				AppendTo[cells, Cell[CellGroupData[
					Prepend[RenderContent[tmpData], tmpCell]
				]]],
			StringMatchQ[line, RegularExpression["\\s*"]],
				lineCount += 1,
			True,
				AppendTo[cells, Cell[BoxData[RowBox @ RenderText @ line], "Text"]];
				lineCount += 1;
		];
	];
	
	Return[cells];
];

RenderTMD[filepath_String] := Block[
	{
		rawData
	},
	
	If[FileExistsQ[filepath],
		rawData = StringSplit[Import[filepath,"String"],RE["\\r?\\n"]],
		Message[MakeDocument::nfound, filepath];
		Return[];
	];
	
	CreateDialog[RenderContent[rawData],
		WindowTitle -> StringSplit[filepath,"/"|"\\"][[-1]],
		WindowMargins -> {{80, Automatic}, {Automatic, 60}},
		WindowElements -> {"VerticalScrollBar"},
		Background -> RGBColor["#F7F7F7"],
		WindowSize -> {1024, 768},
		ClosingSaveDialog -> False,
		StyleDefinitions -> StyleSheet["Documemt"],
		ShowCellBracket -> False,
		CellGrouping -> Manual,
		Saveable -> False,
		Editable -> False,
		Copyable -> False,
		Enabled -> False
	];
];




(* ::Input:: *)
(*RenderTMD[localPath<>"docs/test.tmd"];*)
