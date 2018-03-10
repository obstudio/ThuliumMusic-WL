(* ::Package:: *)

(* Render Syntax *)
RenderText[line_String, style_String] := Block[{output},
	output = StringCases[line, {
		"(("~~text:Except[")"]..~~"))" :>
			BoxApply[RenderText[text, style], Smaller, FontColor -> RGBColor["#555555"]],
		"**"~~text:Except["*"]..~~"**" :>
			BoxApply[RenderText[text, style], FontWeight -> Bold],
		"*"~~text:Except["*"]..~~"*" :>
			BoxApply[RenderText[text, style], FontSlant -> Italic],
		"~~"~~text:Except["~"]..~~"~~" :>
			BoxApply[RenderText[text, style], FontVariations -> {"StrikeThrough" -> True}],
		"_"~~text:Except["_"]..~~"_" :>
			BoxApply[RenderText[text, style], FontVariations -> {"Underline" -> True}],
		text_ :> RowBox @ StringCases[text, {
			text1__?(PrintableASCIIQ) :> StyleBox[text1, style],
			text1__?(Not@*PrintableASCIIQ) :> StyleBox[text1, style <> "-chs"]
		}]
	}];
	Return[BoxSimplify @ RowBox @ output];
];

RenderContent[rawData_List, id_Integer] := Block[
	{
		line, lineCount, lineNext,
		markCount, markCount1,
		$tmpID, $tmpTag, tmData
	},
	
	$tmpTag := "tmpCell" <> ToString[$tmpID];
	lineCount = 1;
	
	Return @ Flatten[Last @ Reap[
		While[lineCount <= Length[rawData],
			line = rawData[[lineCount]];
			lineCount += 1;
			Which[
				
				(* Separator *)
				StringMatchQ[line, RegularExpression["\\-{3,} *"]],
					Sow[SpacerCell[{40, 1}, 2, CellFrameColor -> RGBColor["#777777"]], "Cell"],
				StringMatchQ[line, RegularExpression["={3,} *"]],
					Sow[SpacerCell[{40, 2}, 4, CellFrameColor -> RGBColor["#999999"]], "Cell"],
					
				(* Title *)
				StringStartsQ[line, RegularExpression["#"]],
					markCount = StringLength @ StringCases[line, RegularExpression["^#+"]][[1]];
					Sow[Cell[
						BoxData @ RenderText[StringDelete[line, RegularExpression["^#+ *| *#*$"]], "Title"],
						CellMargins -> {{72, 72}, {20, 40}},
						FontSize -> 72 - markCount * 12,
						FontColor -> RGBColor["#111111"],
						TextAlignment -> If[StringEndsQ[line, RegularExpression["# *"]], Center, Left]
					], "Cell"],
				
				(* Usage *)
				StringStartsQ[line, "-"...~~"?"],
					Sow[Cell[CellGroupData[{
						SpacerCell[-2, CellFrameColor -> RGBColor["#77BBFF"]],
						Sequence @@ (Cell[BoxData[RowBox[#]],
							CellMargins -> {{0, 0}, {0, 0}},
							CellFrame -> {{0, 0}, {1, 1}},
							CellFrameColor -> RGBColor["#77BBFF"],
							Background -> RGBColor["#DDEEFF"],
							LineSpacing -> {1.5, 0}
						]&/@ Last @ Reap[
							$tmpID = 0;
							lineCount -= 1;
							While[lineCount <= Length[rawData] && StringStartsQ[line, RegularExpression["\\-*\\?"]],
								markCount = StringLength @ StringCases[line, RegularExpression["^\\-*"]][[1]];
								If[markCount == 0, $tmpID += 1, Sow["\n", $tmpTag]];
								Sow[TemplateBox[{48 + markCount * 24}, "Spacer1"], $tmpTag];
								Sow[RenderText[StringDelete[line, RegularExpression["^-*\\? *"]], "Usage"], $tmpTag];
								lineCount += 1;
								line = rawData[[lineCount]];
							];
						]),
						SpacerCell[2, CellFrameColor -> RGBColor["#77BBFF"]]
					}]], "Cell"],
				
				(* Section *)
				StringStartsQ[line, RegularExpression["\\-*\\^+"]],
					markCount = StringLength @ StringCases[line, RegularExpression["^\\-*\\^+"]][[1]];
					markCount1 = StringLength @ StringCases[line, RegularExpression["^\\-*"]][[1]];
					lineNext = lineCount + LengthWhile[rawData[[lineCount ;; ]], Nor[
						StringStartsQ[#, Repeated["#", markCount]],
						And[
							StringStartsQ[#, RegularExpression["\\-*\\^+"]],
							StringLength @ StringCases[#, RegularExpression["^\\-*\\^+"]][[1]] <= markCount
						],
						StringMatchQ[#, RegularExpression["={3,} *"]]
					]&];
					Sow[Cell[CellGroupData[{
						Cell[
							BoxData @ RenderText[StringDelete[line, RegularExpression["^\\-*\\^+ *"]], "Section"],
							ShowGroupOpener -> True,
							CellMargins -> {{48, 48} + markCount * 12, {10, 18}},
							FontSize -> 48 - (markCount - markCount1) * 6
						],
						Sequence @@ RenderContent[rawData[[lineCount ;; lineNext - 1]], id]
					}]], "Cell"];
					lineCount = lineNext,
				
				(* Unordered List *)
				StringStartsQ[line, "-"...~~"+"],
					markCount = StringLength @ StringCases[line, RegularExpression["^\\-*"]][[1]];
					lineNext = lineCount + LengthWhile[rawData[[lineCount ;; ]], And[
						StringStartsQ[#, "-"...~~"+"],
						StringLength @ StringCases[#, StartOfLine~~"-"...][[1]] > markCount
					]&];
					Sow[Cell[CellGroupData[{
						Cell[
							BoxData @ RowBox[{
								TemplateBox[{4}, "Spacer1"],
								RenderText[StringDelete[line, RegularExpression["^\\-*\\+ *"]], "Text"]
							}],
							CellDingbat -> TemplateBox[{DingBatList[[markCount + 1]]}, "DingBat"],
							CellMargins -> {{72 + markCount * 16, 48}, {4, 4}}
						],
						Sequence @@ RenderContent[rawData[[lineCount ;; lineNext - 1]], id]
					}]], "Cell"];
					lineCount = lineNext,
				
				(* Comment *)
				StringStartsQ[line, ">"~~Except[">"]],
					lineNext = lineCount + LengthWhile[rawData[[lineCount ;; ]], StringStartsQ[#, ">"]&];
					Sow[Cell[
						BoxData @ RowBox[Riffle[Riffle[
							Map[
								RenderText[StringDelete[#, RegularExpression["^> *"]], "Comment"]&,
								rawData[[lineCount - 1 ;; lineNext - 1]]
							],
							TemplateBox[{4}, "Spacer1"], {1, -2, 2}
						], "\n", 3]],
						LineSpacing -> {1, 12},
						CellMargins -> {{54, 15}, {12, 12}},
						CellFrame -> {{8, 0}, {0, 0}},
						CellFrameColor -> RGBColor["#CCCCCC"]
					], "Cell"];
					lineCount = lineNext,
					
				(* CodeBlock *)
				StringStartsQ[line, "```"],
					lineNext = lineCount + LengthWhile[rawData[[lineCount ;; ]], !StringStartsQ[#, "```"]&];
					Sow[Cell[
						BoxData @ TemplateBox[{
							RowBox[Riffle[RenderText[#, "Code"]&/@ rawData[[lineCount ;; lineNext - 1]], "\n"]],
							Dynamic @ CurrentValue[$GeneratedList[[id]], WindowSize][[1]] - 120,
							(lineNext - lineCount) * 24 + 32
						}, "CodeBlock"],
						CellMargins -> {{48, 48}, {18, 18}}
					], "Cell"];
					lineCount = lineNext + 1,
					
				(* Text *)
				!StringMatchQ[line, RegularExpression["\\s*"]],
					Sow[Cell[
						BoxData[RenderText[line, "Text"]], 
						CellMargins -> {{48, 15}, {6, 6}}
					], "Cell"];
				
			];
		],
	"Cell"], 1];
];

(* API *)
RenderTMD::nfound = "Cannot find file `1`.";
RenderTMD::usage = "\
\!\(\*RowBox[{\"RenderTMD\",\"[\",RowBox[{StyleBox[\"filepath\",\"TI\"]}],\"]\"}]\)
generate a document notebook for \!\(\*StyleBox[\"filepath\",\"TI\"]\).";

RenderTMD[filepath_String] := Block[{rawData},
	
	If[FileExistsQ[filepath],
		rawData = StringSplit[Import[filepath, "Text", CharacterEncoding -> "UTF-8"], "\n"],
		Message[RenderTMD::nfound, filepath];
		Return[];
	];
	
	AppendTo[$GeneratedList, CreateDialog[
		RenderContent[rawData, Length[$GeneratedList] + 1],
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
		Copyable -> False
	]];
];



(* ::Input:: *)
(*RenderTMD[localPath<>"docs/test.tmd"];*)
