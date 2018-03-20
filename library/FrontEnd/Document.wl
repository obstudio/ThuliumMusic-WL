(* ::Package:: *)

RenderText[line_String, style_String] := Block[{output},
	output = StringCases[line, {
		"`"~~text:RegularExpression["([^`\\\\]|\\\\.)+"]~~"`" :>
			TemplateBox[{StyleBox[text, "Code"]}, "CodeBox"],
		"(("~~text:RegularExpression["([^\\)\\\\]|\\\\.)+"]~~"))" :>
			BoxApply[RenderText[text, style], FontSize -> Inherited * 0.7, FontColor -> RGBColor["#555555"]],
		"**"~~text:RegularExpression["([^\\*\\\\]|\\\\.)+"]~~"**" :>
			BoxApply[RenderText[text, style], FontWeight -> Bold],
		"*"~~text:RegularExpression["([^\\*\\\\]|\\\\.)+"]~~"*" :>
			BoxApply[RenderText[text, style], FontSlant -> Italic],
		"--"~~text:RegularExpression["([^\\-\\\\]|\\\\.)+"]~~"--" :>
			BoxApply[RenderText[text, style], FontVariations -> {"StrikeThrough" -> True}],
		"__"~~text:RegularExpression["([^_\\\\]|\\\\.)+"]~~"__" :>
			BoxApply[RenderText[text, style], FontVariations -> {"Underline" -> True}],
		"\\"~~text_ :> StyleBox[text, style],
		text_ :> RenderLanguage[text, style]
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
				StringMatchQ[line, RegularExpression["(-[ .]*)\\1+- *"]],
					Sow[SpacerCell[{40, 1}, 2,
						FrameStyle -> Directive[RGBColor["#777777"], Dashing[2 StringToDashing[line]]]
					], "Cell"],
				StringMatchQ[line, RegularExpression["(=[ .]*)\\1+= *"]],
					Sow[SpacerCell[{40, 2}, 4,
						FrameStyle -> Directive[RGBColor["#999999"], Dashing[4 StringToDashing[line]]]
					], "Cell"],
					
				(* Title *)
				StringStartsQ[line, "#"],
					markCount = StringLength @ StringCases[line, RegularExpression["^#+"]][[1]];
					Sow[Cell[
						BoxData @ RenderText[StringDelete[line, RegularExpression["^#+ *| *#*$"]], "Title"],
						"Title",
						FontSize -> 72 - markCount * 12,
						TextAlignment -> If[StringEndsQ[line, RegularExpression["# *"]], Center, Left]
					], "Cell"],
				
				(* Usage *)
				StringStartsQ[line, "?"],
					Sow[Cell[CellGroupData[{
						SpacerCell[-1, FrameStyle -> RGBColor["#99CCFF"]],
						Sequence @@ (Cell[BoxData[RowBox[#]], "Usage"]&/@ Last @ Reap[
							$tmpID = 0;
							lineCount -= 1;
							While[lineCount <= Length[rawData] && StringStartsQ[line, RegularExpression["[ \\?]"]],
								If[StringStartsQ[line, "?"],
									$tmpID += 1;
									ListSow[{SpacerBox[48],
										RenderText[StringDelete[line, StartOfString~~"?"], "Usage-Illust"]
									}, $tmpTag],
									ListSow[{"\n", SpacerBox[72],
										RenderText[StringDelete[line, StartOfString~~" "..], "Usage"]
									}, $tmpTag];
								];
								lineCount += 1;
								line = rawData[[lineCount]];
							];
						]),
						SpacerCell[2, FrameStyle -> RGBColor["#99CCFF"]]
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
							CellMargins -> {{48, 48} + markCount * 18, {20, 40} / markCount},
							FontSize -> 40 - (markCount - markCount1) * 6
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
							RowBox[Riffle[
								StyleBox[FormBox["\""<>#<>"\"", InputForm], "Code"]&/@ rawData[[lineCount ;; lineNext - 1]],
							"\n"]],
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


(* ::Input:: *)
(*RenderTMD[localPath<>"docs/Standard/GraceNote.tmd"];*)


(* API *)
RenderTMD::nfound = "Cannot find file `1`.";
RenderTMD::usage = "\
\!\(\*RowBox[{\"RenderTMD\",\"[\",RowBox[{StyleBox[\"filepath\",\"TI\"]}],\"]\"}]\)
generate a document notebook for \!\(\*StyleBox[\"filepath\",\"TI\"]\).";

RenderTMD[filepath_String] := Block[{rawData, output},
	
	If[FileExistsQ[filepath],
		rawData = StringSplit[Import[filepath, "Text", CharacterEncoding -> "UTF-8"], "\n"],
		Message[RenderTMD::nfound, filepath];
		Return[];
	];
	
	output = CreateDialog[
		RenderContent[rawData, Length[$GeneratedList] + 1],
		WindowTitle -> StringSplit[filepath,"/"|"\\"][[-1]],
		WindowMargins -> {{80, Automatic}, {Automatic, 60}},
		WindowElements -> {"VerticalScrollBar"},
		Background -> RGBColor["#FBFBFB"],
		WindowSize -> {1024, 768},
		StyleDefinitions -> StyleSheet["Documemt"],
		ShowCellBracket -> False,
		CellGrouping -> Manual,
		Saveable -> False,
		Editable -> False,
		Copyable -> False
	];
	
	AppendTo[$GeneratedList, output];
	Return[output];
];

