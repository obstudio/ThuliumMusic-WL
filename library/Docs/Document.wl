(* ::Package:: *)

RenderContent[rawData_List] := Block[
	{
		line, lineCount, lineNext, items,
		markCount, markLevel,
		alignment, gridData, emphasize,
		$tmpID, $tmpTag, $stack, $depth
	},
	
	$tmpTag := "tmpCell" <> ToString[$tmpID];
	$depth := Length @ $stack;
	lineCount = 1;
	
	Return @ Flatten[Last @ Reap[
		While[lineCount <= Length[rawData],
			line = rawData[[lineCount]];
			lineCount += 1;
			Which[
				
				(* Separator *)
				StringMatchQ[line, RegularExpression["(-[ .]*)\\1+- *"]],
					Sow[SpacerCell[{40, 12}, 2,
						FrameStyle -> Directive[RGBColor["#777777"], Dashing[2 StringToDashing[line]]]
					], "Cell"],
				StringMatchQ[line, RegularExpression["(=[ .]*)\\1+= *"]],
					Sow[SpacerCell[{40, 24}, 4,
						FrameStyle -> Directive[RGBColor["#999999"], Dashing[4 StringToDashing[line]]]
					], "Cell"],
					
				(* Title *)
				StringStartsQ[line, "#"],
					markCount = StringLength @ StringCases[line, RegularExpression["^#+"]][[1]];
					Sow[Cell[
						BoxData @ RenderText[StringDelete[line, RegularExpression["^#+ *| *#*$"]], "Title"],
						"Title",
						FontSize -> 72 - markCount * 12,
						TextAlignment -> If[StringEndsQ[line, RegularExpression["# *"]], Center, Left],
						CellMargins -> {{64, 64}, {24 / markCount, 60 / markCount}}
					], "Cell"],
				
				(* Section *)
				StringStartsQ[line, " "...~~"^"],
					markCount = StringLength @ StringCases[line, StartOfLine~~" "...][[1]] + 1;
					lineNext = lineCount + LengthWhile[rawData[[lineCount ;; ]], Nor[
						StringStartsQ[#, Repeated["#", markCount]],
						And[
							StringStartsQ[#, " "...~~"^"],
							StringLength @ StringCases[#, StartOfLine~~" "...][[1]] < markCount
						],
						StringMatchQ[#, RegularExpression["={3,} *"]]
					]&];
					Sow[Cell[CellGroupData[{
						Cell[
							BoxData @ RenderText[StringDelete[line, RegularExpression["^ *\\^ *| *\\^$"]], "Section"],
							"Section",
							CellMargins -> {{48, 48} + markCount * 8, {20, 40} / markCount},
							FontSize -> 40 - markCount * 6
						],
						Sequence @@ RenderContent[rawData[[lineCount ;; lineNext - 1]]]
					}, StringEndsQ[line, "^"]]], "Cell"];
					lineCount = lineNext,
				
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
				
				(* Inline list *)
				StringMatchQ[line, "+ "~~__~~" +"],
					items = StringSplit[line, RegularExpression["^\\+ +| +\\+ +| +\\+$"]];
					Sow[Cell[
						BoxData @ RowBox[Riffle[
							RenderText["InlineList"] /@ items,
							LocBox[FontBox["\[FilledSquare]", 16, RGBColor["#555555"]], -0.2, {4, 4}]
						]],
					"InlineList"], "Cell"],
				
				(* Multiline List *)
				StringStartsQ[line, RegularExpression[" *(\\+|\\d+\\.)"]],
					Sow[Cell[CellGroupData @ Flatten[Last @ Reap[
						$stack = {};
						lineCount -= 1;
						While[lineCount <= Length[rawData] && StringStartsQ[line, RegularExpression[" *(\\+|\\d+\\.)"]],
							markCount = StringLength @ StringCases[line, " "...][[1]];
							markLevel = LengthWhile[$stack, # <= markCount&];
							If[markLevel == 0 || $stack[[markLevel]] < markCount,
								AppendTo[$stack, markCount],
								$stack = Take[$stack, markLevel];
							];
							Sow[Cell[
								BoxData @ RowBox[{
									SpacerBox[4],
									RenderText[StringDelete[line, RegularExpression["^ *(\\+|\\d+\\.) *"]], "Text"]
								}],
								CellDingbat -> If[StringStartsQ[line, " "...~~"+"],
									TemplateBox[{DingBatList[[$depth]]}, "DingBat"],
									TemplateBox[{StringCases[line, DigitCharacter..~~"."][[1]]}, "Order"]
								],
								CellMargins -> {
									{If[StringStartsQ[line, " "...~~"+"], 48, 56] + $depth * 24, 48},
									{4, 8}
								}
							], "Subcell"];
							lineCount += 1;
							line = rawData[[lineCount]];
						],
					"Subcell"], 1]], "Cell"],
				
				(* Table *)
				StringMatchQ[line, RegularExpression["[<=>]\\*?(\\t+[<=>]\\*?)*"]],
					alignment = StringCases[line, {"<" -> Left, "=" -> Center, ">" -> Right}];
					emphasize = StringContainsQ["*"] /@ StringSplit[line, "\t"..];
					line = rawData[[lineCount]];
					Sow[Cell[BoxData @ GridBox[
						Flatten[Last @ Reap[
							While[lineCount <= Length[rawData] && !StringMatchQ[line, " "...],
								Block[{innerAlign = alignment, innerEmph = emphasize},
									Scan[If[StringStartsQ[line, Keys[#]] && StringEndsQ[line, Keys[#]],
										innerAlign = ConstantArray[Values[#], Length[alignment]];
										line = StringDelete[line, {StartOfString~~Keys[#], Keys[#]~~EndOfString}];
									]&, {"<<" -> Left, "==" -> Center, ">>" -> Right}];
									If[StringStartsQ[line, "**"] && StringEndsQ[line, "**"],
										innerEmph = ConstantArray[True, Length[alignment]];
										line = StringDelete[line, {StartOfString~~"**", "**"~~EndOfString}];
									];
									Sow[PadRight[MapIndexed[
										With[{id = First[#2]}, RenderItem[#1,
											Alignment -> innerAlign[[id]],
											FontWeight -> If[innerEmph[[id]], Bold, Automatic],
											Background -> If[innerEmph[[id]], "#EEEEFF", Automatic]
										]]&,
										StringSplit[line, "\t"..]
									], Length[alignment], "\[SpanFromLeft]"], "Row"];
								];
								lineCount += 1;
								line = rawData[[lineCount]];
							],
						"Row"], 1]
					], "Table"], "Cell"],
				
				(* Comment *)
				StringStartsQ[line, ">"],
					lineNext = lineCount + LengthWhile[rawData[[lineCount ;; ]], StringStartsQ[#, ">"]&];
					Sow[Cell[BoxData @ RowBox[
						Riffle[Riffle[Map[
							RenderText[StringDelete[#, RegularExpression["^> *"]], "Comment"]&,
							rawData[[lineCount - 1 ;; lineNext - 1]]
						], "\n"], SpacerBox[4], {1, -2, 3}]
					], "Comment"], "Cell"];
					lineCount = lineNext,
				
				(* Tip *)
				StringStartsQ[line, "!"],
					With[{maxMatch = StringCases[line, Repeated["!", 3]][[1]]},
						lineNext = lineCount + LengthWhile[rawData[[lineCount ;; ]], StringStartsQ[#, maxMatch]&];
						Sow[Cell[BoxData @ TemplateBox[
							{BoxSimplify[Riffle[Map[
								RenderText[StringDelete[#, StartOfString~~maxMatch~~" "...], "Tip"]&,
								rawData[[lineCount - 1 ;; lineNext - 1]]
							], "\n"]]},
							"Tip" <> ToString[StringLength[maxMatch]]
						], "Tip"], "Cell"];
					];
					lineCount = lineNext,
				
				(* CodeBlock *)
				StringStartsQ[line, "```"],
					lineNext = lineCount + LengthWhile[rawData[[lineCount ;; ]], !StringStartsQ[#, "```"]&];
					Sow[Cell[
						BoxData @ TemplateBox[{
							RowBox[Riffle[
								RenderCode /@ rawData[[lineCount ;; lineNext - 1]],
							"\n"]]
						}, "CodeBlock"],
					"CodeBlock"], "Cell"];
					lineCount = lineNext + 1,
					
				(* Text *)
				!StringMatchQ[line, RegularExpression["\\s*"]],
					lineNext = lineCount + LengthWhile[rawData[[lineCount ;; ]], Through[Nor[
						StringMatchQ[" "...],
						StringMatchQ[RegularExpression["([=\\-][ .]*)\\1+[=\\-] *"]],
						StringMatchQ[RegularExpression["[<=>]\\*?(\\t+[<=>]\\*?)*"]],
						StringStartsQ[RegularExpression[" *(\\+|\\d+\\.)"]],
						StringStartsQ[RegularExpression["\\-*\\^+"]],
						StringStartsQ["#"|"?"|"!"|">"|"```"]
					]]];
					Sow[Cell[
						BoxData[RowBox @ Riffle[
							RenderText[#, "Text"]& /@ rawData[[lineCount - 1 ;; lineNext - 1]],
						"\n"]], 
					"Text"], "Cell"];
					lineCount = lineNext;
				
			];
		],
	"Cell"], 1];
];


(* ::Input:: *)
(*RenderTMD[localPath<>"docs/Standard/GraceNote.tmd"];*)


(* API *)
RenderTMD::nfound = "Cannot find file `1`.";
RenderTMD::usage = "\
\!\(\*RowBox[{\"RenderTMD\",\"[\",RowBox[{StyleBox[\"filepath\",\"TI\"]}],\"]\"}]\)\
 generate a document notebook for \!\(\*StyleBox[\"filepath\",\"TI\"]\).";

RenderTMD[filepath_String] := Block[{rawData, output},
	
	If[FileExistsQ[filepath],
		rawData = StringSplit[Import[filepath, "Text", CharacterEncoding -> "UTF-8"], "\n"],
		Message[RenderTMD::nfound, filepath];
		Return[];
	];
	
	output = CreateDialog[
		RenderContent[rawData],
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

