(* ::Package:: *)

RenderText[style_String] := Function[RenderText[#1, style]];
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


Options[RenderItem] = {FontWeight -> Automatic, Alignment -> Automatic, Background -> None};
RenderItem[spec:OptionsPattern[]] := Function[RenderItem[#1, spec]];
RenderItem[string_String, OptionsPattern[]] := Block[
	{alignment, text},
	If[string == "-", Return["\[SpanFromLeft]"]];
	If[string == "|", Return["\[SpanFromAbove]"]];
	If[string == "+", Return["\[SpanFromBoth]"]];
	If[StringStartsQ[string, "<"|"="|">"],
		text = StringDelete[string, StartOfString~~("<"|"="|">")];
		alignment = Switch[StringPart[string, 1], "<", Left, "=", Center, ">", Right],
		text = string;
		alignment = OptionValue[Alignment]
	];
	Return @ ItemBox[
		BoxApply[RenderText[text, "Table"], FontWeight -> OptionValue[FontWeight]],
		Alignment -> alignment,
		Background -> RGBColor @ OptionValue[Background]
	];
];


RenderCode[code_String] := Block[
	{output},
	output = StyleBox[FormBox["\"" <> code <> "\"", InputForm], "Code"];
	Return @ output;
];


RenderContent[rawData_List, id_Integer] := Block[
	{
		line, lineCount, lineNext, items,
		markCount, markCount1, markLevel,
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
				
				(* Inline list *)
				StringMatchQ[line, "+ "~~__~~" +"],
					items = StringSplit[line, RegularExpression["^\\+ +| +\\+ +| +\\+$"]];
					Sow[Cell[
						BoxData @ RowBox[Riffle[
							RenderText["InlineList"] /@ items,
							TemplateBox[{"\[FilledSquare]"}, "Separator"]
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
								CellMargins -> {{48 + $depth * 16, 48}, {4, 4}}
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
										RenderItem[#1,
											Alignment -> innerAlign[[First[#2]]],
											FontWeight -> If[innerEmph[[First[#2]]], Bold, Automatic],
											Background -> If[innerEmph[[First[#2]]], "#EEEEFF", Automatic]
										]&,
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
					lineNext = lineCount + LengthWhile[rawData[[lineCount ;; ]], Nor[
						StringMatchQ[#, " "...],
						StringMatchQ[#, RegularExpression["([=\\-][ .]*)\\1+[=\\-] *"]],
						StringMatchQ[#, RegularExpression["[<=>]\\*?(\\t+[<=>]\\*?)*"]],
						StringStartsQ[#, RegularExpression[" *(\\+|\\d+\\.)"]],
						StringStartsQ[#, RegularExpression["\\-*\\^+"]],
						StringStartsQ[#, "#"|"?"|">"|"```"]
					]&];
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

