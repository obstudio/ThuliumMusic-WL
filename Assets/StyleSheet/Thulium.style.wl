(* ::Package:: *)

StyleSheet["Thulium"] = Notebook[{
	Cell[StyleData[StyleDefinitions -> "Default.nb"]],
	
	Cell[StyleData["TitleText"],
		FontFamily -> "Source Sans Pro",
		FontSize -> 32,
		FontColor -> RGBColor["#115599"]
	],
	
	Cell[StyleData["TitleVersion"],
		TemplateBoxOptions -> {DisplayFunction -> Function[
			StyleBox[
				FormBox[#1, InputForm],
				FontFamily -> "Source Sans Pro",
				FontSize -> 24,
				FontColor -> RGBColor[0.3, 0.5, 0.8]
			]
		]}
	],
	
	Cell[StyleData["Title"],
		TextAlignment -> Center,
		ShowStringCharacters -> False,
		CellMargins -> {{40, 40}, {16, 32}},
		ShowCellBracket -> False,
		Evaluatable -> False,
		Editable -> False,
		Deletable -> False
	],
	
	Cell[StyleData["Controls"],
		CellMargins -> {{24, 24}, {8, 8}},
		TextAlignment -> Center,
		ShowCellBracket -> False,
		Evaluatable -> False,
		Editable -> False,
		Deletable -> False,
		Deployed -> True
	],
	
	Cell[StyleData["Tip"],
		CellMargins -> {{60, 60}, {8, 4}},
		Deployed -> True,
		Copyable -> False,
		ShowCellBracket -> False,
		TextAlignment -> Center,
		ShowCellLabel -> False,
		TemplateBoxOptions -> {DisplayFunction -> Function[
			FrameBox[
				AdjustmentBox[
					RowBox[{
						StyleBox["\[LightBulb]", FontSize -> 18],
						TemplateBox[{4}, "Spacer1"],
						StyleBox[#1, FontFamily -> "Calibri", FontSize -> 16]
					}],
					BoxBaselineShift -> 0,
					BoxMargins -> {{2, 2}, {2, 2}}
				],
				Background -> RGBColor[1, 0.96, 0.98],
				RoundingRadius -> {8, 8},
				ContentPadding -> True,
				FrameStyle -> None
			]
		]}
	],
	
	Cell[StyleData["Initialization"],
		FontSize -> 1,
		FontColor -> RGBColor[0, 0, 0, 0],
		CellSize -> {Inherited, 1},
		CellMargins -> {{24, 24}, {8, 8}},
		CellElementSpacings -> {"CellMinHeight" -> 1},
		CellFrame -> {{0, 0}, {0, 2}},
		CellFrameColor -> RGBColor["#999999"],
		CellFrameMargins -> 0,
		Background -> Inherited,
		ShowCellBracket -> False,
		ShowCellLabel -> False,
		Evaluatable -> True,
		CellGroupingRules -> "InputGrouping"
	],
	
	Cell[StyleData["TextButtonContent"],
		FontFamily -> "Sitka Text",
		FontSize -> 15
	],
	
	Cell[StyleData["TextButtonTooltip"],
		TemplateBoxOptions -> {DisplayFunction -> Function[
			RowBox[{
				TemplateBox[{1}, "Spacer1"],
				StyleBox[#1,
					FontFamily -> "Calibri",
					FontSize -> 24
				],
				TemplateBox[{1}, "Spacer1"]
			}]
		]}
	],
	
	Cell[StyleData["TextButtonDisplay"],
		TemplateBoxOptions -> {DisplayFunction -> Function[
			FrameBox[
				RowBox[{
					TemplateBox[{4}, "Spacer1"],
					AdjustmentBox[
						StyleBox[#1, FontColor -> #2],
						BoxBaselineShift -> 0.2
					],
					TemplateBox[{4}, "Spacer1"]
				}],
				Background -> #3,
				ImageMargins -> {{1, 1}, {0, 0}},
				ImageSize -> {Automatic, 32},
				BoxFrame -> {{0, 0}, {0, 0}},
				RoundingRadius -> {8, 8},
				ContentPadding -> True,
				BaselinePosition -> 1
			]
		]}
	],
	
	Cell[StyleData["TextButton"],
		TemplateBoxOptions -> {DisplayFunction -> Function[
			PaneSelectorBox[{
				True -> TooltipBox[
					TagBox[
						TagBox[
							PaneSelectorBox[{
								True -> TemplateBox[{
									StyleBox[#1, "TextButtonContent"],
									RGBColor[0, 0, 0],
									RGBColor[0.5, 0.8, 1]
								}, "TextButtonDisplay"],
								False -> TemplateBox[{
									StyleBox[#1, "TextButtonContent"],
									RGBColor[0.08, 0.04, 0],
									RGBColor[0.8, 0.9, 1]
								}, "TextButtonDisplay"]
							}, Dynamic @ CurrentValue["MouseButtonTest"]],
						EventHandlerTag @ {"MouseClicked" :> ReleaseHold @ #3}],
					MouseAppearanceTag @ "LinkHand"],
					TemplateBox[{#2}, "TextButtonTooltip"],
					TooltipDelay -> 0.2,
					TooltipStyle -> {
						CellFrameColor -> RGBColor[0.7, 0.7, 0.6, 0.5],
						Background -> RGBColor[1, 1, 0.9, 0.7]
					}
				],
				False -> TemplateBox[{
					StyleBox[#1, "TextButtonContent"],
					RGBColor[0.2, 0.1, 0],
					RGBColor[0.92, 0.96, 1]
				}, "TextButtonDisplay"]
			}, Dynamic @ CurrentValue["MouseOver"]]
		]}
	],
	
	Cell[StyleData["TextButtonMonitored"],
		TemplateBoxOptions -> {DisplayFunction -> Function[
			TemplateBox[{#1, #2, Hold[
				NotebookLocate["$init"];
				NotebookWrite[EvaluationNotebook[], Cell[
					BoxData @ MakeBoxes @ Evaluate @ #3,
					"Initialization",
					CellTags -> "$init"
				], All];
				SelectionEvaluate[EvaluationNotebook[]];
				NotebookLocate["$title"];
			]}, "TextButton"]
		]}
	],
	
	Cell[StyleData["PrintTemporary", StyleDefinitions -> "PrintTemporary"],
		FontFamily -> "Calibri",
		FontSize -> 16,
		CellMargins -> {{60, 60}, {8, 4}},
		Deployed -> True,
		Copyable -> False,
		ShowCellBracket -> False,
		TextAlignment -> Center
	],
	
	(*Cell[StyleData["Music"],
		MenuSortingValue -> 500,
		MenuCommandKey -> "M"
	],*)
	
	Cell[StyleData["AdapterSong"],
		FontColor -> RGBColor[0.5, 0.4, 0.3],
		FontSize -> 12,
		FontFamily -> "Cambria",
		FontWeight -> Bold
	],
	
	Cell[StyleData["AdapterTag"],
		FontColor -> RGBColor[0.6, 0.5, 0.4],
		FontSize -> 20,
		FontFamily -> "Calibri"
	],
	
	Cell[StyleData["AdapterMeta"],
		FontColor -> RGBColor[0.2, 0.1, 0.3],
		FontSize -> 20,
		FontFamily -> "Calibri"
	],
	
	Cell[StyleData["Input", StyleDefinitions -> "Input"],
		NumberMarks -> False,
		StyleKeyMapping -> {">" -> "Music"},
		ContextMenu -> {
			MenuItem["Cut", "Cut"],
			MenuItem["Copy", "Copy"],
			MenuItem["Paste", FrontEnd`Paste[After]],
			Delimiter,
			MenuItem["Evaluate Cell", "EvaluateCells"]
		}
	],
	
	Sequence @@ (Cell[StyleData[#, StyleDefinitions -> #],
		MenuSortingValue -> 0,
		MenuCommandKey -> None
	]&/@{
		"Title", "Subtitle", "Subsubtitle",
		"Chapter", "Subchapter", "Output",
		"Section", "Subsection", "Subsubsection",
		"Item", "Subitem", "Subsubitem",
		"ItemNumbered", "SubitemNumbered", "SubsubitemNumbered",
		"ItemParagraph", "SubitemParagraph", "SubsubitemParagraph",
		"InlineFormula", "DisplayFormula", "DisplayFormulaNumbered",
		"Code", "CodeText", "ExternalLanguage", "Program"
	})
	
}];


(* ::Input:: *)
(*CurrentValue[{StyleDefinitions, "FractionBox"}]*)
