(* ::Package:: *)

StyleSheet["Thulium"] = Notebook[{
	Cell[StyleData[StyleDefinitions -> "Default.nb"]],
	
	Cell[StyleData["Thulium-Title"],
		FontFamily -> "Source Sans Pro",
		FontSize -> 36,
		FontColor -> RGBColor["#335599"],
		LineSpacing -> {1, 4},
		LanguageCategory -> "NaturalLanguage",
		CellMargins -> {{40, Inherited}, {16, 32}},
		ShowCellBracket -> False,
		Evaluatable -> False,
		Editable -> False,
		Deletable -> False
	],
	
	Cell[StyleData["Thulium-Controls"],
		CellMargins -> {{48, 48}, {8, 8}},
		TextAlignment -> Center,
		ShowCellBracket -> False,
		Evaluatable -> False,
		Editable -> False,
		Deletable -> False,
		Deployed -> True
	],
	
	Cell[StyleData["Thulium-TextButton-Content"],
		FontFamily -> "Book Antiqua",
		FontSize -> 16
	],
	
	Cell[StyleData["Thulium-TextButton-Tooltip"],
		FontFamily -> "Calibri",
		FontSize -> 20
	],
	
	Cell[StyleData["Thulium-Initialization"],
		FontSize -> 1,
		FontColor -> RGBColor[0, 0, 0, 0],
		CellSize -> {Inherited, 1},
		CellMargins -> {{24, 24}, {16, 16}},
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
	
	Cell[StyleData["Thulium-TextButton-Display"],
		TemplateBoxOptions -> {DisplayFunction -> Function[
			FrameBox[
				RowBox[{
					TemplateBox[{4}, "Spacer1"],
					AdjustmentBox[
						StyleBox[#1, FontColor -> #2],
						BoxBaselineShift -> 0.5
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
	
	Cell[StyleData["Thulium-TextButton"],
		TemplateBoxOptions -> {DisplayFunction -> Function[
			PaneSelectorBox[{
				True -> TooltipBox[
					TagBox[
						TagBox[
							PaneSelectorBox[{
								True -> TemplateBox[{
									StyleBox[#1, "Thulium-TextButton-Content"],
									RGBColor[0, 0, 0],
									RGBColor[0.3, 0.7, 1]
								}, "Thulium-TextButton-Display"],
								False -> TemplateBox[{
									StyleBox[#1, "Thulium-TextButton-Content"],
									RGBColor[0.08, 0.04, 0],
									RGBColor[0.8, 0.9, 1]
								}, "Thulium-TextButton-Display"]
							}, Dynamic @ CurrentValue["MouseButtonTest"]],
						EventHandlerTag @ {"MouseClicked" :> ReleaseHold @ #3}],
					MouseAppearanceTag @ "LinkHand"],
					StyleBox[#2, "Thulium-TextButton-Tooltip"],
					TooltipDelay -> 0.2,
					TooltipStyle -> {
						CellFrameColor -> RGBColor[0.7, 0.7, 0.6, 0.5],
						Background -> RGBColor[1, 1, 0.9, 0.7]
					}
				],
				False -> TemplateBox[{
					StyleBox[#1, "Thulium-TextButton-Content"],
					RGBColor[0.2, 0.1, 0],
					RGBColor[0.92, 0.96, 1]
				}, "Thulium-TextButton-Display"]
			}, Dynamic @ CurrentValue["MouseOver"]]
		]}
	],
	
	Cell[StyleData["Thulium-TextButton-Monitored"],
		TemplateBoxOptions -> {DisplayFunction -> Function[
			TemplateBox[{#1, #2, Hold[
				NotebookLocate["$init"];
				NotebookWrite[EvaluationNotebook[], Cell[
					BoxData @ MakeBoxes @ Evaluate @ #3,
					"Thulium-Initialization",
					CellTags -> "$init"
				], All];
				SelectionEvaluate[EvaluationNotebook[]];
				NotebookLocate["$title"];
			]}, "Thulium-TextButton"]
		]}
	],
	
	Cell[StyleData["Thulium-Monitor"],
		FontFamily -> "Calibri",
		FontSize -> 20,
		TemplateBoxOptions -> {DisplayFunction -> Function[
			FrameBox[#1,
				Background -> RGBColor[0.96, 0.96, 0.96],
				BoxFrame -> {{0, 0}, {0, 0}},
				RoundingRadius -> {8, 8},
				ContentPadding -> True
			]
		]}
	]
	
}];


(* ::Input:: *)
(*CurrentValue[{StyleDefinitions, "PrintTemporary"}]*)
