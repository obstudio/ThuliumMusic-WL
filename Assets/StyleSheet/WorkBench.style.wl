(* ::Package:: *)

StyleSheet["WorkBench"] = Notebook[{
	
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
	]
	
}];
