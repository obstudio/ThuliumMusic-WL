(* ::Package:: *)

StyleSheet["Core"] = Notebook[{
	
	Cell[StyleData["<Font3>"],
		TemplateBoxOptions -> {DisplayFunction -> Function[
			StyleBox[#1, FontSize -> #2, FontColor -> #3]
		]}
	],
	
	Cell[StyleData["<Tip>"],
		TemplateBoxOptions -> {DisplayFunction -> Function[
			FrameBox[
				AdjustmentBox[
					RowBox[{
						TemplateBox[{#1, #2, #3}, "<Locator>"],
						PaneBox[
							StyleBox[#4,
								LineSpacing -> {1, 2},
								LineBreakWithin -> False,
								TextAlignment -> Left
							],
							AppearanceElements -> {},
							ContentPadding -> False
						]
					}],
					BoxBaselineShift -> 0,
					BoxMargins -> {{1.2, 1.2}, {1.2, 1.2}}
				],
				Background -> #5,
				RoundingRadius -> {16, 16},
				ContentPadding -> False,
				FrameStyle -> None
			]
		]}
	],
	
	Cell[StyleData["<Locator>"],
		TemplateBoxOptions -> {DisplayFunction -> Function[
			RowBox[{
				TemplateBox[{#3[[1]]}, "Spacer1"],
				AdjustmentBox[#1,
					BoxBaselineShift -> #2
				],
				TemplateBox[{#3[[2]]}, "Spacer1"]
			}]
		]}
	]

}];


FontBox[args__] := TemplateBox[{args}, "<Font" <> ToString[Length[{args}]] <> ">"];
LocBox[args__] := TemplateBox[{args}, "<Locator>"];
