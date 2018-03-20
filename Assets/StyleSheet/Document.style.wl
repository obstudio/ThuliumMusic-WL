(* ::Package:: *)

StyleSheet["Documemt"] = Notebook[{
	
	Cell[StyleData["Title"],
		FontFamily -> "Source Sans Pro Semibold",
		FontWeight -> "DemiBold",
		CellMargins -> {{64, 64}, {30, 60}},
		FontColor -> RGBColor["#111111"],
		FontSize -> 60,
		TextAlignment -> Left
	],
	Cell[StyleData["Title-chs"],
		FontFamily -> "\:9ed1\:4f53",
		FontWeight -> Plain
	],
	
	Cell[StyleData["Usage"],
		FontFamily -> "Cambria",
		FontSize -> 22,
		CellMargins -> {{0, 0}, {0, 0}},
		CellFrame -> {{0, 0}, {0, 1}},
		CellFrameColor -> RGBColor["#99CCFF"],
		Background -> RGBColor["#EFF7FF"],
		LineSpacing -> {1.5, 0}
	],
	Cell[StyleData["Usage-Illust"],
		FontFamily -> "Cambria",
		FontSize -> 24
	],
	Cell[StyleData["Usage-chs"],
		FontFamily -> "\:534e\:6587\:7ec6\:9ed1",
		FontSize -> 22
	],
	
	Cell[StyleData["Text"],
		FontFamily -> "Calibri",
		FontSize -> 24
	],
	Cell[StyleData["Text-chs"],
		FontFamily -> "\:534e\:6587\:7ec6\:9ed1",
		FontSize -> 24
	],
	
	Cell[StyleData["Code"],
		FontFamily -> "Consolas",
		FontSize -> 20,
		FontColor -> RGBColor["#000000"]
	],
	Cell[StyleData["Code-chs"],
		FontFamily -> "\:534e\:6587\:4eff\:5b8b",
		FontSize -> 20,
		FontColor -> RGBColor["#000000"]
	],
	
	Cell[StyleData["Comment"],
		FontFamily -> "Calibri",
		FontSize -> 24,
		FontColor -> RGBColor["#777777"]
	],
	Cell[StyleData["Comment-chs"],
		FontFamily -> "\:534e\:6587\:7ec6\:9ed1",
		FontSize -> 24,
		FontColor -> RGBColor["#777777"]
	],
	
	Cell[StyleData["Section"],
		FontFamily -> "Corbel",
		FontSize -> 32,
		FontColor -> RGBColor["#772200"]
	],
	Cell[StyleData["Section-chs"],
		FontFamily -> "\:5e7c\:5706",
		FontSize -> 32,
		FontColor -> RGBColor["#772200"]
	],
	
	Cell[StyleData["DingBat"],
		TemplateBoxOptions -> {DisplayFunction -> Function[
			AdjustmentBox[
				StyleBox[#,
					FontFamily -> "Source Sans Pro",
					FontSize -> 20,
					FontColor -> RGBColor["#777777"]
				],
				BoxBaselineShift -> -0.3
			]
		]}
	],
	
	Cell[StyleData["CodeBox"],
		TemplateBoxOptions -> {DisplayFunction -> Function[
			FrameBox[
				RowBox[{
					TemplateBox[{1}, "Spacer1"],
					#,
					TemplateBox[{1}, "Spacer1"]
				}],
				Background -> RGBColor[0.92, 0.92, 0.92],
				ImageMargins -> {{1, 1}, {0, 0}},
				ImageSize -> {Automatic, 32},
				BoxFrame -> {{0, 0}, {0, 0}},
				RoundingRadius -> {8, 8},
				ContentPadding -> True,
				BaselinePosition -> 1
			]
		]}
	],
	
	Cell[StyleData["CodeBlock"],
		TemplateBoxOptions -> {DisplayFunction -> Function[
			GraphicsBox[
				{
					GraphicsGroupBox[{
						RGBColor[0.92, 0.92, 0.92],
						RectangleBox[{0, 0}, {#2, #3}, RoundingRadius -> 16]
					}, ContentSelectable -> False],
					InsetBox[AdjustmentBox[#1,
						LineSpacing -> {1, 4},
						BoxBaselineShift -> 0.5
					], {20, Center}, {Left, Center}, ContentSelectable -> True, Alignment->Left]
				},
				PlotRange -> {{0, #2}, {0, #3}},
				ContentSelectable -> True,
				ImageSize -> {#2, Automatic}
			]
		]}
	],
	
	Cell[StyleData["Miniplayer"],
		TemplateBoxOptions -> {DisplayFunction -> Function[
			GraphicsBox[
				{
					RGBColor[0.92, 0.92, 0.92],
					RectangleBox[{0, 0}, {600, 48}, RoundingRadius -> 16],
					InsetBox[GraphicsBox[
						{
							RGBColor[0.96, 0.96, 0.96],
							RectangleBox[{0, 0}, {400, 32}, RoundingRadius -> 8]
						},
						ImageSize -> 400,
						PlotRange -> {{0, 400}, {0, 32}}
					], {8, Center}, {Left, Center}],
					InsetBox[AdjustmentBox[
						StyleBox[#1,
							FontSize -> 20,
							FontColor -> RGBColor[0, 0, 0]
						],
						BoxBaselineShift -> 0.1
					], {20, Center}, {Left, Center}]
				},
				ContentSelectable -> False,
				ImageSize -> 600,
				PlotRange -> {{0, 600}, {0, 48}}
			]
		]}
	]
	
}];


(* ::Input:: *)
(*RenderTMD[localPath<>"docs/test.tmd"];*)


(* ::Input:: *)
(*CurrentValue[{StyleDefinitions,"Usage"}]*)
