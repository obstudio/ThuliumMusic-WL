(* ::Package:: *)

$Width = Dynamic[Divide[
	AbsoluteCurrentValue[EvaluationNotebook[], WindowSize][[1]],
	AbsoluteCurrentValue[EvaluationNotebook[], Magnification]
]];

StyleSheet["Documemt"] = With[{$Width = $Width}, 
Notebook[{
	
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
		FontSize -> 24,
		CellMargins -> {{48, 15}, {12, 6}}
	],
	Cell[StyleData["Text-chs"],
		FontFamily -> "\:7b49\:7ebf",
		FontSize -> 24
	],
	
	Cell[StyleData["Code"],
		FontFamily -> "Consolas",
		FontSize -> 20,
		FontWeight -> Bold,
		FontColor -> RGBColor["#000000"]
	],
	Cell[StyleData["Code-chs"],
		FontFamily -> "\:534e\:6587\:4eff\:5b8b",
		FontSize -> 20,
		FontWeight -> Bold,
		FontColor -> RGBColor["#000000"]
	],
	
	Cell[StyleData["Comment"],
		FontFamily -> "Calibri",
		FontSize -> 24,
		FontColor -> RGBColor["#777777"],
		LineSpacing -> {1, 12},
		CellMargins -> {{54, 15}, {8, 12}},
		CellFrame -> {{8, 0}, {0, 0}},
		CellFrameColor -> RGBColor["#CCCCCC"]
	],
	Cell[StyleData["Comment-chs"],
		FontFamily -> "\:5fae\:8f6f\:96c5\:9ed1",
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
	
	Cell[StyleData["DingBatTemplate"],
		TemplateBoxOptions -> {DisplayFunction -> Function[
			TagBox[RowBox[{
				TemplateBox[{#5}, "Spacer1"],
				AdjustmentBox[
					StyleBox[#1,
						FontFamily -> "Source Sans Pro",
						FontSize -> #2,
						FontColor -> RGBColor[#3]
					],
					BoxBaselineShift -> #4
				],
				TemplateBox[{#5}, "Spacer1"]
			}], Deploy, DefaultBaseStyle -> "Deployed"]
		]}
	],
	TemplateCell["DingBat", "DingBatTemplate", {Slot[1], 20, "#777777", -0.3, 0}],
	TemplateCell["Order", "DingBatTemplate", {Slot[1], 24, "#777777", -0.1, 0}],
	TemplateCell["Separator", "DingBatTemplate", {Slot[1], 24, "#777777", 0, 4}],
	
	Cell[StyleData["InlineList"],
		FontFamily -> "Source Sans Pro",
		FontSize -> 24,
		TextAlignment -> Center,
		CellMargins -> {{40, 40}, {16, 16}}
	],
	Cell[StyleData["InlineList-chs"],
		FontFamily -> "\:9ed1\:4f53",
		FontSize -> 24
	],
	
	Cell[StyleData["Table"],
		FontFamily -> "Calibri",
		FontSize -> 28,
		TextAlignment -> Center,
		CellMargins -> {{40, 40}, {16, 16}},
		GridBoxOptions -> {
			ColumnWidths -> Automatic,
			ColumnSpacings -> 2,
			RowHeights -> Automatic,
			RowSpacings -> 1.2,
			GridFrameMargins -> {{0, 0}, {0, 0}},
			BaselinePosition -> Automatic,
			RowAlignments -> Center
		}
	],
	Cell[StyleData["Table-chs"],
		FontFamily -> "\:7b49\:7ebf",
		FontSize -> 28
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
				ContentPadding -> False,
				BaselinePosition -> 1
			]
		]}
	],
	
	Cell[StyleData["CodeBlock"],
		TextAlignment -> Center,
		Selectable -> False,
		CellMargins -> {{40, 40}, {12, 12}},
		TemplateBoxOptions -> {DisplayFunction -> Function[
			FrameBox[
				AdjustmentBox[
					PaneBox[
						StyleBox[#1,
							LineSpacing -> {1, 8},
							LineBreakWithin -> False,
							TextAlignment -> Left,
							Selectable -> True
						],
						Alignment -> {Left, Center},
						AppearanceElements -> {},
						ContentPadding -> True,
						Enabled -> True,
						ImageSize -> $Width - 200
					],
					BoxMargins -> {{1.6, 1.6}, {2, 2}}
				],
				Background -> RGBColor[0.92, 0.92, 0.92],
				BoxFrame -> {{0, 0}, {0, 0}},
				RoundingRadius -> {8, 8},
				ContentPadding -> True
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
	
}]];


(* ::Input:: *)
(*RenderTMD[localPath<>"docs/test.tmd"];*)


(* ::Input:: *)
(*CurrentValue[{StyleDefinitions,"Pane"}]*)


(* ::Input:: *)
(*CreateDialog[{*)
(*Cell[*)
(*BoxData[TemplateBox[{*)
(*RowBox[{StyleBox[FormBox["\"GraceNote({13}, {5-})\"",InputForm],"Code"],"\n",StyleBox[FormBox["\"(13^)5-\"",InputForm],"Code"]}]*)
(*},"CodeBlock"]],*)
(*"CodeBlock"]*)
(*},*)
(*StyleDefinitions->StyleSheet["Document"],*)
(*WindowElements->{},*)
(*Magnification->2];*)


(* ::Input:: *)
(*PaneBox//Options*)
