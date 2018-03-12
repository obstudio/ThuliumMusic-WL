(* ::Package:: *)

StyleSheet["Thulium"] = Notebook[{
	Cell[StyleData[StyleDefinitions -> "Default.nb"]],
	
	Cell[StyleData["Thulium-Title"],
		FontFamily -> "Source Sans Pro",
		FontSize -> 36,
		FontColor -> RGBColor["#335599"],
		LineSpacing -> {1, 4},
		LanguageCategory -> "NaturalLanguage",
		CellMargins -> {{32, Inherited}, {16, 32}},
		ShowCellBracket -> False,
		Evaluatable -> False,
		Editable -> False,
		Deletable -> False
	],
	
	Cell[StyleData["Thulium-Instruction"],
		FontFamily -> "Corbel",
		FontSize -> 16,
		FontWeight -> Bold,
		FontColor -> RGBColor["#553399"],
		CellMargins -> {{60, Inherited}, {Inherited, Inherited}},
		LanguageCategory -> "Input",
		ShowCellBracket -> False,
		Evaluatable -> False,
		Editable -> False,
		Deletable -> False
	],
	
	Cell[StyleData["Thulium-Hyperlink"],
		TemplateBoxOptions -> {
			Editable -> False,
			DisplayFunction -> Function[
				TagBox[
					TagBox[
						StyleBox[#2, Underlined],
						EventHandlerTag @ {"MouseClicked" :> #1}
					],
					MouseAppearanceTag @ "LinkHand"
				]
			],
			InterpretationFunction -> (#1&)
		}
	]
	
}];
