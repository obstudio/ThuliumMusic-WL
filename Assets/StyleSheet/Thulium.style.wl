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
		CellContext -> "Global`",
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
						EventHandlerTag @ {"MouseClicked" :> ReleaseHold[#1]}
					],
					MouseAppearanceTag @ "LinkHand"
				]
			],
			InterpretationFunction -> (#1&)
		}
	],
	
	Cell[StyleData["Thulium-Button-Display"],
		TemplateBoxOptions -> {DisplayFunction -> Function[
			FrameBox[
				RowBox[{
					TemplateBox[{1}, "Spacer1"],
					StyleBox[#1, FontColor -> #2],
					TemplateBox[{1}, "Spacer1"]
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
	
	Cell[StyleData["Thulium-Button"],
		TemplateBoxOptions -> {DisplayFunction -> Function[
			TagBox[
				TagBox[
					PaneSelectorBox[{
						True -> TemplateBox[{#1,
							RGBColor[0.08, 0.04, 0],
							RGBColor[0.8, 0.9, 1]
						}, "Thulium-Button-Display"],
						False -> TemplateBox[{#1,
							RGBColor[0.2, 0.1, 0],
							RGBColor[0.92, 0.96, 1]
						}, "Thulium-Button-Display"]
					}, Dynamic[CurrentValue["MouseOver"]]],
				EventHandlerTag @ {"MouseClicked" :> #2}],
			MouseAppearanceTag @ "LinkHand"]
		]}
	]
	
}];


(* ::Input:: *)
(*CurrentValue[{StyleDefinitions, "Text"}]*)


(* ::Input:: *)
(*CreateDocument[{*)
(*Cell[BoxData@*)
(*TemplateBox[{"1234567",Unevaluated@Print[233]},"Thulium-Button"],*)
(*FontSize->20]*)
(*},StyleDefinitions -> StyleSheet["Thulium"],*)
(*Deployed->True,Saveable->False,Magnification->2];*)
