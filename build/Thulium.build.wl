(* ::Package:: *)

Export[localPath <> "Thulium.nb",
	CreateDocument[
		{
			Cell["Thulium Music Player v2.1", "Thulium-Title"],
			Cell[CellGroupData[{
				Cell[BoxData @ RowBox[{
					"Click ",
					TemplateBox[{
						Unevaluated[
							localPath = StringReplace[NotebookDirectory[], "\\"->"/"];
							SetDirectory[localPath];
							Scan[Get, FileNames["*.wl", "library", Infinity]];
							Needs["graphics`"];
							Scan[Get, FileNames["*.wl", "package", Infinity]];
							Scan[Get, FileNames["*.wl", "assets", Infinity]];
							Get[localPath <> "Preload.wl"];
						],
						"here"
					}, "Thulium-Hyperlink"],
					" to initialize the program."
				}], "Thulium-Instruction"],
				Cell[BoxData @ RowBox[{
					"Click ",
					TemplateBox[{
						Unevaluated[InitializeParser],
						"here"
					}, "Thulium-Hyperlink"],
					" to initialize parser."
				}], "Thulium-Instruction"],
				Cell[BoxData @ RowBox[{
					"Click ",
					TemplateBox[{
						Unevaluated[Main],
						"here"
					}, "Thulium-Hyperlink"],
					" to run the front end."
				}], "Thulium-Instruction"]
			}]],
			Cell[BoxData @ MakeBoxes[
				AudioStop[];
			], "Input"]
		},
		StyleDefinitions -> StyleSheet["Thulium"],
		CellGrouping -> Manual,
		WindowTitle -> "Thulium",
		WindowElements -> {"VerticalScrollBar"},
		Magnification -> 1.2,
		Saveable -> False
	]
];
