(* ::Package:: *)

InitializeThulium := (
	localPath = StringReplace[NotebookDirectory[], "\\"->"/"];
	SetDirectory[localPath];
	Scan[Get, FileNames["*.wl", "library", Infinity]];
	Scan[Get, FileNames["*.wl", "package", Infinity]];
	Scan[Get, FileNames["*.wl", "assets", Infinity]];
	Get[localPath <> "Preload.wl"];
	Thulium`$Initialized = True;
);

Export[localPath <> "Thulium.nb", CreateDocument[
	{
		Cell["Thulium Music Player v2.1", "Thulium-Title"],
		Cell[CellGroupData[{
			Cell[BoxData @ RowBox[{
				"Click ",
				TemplateBox[{Unevaluated[InitializeThulium], "here"}, "Thulium-Hyperlink"],
				" to initialize the program."
			}], "Thulium-Instruction"],
			Cell[BoxData @ RowBox[{
				"Click ",
				TemplateBox[{Unevaluated[InitializeParser],"here"}, "Thulium-Hyperlink"],
				" to initialize the parser."
			}], "Thulium-Instruction"],
			Cell[BoxData @ RowBox[{
				"Click ",
				TemplateBox[{Unevaluated[Main],"here"}, "Thulium-Hyperlink"],
				" to run the front end."
			}], "Thulium-Instruction", CellOpen -> Dynamic[Thulium`$Initialized]]
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
]];
