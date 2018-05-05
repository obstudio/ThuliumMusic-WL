(* ::Package:: *)

BeginPackage["Thulium`WorkBench`", {
  "Thulium`System`", "Thulium`Graphics`", "Thulium`Template`"
}];

WorkBench::usage = "Thulium Music WorkBench";
WorkBenchVersion = "1.0";

Begin["`Private`"];

WorkBench[song_: None] := Module[
  {filepath, title = "", info = <|"Status" -> "None"|>, workbench, stream},
  If[song =!= None,
    title = StringReplace[FileBaseName[song], "_" -> " "] <> " - ";
    filepath = $LocalPath <> "Songs/" <> song <> ".tm";
  ];
  
  workbench = CreateDialog[
    {
      Cell[BoxData @ RowBox[{
        StyleBox["Thulium Music WorkBench",
          FontFamily -> "Source Sans Pro",
          FontSize -> 28,
          FontColor -> RGBColor[0.1, 0.4, 0.7]
        ],
        TemplateBox[{1}, "Spacer1"],
        StyleBox[FormBox["\"v" <> WorkBenchVersion <> "\"", InputForm],
          FontFamily -> "Source Sans Pro",
          FontSize -> 20,
          FontColor -> RGBColor[0.3, 0.5, 0.8]
        ]
      }], "Title", CellTags -> "title"],
      
      Cell[BoxData @ "", "Hidden", CellTags -> "hidden"]
    },
    
    StyleDefinitions -> Notebook[{
      Thulium`Template`Include["Tooltip"],
      Thulium`Template`Include["Button"],
      
      Cell[StyleData["Title"],
        TextAlignment -> Left,
        ShowStringCharacters -> False,
        CellMargins -> {{40, 40}, {16, 32}},
        Deletable -> False,
        Deployed -> True
      ],
      
      Cell[StyleData["Menu"],
        CellMargins -> {{24, 24}, {8, 8}},
        TextAlignment -> Center,
        Deletable -> False,
        Deployed -> True
      ],
      
      Cell[StyleData["Hidden"],
        FontSize -> 1,
        FontColor -> RGBColor[0, 0, 0, 0],
        CellSize -> {Inherited, 1},
        CellMargins -> {{24, 24}, {0, 0}},
        CellFrame -> {{0, 0}, {0, 2}},
        CellFrameMargins -> 0,
        CellFrameColor -> RGBColor[0.6, 0.7, 0.7],
        CellElementSpacings -> {"CellMinHeight" -> 1}
      ],
      
      Cell[StyleData["TextButtonDisplay"],
        TemplateBoxOptions -> {DisplayFunction -> Function[
          FrameBox[
            RowBox[{
              TemplateBox[{4}, "Spacer1"],
              AdjustmentBox[
                StyleBox[#1,
                  FontFamily -> "Sitka Text",
                  FontSize -> 15,
                  FontColor -> #2
                ],
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
          TemplateBox[{
            TemplateBox[{#1, RGBColor[0.2, 0.1, 0], RGBColor[0.92, 0.96, 1]}, "TextButtonDisplay"],
            TemplateBox[{#1, RGBColor[0, 0, 0], RGBColor[0.5, 0.8, 1]}, "TextButtonDisplay"],
            TemplateBox[{#1, RGBColor[0.08, 0.04, 0], RGBColor[0.8, 0.9, 1]}, "TextButtonDisplay"],
            #3, #2
          }, "<Button>"]
        ]}
      ]
    }],
    
    ShowCellLabel -> False,
    ShowCellTags -> False,
    ShowCellBracket -> False,
    CellGrouping -> Manual,
    Background -> RGBColor[1, 1, 1],
    WindowTitle -> title <> "Thulium Music WorkBench " <> WorkBenchVersion,
    WindowElements -> {},
    WindowFrameElements -> {"CloseBox", "MinimizeBox", "ZoomBox"},
    WindowSize -> {1440, 900},
    WindowFrame -> "ModelessDialog",
    Magnification -> 2,
    Saveable -> False,
    Evaluatable -> False,
    Editable -> False,
    DynamicEvaluationTimeout -> 60
  ];
  
  SessionSubmit[
    ExternalEvaluate[System`JS, "new Thulium('" <> filepath <> "').information"],
    HandlerFunctions -> <|"TaskFinished" :> (
      info = #EvaluationResult;
      SelectionMove[First @ Cells[workbench, CellTags -> "hidden"], After, Cell, AutoScroll -> False];
      NotebookWrite[workbench, Cell[BoxData @ RowBox[{
        TemplateBox[{1}, "Spacer1"],
        TemplateBox[{
          "Play",
          "Click to play the song",
          Unevaluated @ If[song =!= None, SessionSubmit[
            AudioPlay @ Thulium`AudioAdapt @ info["MusicClips"],
            HandlerFunctions -> <|"TaskFinished" :> (stream = #EvaluationResult&)|>,
            HandlerFunctionsKeys -> {"EvaluationResult"}
          ]]
        }, "TextButton"],
        TemplateBox[{1}, "Spacer1"],
        TemplateBox[{
          "Stop",
          "Click to stop the playback",
          Unevaluated @ AudioStop[]
        }, "TextButton"],
        TemplateBox[{1}, "Spacer1"]
      }], "Menu", CellTags -> "menu"]];
    &)|>,
    HandlerFunctionsKeys -> {"EvaluationResult"}
  ];
];

FileSelector[directory_] := Block[
  {filenames, dirnames},
  filenames = FileNames["*.tm", $LocalPath <> "Songs/" <> directory];
  filenames = FileNames["*", $LocalPath <> "Songs/" <> directory]
];

End[];

EndPackage[];

DeclarePackage["Thulium`WorkBench`", {"WorkBench"}];


(* ::Input:: *)
(*ClearAll[WorkBench]*)


(* ::Input:: *)
(*WorkBench["Touhou/Oriental_Blood"]*)


(* ::Input:: *)
(*tmptoken = ExternalEvaluate[System`JS, "new Thulium('"<>$LocalPath<>"Songs/Touhou/Oriental_Blood"<>".tm').information"];*)


(* ::Input:: *)
(*ExternalEvaluate[System`JS, "new Thulium"]*)


(* ::Input:: *)
(*CurrentValue[{StyleDefinitions,"FileNameSetterBoxes"}]*)
