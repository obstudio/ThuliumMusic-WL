(* ::Package:: *)

BeginPackage["Thulium`WorkBench`", {
  "Thulium`System`", "Thulium`Graphics`"
}];

WorkBench::usage = "Thulium Music WorkBench";
WorkBenchVersion = "1.0";

Begin["`Private`"];

WorkBench[song_: None] := Module[
  {
    filepath, title = "",
    info = <|"Status" -> "None"|>,
    workbench, stream,
    LoadingDisplay, MoveAfter
  },
  
  If[song =!= None,
    title = StringReplace[FileBaseName[song], "_" -> " "] <> " - ";
    filepath = $LocalPath <> "Songs/" <> song <> ".tm";
  ];
  
  workbench = CreateDialog[
    {
      Cell[BoxData @ RowBox[{
        StyleBox["Thulium Music WorkBench",
          FontFamily -> "Source Sans Pro",
          FontSize -> 24,
          FontColor -> RGBColor[0.1, 0.4, 0.7]
        ],
        TemplateBox[{4}, "Spacer1"],
        StyleBox[FormBox["\"v" <> WorkBenchVersion <> "\"", InputForm],
          FontFamily -> "Source Sans Pro",
          FontSize -> 18,
          FontColor -> RGBColor[0.3, 0.5, 0.8]
        ]
      }], "Title", CellTags -> "title"],
      Cell[BoxData @ "", "Separator", CellTags -> "separator"]
    },
    
    StyleDefinitions -> Notebook[{
      Thulium`Template`Include["Tooltip"],
      Thulium`Template`Include["Button"],
      Thulium`Template`Include["Pane"],
      
      Cell[StyleData["Title"],
        TextAlignment -> Center,
        ShowStringCharacters -> False,
        CellMargins -> {{32, 32}, {12, 20}},
        Deletable -> False
      ],
      
      Cell[StyleData["Separator"],
        FontSize -> 1,
        FontColor -> RGBColor[0, 0, 0, 0],
        CellSize -> {Inherited, 1},
        CellMargins -> {{16, 16}, {8, 0}},
        CellFrame -> {{0, 0}, {0, 2}},
        CellFrameMargins -> 0,
        CellFrameColor -> RGBColor[0.5, 0.6, 0.7],
        CellElementSpacings -> {"CellMinHeight" -> 1}
      ],
      
      Cell[StyleData["Menu"],
        CellMargins -> {{16, 16}, {8, 8}},
        TextAlignment -> Center
      ],
      
      Cell[StyleData["TextButtonDisplay"],
        TemplateBoxOptions -> {DisplayFunction -> Function[
          FrameBox[
            RowBox[{
              TemplateBox[{4}, "Spacer1"],
              AdjustmentBox[
                StyleBox[#1,
                  FontFamily -> "Sitka Text",
                  FontSize -> 12,
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
      ],
      
      Cell[StyleData["Monitor"],
        TextAlignment -> Center,
        CellMargins -> {{4, 4}, {8, 4}}
      ]
    }],
    
    ShowCellLabel -> False,
    ShowCellTags -> False,
    ShowCellBracket -> False,
    CellGrouping -> Manual,
    Background -> RGBColor[1, 1, 1],
    WindowTitle -> title <> "Thulium Music WorkBench " <> WorkBenchVersion,
    WindowElements -> {},
    WindowFrameElements -> {"CloseBox", "MinimizeBox"},
    WindowSize -> {800, 600},
    WindowFrame -> "ModelessDialog",
    Magnification -> 2,
    Saveable -> False,
    Evaluatable -> False,
    Editable -> False,
    Deployed -> True,
    DynamicEvaluationTimeout -> 60
  ];
  
  MoveAfter[tag_] := SelectionMove[Last @ Cells[workbench, CellTags -> tag], After, Cell, AutoScroll -> False];
  LoadingDisplay[content_, tag_] := (
    NotebookWrite[workbench, Cell[BoxData @ TemplateBox[{
      content, RGBColor[0.96, 0.98, 1], 320
    }, "<Pane>"], "Monitor", CellTags -> tag]];
    NotebookLocate["title"];
  );
  
  MoveAfter["separator"];
  LoadingDisplay["Initializing ...", ".init"];
  
  SessionSubmit[
    ExternalEvaluate[System`JS, "new Thulium('" <> filepath <> "').information"],
    HandlerFunctions -> <|"TaskFinished" :> Function[
      info = #EvaluationResult;
      NotebookDelete[Cells[workbench, CellTags -> ".init"]];
      MoveAfter["separator"];
      NotebookWrite[workbench, Cell[BoxData @ TemplateBox[{
        GridBox[{
          {"Sections:", Length @ info["Sections"]},
          {"MusicClips:", Length @ info["MusicClips"]}
        }, ColumnAlignments -> {Center, Left}],
        RGBColor[0.98, 1, 0.96], 320
      }, "<Pane>"], "Monitor", CellTags -> "info"]];
      LoadingDisplay["Generating Music ...", ".gen"];
      
      SessionSubmit[
        AudioStream @ Thulium`AudioAdapt @ info["MusicClips"],
        HandlerFunctions -> <|"TaskFinished" :> Function[
          stream = #EvaluationResult;
          NotebookDelete[Cells[workbench, CellTags -> ".gen"]];
          MoveAfter["info"];
          NotebookWrite[workbench, Cell[BoxData @ RowBox[{
            TemplateBox[{1}, "Spacer1"],
            TemplateBox[{
              "Play",
              "Click to play the song",
              Unevaluated @ AudioPlay[stream]
            }, "TextButton"],
            TemplateBox[{1}, "Spacer1"],
            TemplateBox[{
              "Stop",
              "Click to stop the playback",
              Unevaluated @ AudioStop[]
            }, "TextButton"],
            TemplateBox[{1}, "Spacer1"]
          }], "Menu", CellTags -> "menu"]];
        ]|>,
        HandlerFunctionsKeys -> {"EvaluationResult"}
      ];
    ]|>,
    HandlerFunctionsKeys -> {"EvaluationResult"}
  ];
];

WorkBenchInitialize[workbench_, filepath_] := Module[{info, stream},
  Monitor[
    info = ExternalEvaluate[System`JS, "new Thulium('" <> filepath <> "').information"],
    WorkBenchMonitor["Initializing ..."]
  ];
  NotebookDelete[Cells[CellTags -> "monitor"]];
  SelectionMove[First @ Cells[CellTags -> "separator"], After, Cell, AutoScroll -> False];
  NotebookWrite[EvaluationNotebook[], With[{info = info}, Cell[BoxData @ RowBox[{
    TemplateBox[{1}, "Spacer1"],
    TemplateBox[{
      "Play",
      "Click to play the song",
      Unevaluated @ SessionSubmit[
        Thulium`AudioAdapt @ info["MusicClips"],
        HandlerFunctions -> <|"TaskFinished" :> Function[
          stream = AudioStream[#EvaluationResult]; AudioPlay[stream];
        ]|>,
        HandlerFunctionsKeys -> {"EvaluationResult"}
      ]
    }, "TextButton"],
    TemplateBox[{1}, "Spacer1"],
    TemplateBox[{
      "Stop",
      "Click to stop the playback",
      Unevaluated @ AudioStop[]
    }, "TextButton"],
    TemplateBox[{1}, "Spacer1"]
  }], "Menu", CellTags -> "menu"]]];
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
(*CurrentValue[{StyleDefinitions,"Panel"}]*)
