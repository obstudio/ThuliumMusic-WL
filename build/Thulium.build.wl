(* ::Package:: *)

With[
  {
    ThuliumVersion = Thulium`System`$$Version,
    
    LogoCloud = BezierCurve[Thulium`Assets`LogoCloud],
    LogoNote = BezierCurve[Thulium`Assets`LogoNote],
    
    InitializeThulium = Hold[
      BeginPackage["Thulium`System`"];
      Thulium`System`$Init = False;
      Thulium`System`$Parser = False;
      Thulium`System`$LocalPath = StringReplace[NotebookDirectory[], "\\"->"/"];
      EndPackage[];
      DeclarePackage["Thulium`System`", {"$LocalPath"}];
      SetDirectory[$LocalPath];
      If[!MemberQ[
        CurrentValue[$FrontEnd, {"NotebookSecurityOptions", "TrustedPath"}],
        FrontEnd`FileName[{$RootDirectory}, Evaluate @ NotebookDirectory[]]
      ], AppendTo[
        CurrentValue[$FrontEnd, {"NotebookSecurityOptions", "TrustedPath"}],
        FrontEnd`FileName[{$RootDirectory}, Evaluate @ NotebookDirectory[]]
      ]];
      Scan[Get, FileNames[".*.mx", "library/Package", Infinity]];
      Get[$LocalPath <> "library/initialization.wl"];
      NotebookDelete[Cells[CellTags -> "$init"]];
      SelectionMove[First @ Cells[CellTags -> "$title"], After, Cell, AutoScroll -> False];
      NotebookWrite[EvaluationNotebook[], {
        MenuCell,
        Cell[BoxData @ Null, "Hidden", CellTags -> "$monitor"],
        Cell[BoxData @ TemplateBox[{
          "Welcome to Thulium Music!"
        }, "SuccessMessage"], "MessageCell", CellTags -> "$msg"]
      }];
      NotebookLocate["$title"];
    ],
    
    Prototype = {
      Thulium`StyleSheet`Include["Tooltip"],
      Thulium`StyleSheet`Include["Button"],
      Thulium`StyleSheet`Include["Message"]
    }
  },
  
  Thulium`MainNotebook = CreateDialog[
    {
      Cell[BoxData @ RowBox[{
        StyleBox["Thulium Music Player",
          FontFamily -> "Source Sans Pro",
          FontSize -> 32,
          FontColor -> RGBColor[0.1, 0.4, 0.7]
        ],
        TemplateBox[{1}, "Spacer1"],
        StyleBox[FormBox["\"v" <> ThuliumVersion <> "\"", InputForm],
          FontFamily -> "Source Sans Pro",
          FontSize -> 24,
          FontColor -> RGBColor[0.3, 0.5, 0.8]
        ]
      }], "Title", CellTags -> "$title"],
      
      Cell[BoxData @ RowBox[{
        TemplateBox[{4}, "Spacer1"],
        TemplateBox[{
          "Start Thulium",
          "Click to Start Thulium Music.",
          InitializeThulium
        }, "LogoButton"],
        TemplateBox[{4}, "Spacer1"]
      }], "Init", CellTags -> "$init"]
    },
    
    StyleDefinitions -> Notebook[{
      Sequence @@ Prototype,
                  
      Cell[StyleData["Title"],
        TextAlignment -> Center,
        ShowStringCharacters -> False,
        CellMargins -> {{40, 40}, {16, 32}},
        Evaluatable -> False,
        Deletable -> False,
        Deployed -> True
      ],
      
      Cell[StyleData["Init"],
        CellMargins -> {{24, 24}, {60, 60}},
        TextAlignment -> Center,
        Deployed -> True
      ],
      
      Cell[StyleData["Menu"],
        CellMargins -> {{24, 24}, {8, 8}},
        TextAlignment -> Center,
        Deletable -> False,
        Deployed -> True
      ],
      
      Cell[StyleData["MessageCell"],
        FontColor -> RGBColor[0, 0, 0],
        CellMargins -> {{60, 60}, {8, 4}},
        Deployed -> True,
        TextAlignment -> Center
      ],
      
      Cell[StyleData["SuccessMessage"],
        TemplateBoxOptions -> {DisplayFunction -> Function[
          TemplateBox[{#1, "\[LightBulb]", RGBColor[0.98, 1, 0.96]}, "<Message>"]
        ]}
      ],
      
      Cell[StyleData["FailureMessage"],
        TemplateBoxOptions -> {DisplayFunction -> Function[
          TemplateBox[{#1, "\[WarningSign]", RGBColor[1, 0.96, 0.98]}, "<Message>"]
        ]}
      ],
      
      Cell[StyleData["Hidden"],
        FontSize -> 1,
        FontColor -> RGBColor[0, 0, 0, 0],
        CellSize -> {Inherited, 1},
        CellMargins -> {{24, 24}, {10, 10}},
        CellElementSpacings -> {"CellMinHeight" -> 1},
        Background -> Inherited,
        Evaluatable -> True,
        CellGroupingRules -> "InputGrouping"
      ],
      
      Cell[StyleData["LogoButtonDisplay"],
        TemplateBoxOptions -> {DisplayFunction -> Function[
          GraphicsBox[{
            #3,
            FilledCurveBox[{LogoCloud}],
            RGBColor[1, 1, 1],
            FilledCurveBox[{LogoNote}]
          }, ImageSize -> 240]
        ]}
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
      ],
      
      Cell[StyleData["LogoButton"],
        TemplateBoxOptions -> {DisplayFunction -> Function[
          TemplateBox[{
            TemplateBox[{#1, RGBColor[0.2, 0.1, 0], RGBColor[0.8, 0.96, 1]}, "LogoButtonDisplay"],
            TemplateBox[{#1, RGBColor[0, 0, 0], RGBColor[0.5, 0.8, 1]}, "LogoButtonDisplay"],
            TemplateBox[{#1, RGBColor[0.08, 0.04, 0], RGBColor[0.72, 0.9, 1]}, "LogoButtonDisplay"],
            #3, #2
          }, "<Button>"]
        ]}
      ],
      
      Cell[StyleData["TextButtonMonitored"],
        TemplateBoxOptions -> {DisplayFunction -> Function[
          TemplateBox[{#1, #2, Hold[
            NotebookLocate["$monitor"];
            NotebookWrite[EvaluationNotebook[], Cell[
              BoxData @ MakeBoxes @ Evaluate @ #3,
              "Hidden", CellTags -> "$monitor"
            ], All];
            SelectionEvaluate[EvaluationNotebook[]];
            NotebookLocate["$title"];
          ]}, "TextButton"]
        ]}
      ],
      
      (* Enhancement for cell style - PrintTemporary *)
      Cell[StyleData["PrintTemporary", StyleDefinitions -> "PrintTemporary"],
        FontFamily -> "Calibri",
        FontSize -> 16,
        TextAlignment -> Center,
        CellMargins -> {{60, 60}, {8, 4}},
        Deployed -> True
      ]
    }],
    
    ShowCellLabel -> False,
    ShowCellTags -> False,
    ShowCellBracket -> False,
    CellGrouping -> Manual,
    Background -> RGBColor[1, 1, 1],
    WindowTitle -> "Thulium Music Player " <> ThuliumVersion,
    WindowElements -> {},
    WindowFrameElements -> {"CloseBox", "MinimizeBox", "ZoomBox"},
    WindowSize -> {1440, 900},
    WindowFrame -> "ModelessDialog",
    Magnification -> 2,
    Saveable -> False,
    Evaluatable -> False,
    Editable -> False,
    DynamicEvaluationTimeout -> 30
  ];
  
  NotebookSave[Thulium`MainNotebook, $LocalPath <> "Thulium.nb"];
  NotebookClose[Thulium`MainNotebook];
  NotebookOpen[$LocalPath <> "Thulium.nb"];
]




