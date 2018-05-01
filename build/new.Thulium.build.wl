(* ::Package:: *)

With[
  {
    InitializeThulium = Hold[
      (* FIXME: optimized paclet tests *)
      If[Length[PacletCheckUpdate["QuantityUnits"]] != 0,
        PacletUpdate["QuantityUnits"];
        Quit[];
      ];
      localPath = StringReplace[NotebookDirectory[], "\\"->"/"];
      Get[localPath <> "library/initialization.wl"];
      Cells[CellTags -> "$title"];
      NotebookDelete[Cells[CellTags -> "$init"]];
      SelectionMove[First @ Cells[CellTags -> "$monitor"], Before, Cell, AutoScroll -> False];
      NotebookWrite[EvaluationNotebook[], Cell[BoxData @ RowBox[{
        TemplateBox[{4}, "Spacer1"],
        TemplateBox[{
          "Initialize Thulium",
          "Click to initialize the kernel.",
          Unevaluated @ InitializePackage
        }, "TextButton"],
        TemplateBox[{4}, "Spacer1"],
        TemplateBox[{
          "Initialize Parser",
          "Click to initialize the parser.",
          Unevaluated @ InitializeParser
        }, "TextButtonMonitored"],
        TemplateBox[{4}, "Spacer1"],
        TemplateBox[{
          "Initialize Songs",
          "Click to initialize the songs.",
          Unevaluated @ update
        }, "TextButtonMonitored"],
        TemplateBox[{4}, "Spacer1"],
        TemplateBox[{
          "Start Front End",
          "Click to run the front end.",
          Hold @ homepage
        }, "TextButton"],
        TemplateBox[{4}, "Spacer1"]
      }], "Controls", CellTags -> "$controls"]];
    ]
  },
  
  build`TemporaryNotebook = CreateDialog[
    {
      Cell[
        BoxData @ TemplateBox[{"Thulium Music Player", "\"v2.3\""}, "Title"],
        "Title", CellTags -> "$title"
      ],
      Cell[BoxData @ RowBox[{
        TemplateBox[{4}, "Spacer1"],
        TemplateBox[{
          "Start Thulium",
          "Click to initialize the kernel.",
          InitializeThulium
        }, "TextButton"],
        TemplateBox[{4}, "Spacer1"]
      }], "Init", CellTags -> "$init"],
      Cell[BoxData @ Null, "Hidden", CellTags -> "$monitor"]
    },
    
    StyleDefinitions -> Notebook[{
      Cell[StyleData[StyleDefinitions -> "Default.nb"]],
      
      Cell[StyleData["Title"],
        TextAlignment -> Center,
        ShowStringCharacters -> False,
        CellMargins -> {{40, 40}, {16, 32}},
        ShowCellBracket -> False,
        Evaluatable -> False,
        Editable -> False,
        Deletable -> False,
        TemplateBoxOptions -> {DisplayFunction -> Function[
          RowBox[{
            StyleBox[#1,
              FontFamily -> "Source Sans Pro",
              FontSize -> 32,
              FontColor -> RGBColor[0.1, 0.4, 0.7]
            ],
            TemplateBox[{1}, "Spacer1"],
            StyleBox[FormBox[#2, InputForm],
              FontFamily -> "Source Sans Pro",
              FontSize -> 24,
              FontColor -> RGBColor[0.3, 0.5, 0.8]
            ]
          }]
        ]}
      ],
      
      Cell[StyleData["Init"],
        CellMargins -> {{24, 24}, {8, 8}},
        TextAlignment -> Center,
        ShowCellBracket -> False,
        Evaluatable -> False,
        Editable -> False,
        Deployed -> True
      ],
      
      Cell[StyleData["Controls"],
        CellMargins -> {{24, 24}, {8, 8}},
        TextAlignment -> Center,
        ShowCellBracket -> False,
        Evaluatable -> False,
        Editable -> False,
        Deletable -> False,
        Deployed -> True
      ],
      
      Cell[StyleData["Tip"],
        CellMargins -> {{60, 60}, {8, 4}},
        Deployed -> True,
        Copyable -> False,
        ShowCellBracket -> False,
        TextAlignment -> Center,
        ShowCellLabel -> False,
        TemplateBoxOptions -> {DisplayFunction -> Function[
          FrameBox[
            AdjustmentBox[
              RowBox[{
                StyleBox["\[LightBulb]", FontSize -> 18],
                TemplateBox[{4}, "Spacer1"],
                StyleBox[#1, FontFamily -> "Calibri", FontSize -> 16]
              }],
              BoxBaselineShift -> 0,
              BoxMargins -> {{2, 2}, {2, 2}}
            ],
            Background -> RGBColor[1, 0.96, 0.98],
            RoundingRadius -> {8, 8},
            ContentPadding -> True,
            FrameStyle -> None
          ]
        ]}
      ],
      
      Cell[StyleData["Hidden"],
        FontSize -> 1,
        FontColor -> RGBColor[0, 0, 0, 0],
        CellSize -> {Inherited, 1},
        CellMargins -> {{24, 24}, {8, 8}},
        CellElementSpacings -> {"CellMinHeight" -> 1},
        Background -> Inherited,
        Evaluatable -> True,
        CellGroupingRules -> "InputGrouping"
      ],
      
      Cell[StyleData["TextButtonContent"],
        FontFamily -> "Sitka Text",
        FontSize -> 15
      ],
      
      Cell[StyleData["TextButtonTooltip"],
        TemplateBoxOptions -> {DisplayFunction -> Function[
          RowBox[{
            TemplateBox[{1}, "Spacer1"],
            StyleBox[#1,
              FontFamily -> "Calibri",
              FontSize -> 24
            ],
            TemplateBox[{1}, "Spacer1"]
          }]
        ]}
      ],
      
      Cell[StyleData["TextButtonDisplay"],
        TemplateBoxOptions -> {DisplayFunction -> Function[
          FrameBox[
            RowBox[{
              TemplateBox[{4}, "Spacer1"],
              AdjustmentBox[
                StyleBox[#1, FontColor -> #2],
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
          PaneSelectorBox[{
            True -> TooltipBox[
              TagBox[
                TagBox[
                  PaneSelectorBox[{
                    True -> TemplateBox[{
                      StyleBox[#1, "TextButtonContent"],
                      RGBColor[0, 0, 0],
                      RGBColor[0.5, 0.8, 1]
                    }, "TextButtonDisplay"],
                    False -> TemplateBox[{
                      StyleBox[#1, "TextButtonContent"],
                      RGBColor[0.08, 0.04, 0],
                      RGBColor[0.8, 0.9, 1]
                    }, "TextButtonDisplay"]
                  }, Dynamic @ CurrentValue["MouseButtonTest"]],
                EventHandlerTag @ {"MouseClicked" :> ReleaseHold @ #3}],
              MouseAppearanceTag @ "LinkHand"],
              TemplateBox[{#2}, "TextButtonTooltip"],
              TooltipDelay -> 0.2,
              TooltipStyle -> {
                CellFrameColor -> RGBColor[0.7, 0.7, 0.6, 0.5],
                Background -> RGBColor[1, 1, 0.9, 0.7]
              }
            ],
            False -> TemplateBox[{
              StyleBox[#1, "TextButtonContent"],
              RGBColor[0.2, 0.1, 0],
              RGBColor[0.92, 0.96, 1]
            }, "TextButtonDisplay"]
          }, Dynamic @ CurrentValue["MouseOver"]]
        ]}
      ],
      
      Cell[StyleData["TextButtonMonitored"],
        TemplateBoxOptions -> {DisplayFunction -> Function[
          TemplateBox[{#1, #2, Hold[
            NotebookLocate["$monitor"];
            NotebookWrite[EvaluationNotebook[], Cell[
              BoxData @ MakeBoxes @ Evaluate @ #3,
              "Hidden",
              CellTags -> "$monitor"
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
        CellMargins -> {{60, 60}, {8, 4}},
        Deployed -> True,
        Copyable -> False,
        ShowCellBracket -> False,
        TextAlignment -> Center
      ],
      
      (* Enhancement for cell style - Input *)
      Cell[StyleData["Input", StyleDefinitions -> "Input"],
        NumberMarks -> False,
        Editable -> True,
        Evaluatable -> True,
        StyleKeyMapping -> {">" -> "Music"},
        ContextMenu -> {
          MenuItem["Cut", "Cut"],
          MenuItem["Copy", "Copy"],
          MenuItem["Paste", FrontEnd`Paste[After]],
          Delimiter,
          MenuItem["Evaluate Cell", "EvaluateCells"]
        }
      ]
    }],
    ShowCellLabel -> False,
    ShowCellTags -> False,
    ShowCellBracket -> False,
    CellGrouping -> Manual,
    Background -> RGBColor["#FFFFFF"],
    WindowTitle -> "Thulium Music Player",
    WindowElements -> {},
    WindowFrameElements -> {"CloseBox", "MinimizeBox", "ZoomBox"},
    WindowSize -> {1440, 900},
    WindowFrame -> "ModelessDialog",
    Magnification -> 2,
    Saveable -> False,
    Evaluatable -> False,
    Editable -> False
  ];
  NotebookSave[build`TemporaryNotebook, localPath <> "Thulium.new.nb"];
  NotebookClose[build`TemporaryNotebook];
  NotebookOpen[localPath <> "Thulium.new.nb"];
]

