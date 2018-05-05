(* ::Package:: *)

Thulium`WorkBench[config_] := Block[
  {title},
  title = StringReplace[FileBaseName[config["File"]], "_" -> " "];
  CreateDialog[
    {
      Cell[BoxData @ TemplateBox[{
        "Hello World", RGBColor[0.8, 0.9, 1]
      }, "Tab"], "TabSet"]
    },
    
    StyleDefinitions -> Notebook[{
      Cell[StyleData["TabSet"],
        CellMargins -> {{40, 40}, {0, 10}},
        Evaluatable -> False,
        Deletable -> False,
        Deployed -> True
      ],
      
      Cell[StyleData["Tab"],
        TemplateBoxOptions -> {DisplayFunction -> Function[
          FrameBox[
            RowBox[{
              TemplateBox[{4}, "Spacer1"],
              AdjustmentBox[
                StyleBox[#1,
                  FontFamily -> "Sitka Text",
                  FontSize -> 15
                ],
                BoxBaselineShift -> 0.2,
                BoxMargins -> {{0, 0}, {0.4, 0.4}}
              ],
              TemplateBox[{4}, "Spacer1"]
            }],
            Background -> #2,
            ImageMargins -> {{0, 0}, {0, 0}},
            BoxFrame -> {{0, 0}, {0, 0}},
            RoundingRadius -> {8, 8},
            ContentPadding -> True
          ]
        ]}
      ]
    }],
    
    ShowCellLabel -> False,
    ShowCellTags -> False,
    ShowCellBracket -> False,
    CellGrouping -> Manual,
    Background -> RGBColor[1, 1, 1],
    WindowTitle -> title <> " - Thulium Music " <> $Version,
    WindowElements -> {},
    WindowFrameElements -> {"CloseBox", "MinimizeBox", "ZoomBox"},
    WindowSize -> {1440, 900},
    WindowFrame -> "ModelessDialog",
    Magnification -> 2,
    Saveable -> False,
    Evaluatable -> False,
    Editable -> False,
    Deployed -> True,
    DynamicEvaluationTimeout -> 30
  ];
];


(* ::Input:: *)
(*Thulium`WorkBench[<|"File"->"Touhou/Oriental_Blood","Sections" -> tmptoken|>]*)


(* ::Input:: *)
(*tmptoken = ExternalEvaluate[System`JS, "new Thulium('"<>$LocalPath<>"Songs/Touhou/Oriental_Blood"<>".tm').Sections"];*)
