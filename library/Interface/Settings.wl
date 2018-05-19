(* ::Package:: *)

BeginPackage["Thulium`Interface`Settings`", {
  "Thulium`System`",
  "Thulium`Assets`",
  "Thulium`StyleSheet`"
}];

Settings::usage = "Thulium Music Settings Page";

Begin["`Private`"];

Settings[] := Block[{},
  Module[
    {
      language = UserInfo["Language"],
      developer = UserInfo["Developer"],
      listLength = UserInfo["ListLength"],
      looping = UserInfo["Looping"]
    },
    With[{ChsFont = ChsFont, TextDict = TextDict},
      CreateDialog[
        {
          Cell[TextDict["Settings-Title"], "Title"],
          
          Cell[BoxData @ GridBox[{
            {
              TemplateBox[{TextDict["ChooseIdentity"]}, "<Setter-Tag>"],
              RowBox[{
                TemplateBox[{Unevaluated[developer], False, 120,
                  TextDict["NormalUser-Info"],
                  StyleBox[TextDict["NormalUser"], "SetterChoice"]
                }, "<Setter-Tooltip-Local>"],
                TemplateBox[{10}, "Spacer1"],
                TemplateBox[{Unevaluated[developer], True, 120,
                  TextDict["Developer-Info"],
                  StyleBox[TextDict["Developer"], "SetterChoice"]
                }, "<Setter-Tooltip-Local>"]
              }]
            },
            {
              TemplateBox[{TextDict["ChooseLanguage"]}, "<Setter-Tag>"],
              RowBox[{
                TemplateBox[{Unevaluated[language], "chs", 120,
                  StyleBox[LangDict["chs"], "SetterChoice"]
                }, "<Setter-Local>"],
                TemplateBox[{10}, "Spacer1"],
                TemplateBox[{Unevaluated[language], "eng", 120,
                  StyleBox[LangDict["eng"], "SetterChoice"]
                }, "<Setter-Local>"]
              }]
            },
            {
              TemplateBox[{TextDict["ChoosePlayMode"]}, "<Setter-Tag>"],
              RowBox[{
                TemplateBox[{Unevaluated[looping], False, 120,
                  StyleBox[TextDict["Sequential"], "SetterChoice"]
                }, "<Setter-Local>"],
                TemplateBox[{10}, "Spacer1"],
                TemplateBox[{Unevaluated[looping], True, 120,
                  StyleBox[TextDict["Looping"], "SetterChoice"]
                }, "<Setter-Local>"]
              }]
            },
            {
              TemplateBox[{TextDict["ChooseListLength"]}, "<Setter-Tag>"],
              RowBox[{
                TemplateBox[{Unevaluated[listLength], 10, 75,
                  StyleBox["10", "SetterChoice"]
                }, "<Setter-Local>"],
                TemplateBox[{8}, "Spacer1"],
                TemplateBox[{Unevaluated[listLength], 20, 75,
                  StyleBox["20", "SetterChoice"]
                }, "<Setter-Local>"],
                TemplateBox[{8}, "Spacer1"],
                TemplateBox[{Unevaluated[listLength], 30, 75,
                  StyleBox["30", "SetterChoice"]
                }, "<Setter-Local>"]
              }]
            }
          }], "SetterList"],
          
          Cell[BoxData @ RowBox[{
            TemplateBox[{8}, "Spacer1"],
            TemplateBox[{"Tick", Hold @ DialogReturn[
              UserInfo["Developer"] = GetValue[developer];
              UserInfo["Language"] = GetValue[language];
              UserInfo["ListLength"] = GetValue[listLength];
              UserInfo["Looping"] = GetValue[looping];
              Export[$UserPath <> "Default.json", UserInfo];
              RefreshLanguage;
              Thulium`Homepage[];
            ]}, "<Button-Local>"],
            TemplateBox[{8}, "Spacer1"],
            TemplateBox[{"Return", Hold @ DialogReturn[
              Thulium`Homepage[];
            ]}, "<Button-Local>"],
            TemplateBox[{8}, "Spacer1"]
          }], "Bottom"]
        },
        
        StyleDefinitions -> Notebook[{
          tmList,
          tmButton,
          
          Cell[StyleData["Title"],
            CellMargins -> {{0, 0}, {24, 40}},
            TextAlignment -> Center,
            FontFamily -> ChsFont,
            FontSize -> 32,
            FontWeight -> Bold
          ],
          
          Cell[StyleData["SetterList"],
            CellMargins -> {{0, 0}, {8, 8}},
            TextAlignment -> Center,
            GridBoxOptions -> {
              RowSpacings -> 2,
              ColumnSpacings -> 2,
              ColumnAlignments -> {Center, Center},
              RowAlignments -> {Center, Center}
            }
          ],
          
          Cell[StyleData["<Setter-Tag>"],
            TemplateBoxOptions -> {DisplayFunction -> Function[
              AdjustmentBox[
                StyleBox[#1,
                  FontFamily -> ChsFont,
                  FontSize -> 16
                ],
                BoxBaselineShift -> 0.3,
                BoxMargins -> {{0, 0.4}, {0, 0}}
              ]
            ]}
          ],
          
          Cell[StyleData["SetterChoice"],
            FontFamily -> ChsFont,
            FontSize -> 16
          ],
          
          Cell[StyleData["Bottom"],
            CellMargins -> {{0, 0}, {40, 24}},
            TextAlignment -> Center
          ],
          
          Cell[StyleData["<Item-Local>"],
            TemplateBoxOptions -> {DisplayFunction -> Function[
              TemplateBox[{StyleBox[#1, FontWeight -> #2], RGBColor[0, 0, 0], #3, #4, #5, 32}, "<Item>"]
            ]}
          ],
          
          Cell[StyleData["<Setter-Local>"],
            TemplateBoxOptions -> {DisplayFunction -> Function[
              TemplateBox[{#1, #2,
                TemplateBox[{#4, Plain, RGBColor[0.94, 0.96, 1], RGBColor[0.85, 0.9, 1], #3}, "<Item-Local>"],
                TemplateBox[{#4, Bold, RGBColor[0.92, 1, 0.88], RGBColor[0.8, 1, 0.7], #3}, "<Item-Local>"],
                TemplateBox[{#4, Plain, RGBColor[0.8, 1, 0.7], RGBColor[0.92, 1, 0.88], #3}, "<Item-Local>"],
                TemplateBox[{#4, Plain, RGBColor[0.85, 0.9, 1], RGBColor[0.94, 0.96, 1], #3}, "<Item-Local>"]
              }, "<Setter>"]
            ]}
          ],
          
          Cell[StyleData["<Setter-Tooltip-Local>"],
            TemplateBoxOptions -> {DisplayFunction -> Function[
              TemplateBox[{#1, #2, 
                StyleBox[#4, FontFamily -> ChsFont, FontSize -> 24],
                TemplateBox[{#5, Plain, RGBColor[0.94, 0.96, 1], RGBColor[0.85, 0.9, 1], #3}, "<Item-Local>"],
                TemplateBox[{#5, Bold, RGBColor[0.92, 1, 0.88], RGBColor[0.8, 1, 0.7], #3}, "<Item-Local>"],
                TemplateBox[{#5, Plain, RGBColor[0.8, 1, 0.7], RGBColor[0.92, 1, 0.88], #3}, "<Item-Local>"],
                TemplateBox[{#5, Plain, RGBColor[0.85, 0.9, 1], RGBColor[0.94, 0.96, 1], #3}, "<Item-Local>"]
              }, "<Setter-Tooltip>"]
            ]}
          ],
          
          Cell[StyleData["<Button-Local>"],
            TemplateBoxOptions -> {DisplayFunction -> Function[
              TemplateBox[{#1, StyleBox[TextDict[#1], FontFamily -> ChsFont], 40, #2}, "<Button-Default>"]
            ]}
          ]
        }],
        
        ShowCellLabel -> False,
        ShowCellTags -> False,
        ShowCellBracket -> False,
        CellGrouping -> Manual,
        Background -> RGBColor[1, 1, 1],
        WindowTitle -> TextDict["Settings"],
        WindowElements -> {},
        WindowFrameElements -> {"CloseBox", "MinimizeBox"},
        WindowSize -> {1200, 900},
        WindowFrame -> "ModelessDialog",
        Magnification -> 2,
        Saveable -> False,
        Editable -> False,
        Deployed -> True,
        Evaluatable -> False
      ];
    ];
    
    Evaluate[Unique[]] := language;
    Evaluate[Unique[]] := developer;
    Evaluate[Unique[]] := looping;
    Evaluate[Unique[]] := listLength;
  ];
];

End[];

Thulium`Settings = Thulium`Interface`Settings`Settings;

EndPackage[];


(* ::Input:: *)
(*RefreshLanguage;*)


(* ::Input:: *)
(*ClearAll["Thulium`Interface`Settings`*"];*)


(* ::Input:: *)
(*Thulium`Settings[];*)


(* ::Input:: *)
(*UserInfo*)
