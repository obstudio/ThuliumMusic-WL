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
      developer = UserInfo["Developer"]
    },
    With[{ChsFont = ChsFont, TextDict = TextDict},
      CreateDialog[
        {
          Cell[TextDict["Settings-Title"], "Title"],
          
          Cell[BoxData @ GridBox[{
            {
              TemplateBox[{TextDict["ChooseIdentity"] <> ":"}, "<Setter-Tag>"],
              RowBox[{
                TemplateBox[{Unevaluated[developer], False,
                  StyleBox[TextDict["NormalUser-Info"], "SetterInfo"],
                  StyleBox[TextDict["NormalUser"], "SetterChoice"]
                }, "<Setter-Local>"],
                TemplateBox[{10}, "Spacer1"],
                TemplateBox[{Unevaluated[developer], True,
                  StyleBox[TextDict["Developer-Info"], "SetterInfo"],
                  StyleBox[TextDict["Developer"], "SetterChoice"]
                }, "<Setter-Local>"]
              }]
            },
            {
              TemplateBox[{TextDict["ChooseLanguage"] <> ":"}, "<Setter-Tag>"],
              RowBox[{
                TemplateBox[{Unevaluated[language], "chs",
                  StyleBox[LangDict["chs"], "SetterInfo"],
                  StyleBox[LangDict["chs"], "SetterChoice"]
                }, "<Setter-Local>"],
                TemplateBox[{10}, "Spacer1"],
                TemplateBox[{Unevaluated[language], "eng",
                  StyleBox[LangDict["eng"], "SetterInfo"],
                  StyleBox[LangDict["eng"], "SetterChoice"]
                }, "<Setter-Local>"]
              }]
            }
          }], "SetterList"],
          
          Cell[BoxData @ RowBox[{
            TemplateBox[{8}, "Spacer1"],
            TemplateBox[{"Tick", Hold @ DialogReturn[Thulium`Homepage[]]}, "<Button-Local>"],
            TemplateBox[{8}, "Spacer1"],
            TemplateBox[{"Return", Hold @ DialogReturn[
              UserInfo["Developer"] = GetValue[developer];
              UserInfo["Language"] = GetValue[language];
              Export[$UserPath <> "Default.json", UserInfo];
              RefreshLanguage;
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
            CellMargins -> {{0, 0}, {16, 16}},
            TextAlignment -> Center,
            GridBoxOptions -> {
              GridBoxAlignment -> Center,
              RowSpacings -> 4,
              ColumnSpacings -> 1
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
          
          Cell[StyleData["SetterInfo"],
            FontFamily -> ChsFont,
            FontSize -> 12
          ],
          
          Cell[StyleData["Bottom"],
            CellMargins -> {{0, 0}, {40, 24}},
            TextAlignment -> Center
          ],
          
          Cell[StyleData["<Item-Local>"],
            TemplateBoxOptions -> {DisplayFunction -> Function[
              TemplateBox[{StyleBox[#1, FontWeight -> #2], RGBColor[0, 0, 0], #3, #4, 120, 32}, "<Item>"]
            ]}
          ],
          
          Cell[StyleData["<Setter-Local>"],
            TemplateBoxOptions -> {DisplayFunction -> Function[
              TemplateBox[{#1, #2, #3,
                TemplateBox[{#4, Plain, RGBColor[0.94, 0.96, 1], RGBColor[0.85, 0.9, 1]}, "<Item-Local>"],
                TemplateBox[{#4, Bold, RGBColor[0.92, 1, 0.88], RGBColor[0.8, 1, 0.7]}, "<Item-Local>"],
                TemplateBox[{#4, Plain, RGBColor[0.8, 1, 0.7], RGBColor[0.92, 1, 0.88]}, "<Item-Local>"],
                TemplateBox[{#4, Plain, RGBColor[0.85, 0.9, 1], RGBColor[0.94, 0.96, 1]}, "<Item-Local>"]
              }, "<Setter>"]
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
