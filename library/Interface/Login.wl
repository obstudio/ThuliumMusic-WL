(* ::Package:: *)

ClearAll["Thulium`Interface`Login`*"]
ClearAll["Thulium`Interface`Login`*`*"]


BeginPackage["Thulium`Interface`Login`", {
  "Thulium`System`",
  "Thulium`Assets`",
  "Thulium`StyleSheet`"
}];

Login::usage = "Login";

Begin["`Private`"];

SetAttributes[tmInputField, HoldAll];
Options[tmInputField] = {
  FieldHint -> "",
  FieldMasked -> False
};
tmInputField[symbol_, test_, OptionsPattern[]] := FrameBox[
  InputFieldBox[Dynamic[symbol], String,
    FieldHint -> OptionValue[FieldHint],
    FieldMasked -> OptionValue[FieldMasked]
  ],
  FrameStyle -> Dynamic @ If[test, RGBColor[0.6, 1, 0.8], RGBColor[1, 0.7, 0.7], RGBColor[1, 0.7, 0.7]],
  Background -> Dynamic @ If[test, RGBColor[0.92, 1, 0.96], RGBColor[1, 0.94, 0.94], RGBColor[1, 0.94, 0.94]]
];

Login[usernameInput_: "", passwordInput_: "", rememberInput_: False] := Module[
  {
    username = usernameInput,
    password = passwordInput,
    remember = rememberInput
  },
  With[{TextDict = TextDict, ChsFont = ChsFont},
    CreateDialog[
      {
        Cell[BoxData @ StyleBox[TextDict["Login-Title"], "Title"], "Title"],
        
        Cell[BoxData @ GridBox[{
          {
            TemplateBox[{TextDict["UserName"]}, "<Form-Tag>"],
            tmInputField[username, StringMatchQ[username, RegularExpression["\\w+"]], FieldHint -> TextDict["EnterUserName"]]
          },
          {
            TemplateBox[{TextDict["Password"]}, "<Form-Tag>"],
            tmInputField[password, password =!= "", FieldHint -> TextDict["EnterPassword"], FieldMasked -> True]
          }
        }, ColumnAlignments -> Center], "Form"],
        
        Cell[BoxData @ RowBox[{
          TemplateBox[{10}, "Spacer1"],
          PaneSelectorBox[{
            True -> TemplateBox[{
              TextDict["Login"],
              Hold @ DialogReturn[
                LoginWait[username, password, remember]
              ]
            }, "<Button-Local>"],
            False -> TemplateBox[{TextDict["Login"], GrayLevel[0.4], GrayLevel[0.96], GrayLevel[0.92]}, "<Item-Local>"]
          }, Dynamic[StringMatchQ[username, RegularExpression["\\w+"]] && password =!= ""]],
          TemplateBox[{10}, "Spacer1"],
          TemplateBox[{TextDict["GuestLogin"], Hold @ DialogReturn[]}, "<Button-Local>"],
          TemplateBox[{10}, "Spacer1"]
        }], "Buttom"]
      },
      
      StyleDefinitions -> Notebook[{
        tmButton,
        tmList,
        
        Cell[StyleData["Title"],
          CellMargins -> {{8, 8}, {12, 32}},
          TextAlignment -> Center,
          FontFamily -> ChsFont,
          FontSize -> 28
        ],
      
        Cell[StyleData["Form"],
          CellMargins -> {{4, 4}, {12, 12}},
          TextAlignment -> Center,
          GridBoxOptions -> {
            ColumnSpacings -> 2,
            RowSpacings -> 2
          },
          FrameBoxOptions -> {
            RoundingRadius -> {4, 4},
            ImageMargins -> 0,
            BoxFrame -> 1
          },
          InputFieldBoxOptions -> {
            Alignment -> Center,
            Background -> None,
            Appearance -> "Frameless",
            FieldSize -> {10, 1},
            ImageMargins -> {{4, 4}, {4, 0}},
            BaseStyle -> {
              FontSize -> 16,
              FontFamily -> EngFont,
              FontSlant -> Plain,
              FontColor -> RGBColor[0, 0, 0]
            },
            FieldHintStyle -> {
              FontSize -> 16,
              FontFamily -> ChsFont,
              FontSlant -> Plain,
              FontColor -> GrayLevel[0.4]
            },
            ContinuousAction -> True
          }
        ],
        
        Cell[StyleData["Buttom"],
          CellMargins -> {{8, 8}, {32, 12}},
          TextAlignment -> Center
        ],
        
        Cell[StyleData["<Form-Tag>"],
          TemplateBoxOptions -> {DisplayFunction -> Function[
            AdjustmentBox[
              StyleBox[#1,
                FontFamily -> ChsFont,
                FontSize -> 16
              ],
              BoxBaselineShift -> 0.3
            ]
          ]}
        ],
        
        Cell[StyleData["<Item-Local>"],
          TemplateBoxOptions -> {DisplayFunction -> Function[
            TemplateBox[{
              StyleBox[#1, FontFamily -> ChsFont, FontSize -> 14],
              #2, #3, #4, 72, 20
            }, "<Item>"]
          ]}
        ],
        
        Cell[StyleData["<Button-Local>"],
          TemplateBoxOptions -> {DisplayFunction -> Function[
            TemplateBox[{
              TemplateBox[{#1, RGBColor[0, 0, 0], RGBColor[0.92, 0.96, 1], RGBColor[0.88, 0.94, 1]}, "<Item-Local>"],
              TemplateBox[{#1, RGBColor[0, 0, 0], RGBColor[0.92, 1, 0.96], RGBColor[0.88, 1, 0.94]}, "<Item-Local>"],
              TemplateBox[{#1, RGBColor[0, 0, 0], RGBColor[0.88, 1, 1], RGBColor[0.8, 1, 1]}, "<Item-Local>"],
              #2
            }, "<Button>"]
          ]}
        ]
      }],
      
      ShowCellLabel -> False,
      ShowCellTags -> False,
      ShowCellBracket -> False,
      CellGrouping -> Manual,
      Background -> RGBColor[1, 1, 1],
      WindowTitle -> TextDict["Login"],
      WindowElements -> {},
      WindowFrameElements -> {"CloseBox", "MinimizeBox"},
      WindowSize -> {800, 600},
      WindowFrame -> "ModelessDialog",
      Magnification -> 2,
      Saveable -> False,
      Evaluatable -> False,
      Editable -> False,
      Deployed -> True,
      DynamicEvaluationTimeout -> 30
    ]];
    
    Evaluate[Unique[]] := username;
    Evaluate[Unique[]] := password;
    Evaluate[Unique[]] := remember;
];

LoginWait[username_, password_, remember_] := Module[{status = -1},
  With[{TextDict = TextDict, ChsFont = ChsFont, LogoCloud = LogoCloud, LogoNote = LogoNote},
    CreateDialog[
      {
        Cell[BoxData @ PaneSelectorBox[{
          -1 -> GridBox[{
            {TemplateBox[{
              RGBColor[0.84, 0.96, 1], 0.8,
              Dynamic @ Refresh[28 Sin[2Pi ((DateValue["SecondFraction"] - 0.5) ^ 2 + 0.5)], UpdateInterval -> 0.04]
            }, "<Logo-Dynamic>"]},
            {TextDict["Login-Wait"]}
          }],
          0 -> GridBox[{
            {TemplateBox[{RGBColor[0.84, 1, 0.96], 0.8}, "<Logo>"]},
            {TextDict["Login-Succeed"]}
          }],
          1 -> GridBox[{
            {TemplateBox[{RGBColor[1, 0.96, 0.84], 0.8}, "<Logo>"]},
            {TextDict["Login-Failed"]}
          }]
        }, Dynamic[status]], "Logo"],
        
        Cell[BoxData @ RowBox[{
          TemplateBox[{10}, "Spacer1"],
          TemplateBox[{TextDict["Confirm"], Hold @ DialogReturn[
            If[status === 0, Thulium`Homepage[], Thulium`Login[username, password, remember]]
          ]}, "<Button-Local>"],
          TemplateBox[{10}, "Spacer1"]
        }], "Buttom", CellOpen -> Dynamic[status >= 0]]
      },
      
      StyleDefinitions -> Notebook[{
        tmButton,
        tmList,
        
        Cell[StyleData["Buttom"],
          CellMargins -> {{8, 8}, {24, 12}},
          TextAlignment -> Center
        ],
        
        Cell[StyleData["Logo"],
          CellMargins -> {{8, 8}, {8, 24}},
          TextAlignment -> Center,
          GridBoxOptions -> {
            RowSpacings -> 2,
            BaseStyle -> {
              FontSize -> 16,
              FontFamily -> ChsFont
            }
          }
        ],
        
        Cell[StyleData["<Capture>"],
          TemplateBoxOptions -> {DisplayFunction -> Function[
            StyleBox[#1,
              FontSize -> 16,
              FontFamily -> ChsFont
            ]
          ]}
        ],
        
        Cell[StyleData["<Logo>"],
          TemplateBoxOptions -> {DisplayFunction -> Function[
            DynamicBox @ GraphicsBox[{
              #1,
              FilledCurveBox[{BezierCurve @ LogoCloud}],
              RGBColor[1, 1, 1],
              GeometricTransformationBox[
                FilledCurveBox[{BezierCurve @ LogoNote}],
                {{{#2, 0}, {0, #2}}, {120 #2, -120 #2}}
              ]
            }, ImageSize -> 220]
          ]}
        ],
      
        Cell[StyleData["<Logo-Dynamic>"],
          TemplateBoxOptions -> {DisplayFunction -> Function[
            DynamicBox @ GraphicsBox[{
              #1,
              FilledCurveBox[{BezierCurve @ LogoCloud}],
              RGBColor[1, 1, 1],
              GeometricTransformationBox[
                FilledCurveBox[{BezierCurve @ LogoNote}],
                {{{#2, 0}, {0, #2}}, {120 #2, #3 - 120 #2}}
              ]
            }, ImageSize -> 220]
          ]}
        ],
      
        Cell[StyleData["<Item-Local>"],
          TemplateBoxOptions -> {DisplayFunction -> Function[
            TemplateBox[{
              StyleBox[#1, FontFamily -> ChsFont, FontSize -> 14],
              #2, #3, #4, 72, 20
            }, "<Item>"]
          ]}
        ],
        
        Cell[StyleData["<Button-Local>"],
          TemplateBoxOptions -> {DisplayFunction -> Function[
            TemplateBox[{
              TemplateBox[{#1, RGBColor[0, 0, 0], RGBColor[0.92, 0.96, 1], RGBColor[0.88, 0.94, 1]}, "<Item-Local>"],
              TemplateBox[{#1, RGBColor[0, 0, 0], RGBColor[0.92, 1, 0.96], RGBColor[0.88, 1, 0.94]}, "<Item-Local>"],
              TemplateBox[{#1, RGBColor[0, 0, 0], RGBColor[0.88, 1, 1], RGBColor[0.8, 1, 1]}, "<Item-Local>"],
              #2
            }, "<Button>"]
          ]}
        ]
      }],
      
      ShowCellLabel -> False,
      ShowCellTags -> False,
      ShowCellBracket -> False,
      CellGrouping -> Manual,
      Background -> RGBColor[1, 1, 1],
      WindowTitle -> TextDict["Login-Wait"],
      WindowElements -> {},
      WindowFrameElements -> {"CloseBox", "MinimizeBox"},
      WindowSize -> {800, 600},
      WindowFrame -> "ModelessDialog",
      Magnification -> 2,
      Saveable -> False,
      Evaluatable -> False,
      Editable -> False,
      Deployed -> True
    ]
  ];
  
  URLSubmit[
    HTTPRequest["http://id.ob-studio.cn:3000/api/login", <|
      Method -> "POST",
      "ContentType" -> "application/x-www-form-urlencoded",
      "Body" -> "username=" <> username <> "&password_hash=" <> Hash[password, "SHA256", "HexString"]
    |>],
    HandlerFunctions -> <|"TaskFinished" :> Function[
      Switch[First[#Body],
        "0", status = 0,
        _, status = 1
      ]
    ]|>,
    HandlerFunctionsKeys -> {"Body"}
  ];
  
  Evaluate[Unique[]] := status;
];

End[];

Thulium`Login = Thulium`Interface`Login`Login;

EndPackage[];


(* ::Input:: *)
(*RefreshLanguage;*)


(* ::Input:: *)
(*Thulium`Interface`Login`Login[];*)


(* ::Input:: *)
(*InputFieldBox//Options*)
