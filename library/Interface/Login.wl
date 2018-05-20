(* ::Package:: *)

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

Login[] := Module[{username = "", password = "", remember = False},
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
            Hold @ URLSubmit[
              HTTPRequest["http://id.ob-studio.cn:3000/api/login", <|
                Method -> "POST",
                "Body" -> "username=" <> username <> "&password_hash=" <> Hash[password, "SHA256", "HexString"]
              |>],
              HandlerFunctions -> <|"TaskFinished" :> (Print[#Body]&)|>,
              HandlerFunctionsKeys -> {"Body"}
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
          ImageMargins -> {{4, 4}, {4, 4}},
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

End[];

EndPackage[];


(* ::Input:: *)
(*Thulium`Interface`Login`Login[];*)


(* ::Input:: *)
(*InputFieldBox//Options*)
