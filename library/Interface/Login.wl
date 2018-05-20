(* ::Package:: *)

BeginPackage["Thulium`Interface`Login`", {
  "Thulium`System`",
  "Thulium`Assets`"
}];

Login::usage = "Login";

Begin["`Private`"];

SetAttributes[tmInputField, HoldFirst];
Options[tmInputField] = {
  FieldHint -> "",
  FieldMasked -> False
};
tmInputField[symbol_, test_, OptionsPattern[]] := FrameBox[
  InputFieldBox[Dynamic[symbol], String,
    FieldHint -> OptionValue[FieldHint],
    FieldMasked -> OptionValue[FieldMasked],
    ContinuousAction -> True
  ],
  FrameStyle -> Dynamic @ If[test, RGBColor[0.6, 1, 0.8], RGBColor[1, 0.7, 0.7], RGBColor[1, 0.7, 0.7]],
  Background -> Dynamic @ If[test, RGBColor[0.92, 1, 0.96], RGBColor[1, 0.94, 0.94], RGBColor[1, 0.94, 0.94]]
];

Login[] := Module[{username = "", password = "", remember = ""},
  CreateDialog[
    {
      Cell[BoxData @ StyleBox[TextDict["Login-Title"], "Title"], "Title"],
      Cell[BoxData @ GridBox[{
        {
          TextDict["UserName"],
          tmInputField[username, StringMatchQ[username, RegularExpression["\\w+"]], FieldHint -> TextDict["EnterUserName"]]
        },
        {
          TextDict["Password"],
          tmInputField[password, password != "", FieldHint -> TextDict["EnterPassword"], FieldMasked -> True]
        }
      }, ColumnAlignments -> Center], "Form"],
      Cell[BoxData @ RowBox[{
        TemplateBox[{10}, "Spacer1"],
        TemplateBox[{}, "<Button-Local>"]
      }]]
    },
    
    StyleDefinitions -> Notebook[{
      Cell[StyleData["Title"],
        CellMargins -> {{8, 8}, {32, 32}},
        TextAlignment -> Center,
        FontFamily -> ChsFont,
        FontSize -> 32
      ],
    
      Cell[StyleData["Form"],
        CellMargins -> {{4, 4}, {24, 24}},
        TextAlignment -> Center,
        FrameBoxOptions -> {
          RoundingRadius -> {4, 4},
          ImageMargins -> 0,
          BoxFrame -> 1
        },
        InputFieldBoxOptions -> {
          Background -> None,
          Appearance -> "Frameless",
          FieldSize -> {8, 1},
          ImageMargins -> {{4, 4}, {4, 0}},
          BaseStyle -> {
            FontSize -> 20,
            FontFamily -> EngFont,
            FontSlant -> Plain,
            FontColor -> RGBColor[0, 0, 0]
          },
          FieldHintStyle -> {
            FontSize -> 20,
            FontFamily -> ChsFont,
            FontSlant -> Plain,
            FontColor -> GrayLevel[0.4]
          }
        }
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
  ];
  
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
