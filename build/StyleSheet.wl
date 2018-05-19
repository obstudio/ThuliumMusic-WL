(* ::Package:: *)

ClearAll["Thulium`StyleSheet`*"];
ClearAll["Thulium`StyleSheet`*`*"];


BeginPackage["Thulium`StyleSheet`"];

Include = <||>;

Begin["`Build`"];

AssignTemplate[name_String, display_Function] := (
  AssociateTo[Include, name -> Cell[
    StyleData["<" <> name <> ">"],
    TemplateBoxOptions -> {
      DisplayFunction -> display
    }
  ]]
);

AssignTemplate["Tooltip", Function[
  TooltipBox[#1,
    FrameBox[
      AdjustmentBox[
        StyleBox[#2,
          FontFamily -> "Calibri",
          FontSize -> 24,
          FontColor -> RGBColor[0, 0, 0]
        ],
        BoxMargins -> {{0.2, 0.2}, {0.2, 0.4}}
      ],
      Background -> RGBColor[1, 1, 0.9, 0.8],
      FrameStyle -> {1, RGBColor[0.8, 0.8, 0.7, 0.2]},
      RoundingRadius -> {8, 8},
      ContentPadding -> True
    ],
    TooltipDelay -> #3,
    TooltipStyle -> {
      CellFrame -> {{0, 0}, {0, 0}},
      Background -> RGBColor[0, 0, 0, 0]
    }
  ]
]];

AssignTemplate["Button", Function[
  PaneSelectorBox[{
    True -> TemplateBox[{
      TagBox[
        TagBox[
          PaneSelectorBox[{
            True -> #2,
            False -> #3
          }, Dynamic @ CurrentValue["MouseButtonTest"]],
        EventHandlerTag @ {"MouseClicked" :> ReleaseHold @ #4, PassEventsUp -> False}],
      MouseAppearanceTag @ "LinkHand"],
      #5, 0.2
    }, "<Tooltip>"],
    False -> #1
  }, Dynamic @ CurrentValue["MouseOver"]]
]];

AssignTemplate["Button-Default", Function[
  TemplateBox[{
    TemplateBox[{#1, Opacity[0], RGBColor[0, 0.7, 0.94], RGBColor[0, 0.7, 0.94], #3}, "<Button-Round>"],
    TemplateBox[{#1, RGBColor[0, 0.7, 0.94], RGBColor[0, 0.7, 0.94], RGBColor[1, 1, 1], #3}, "<Button-Round>"],
    TemplateBox[{#1, RGBColor[0, 0.7, 0.94, 0.3], RGBColor[0, 0.7, 0.94], RGBColor[0, 0.7, 0.94], #3}, "<Button-Round>"],
    #4, #2
  }, "<Button>"]
]];

AssignTemplate["Button-no-Tooltip", Function[
  PaneSelectorBox[{
    True -> TagBox[
      TagBox[
        PaneSelectorBox[{
          True -> #2,
          False -> #3
        }, Dynamic @ CurrentValue["MouseButtonTest"]],
      EventHandlerTag @ {"MouseClicked" :> ReleaseHold @ #4, PassEventsUp -> False}],
    MouseAppearanceTag @ "LinkHand"],
    False -> #1
  }, Dynamic @ CurrentValue["MouseOver"]]
]];

AssignTemplate["Button-r-Template", Function[
  GraphicsBox[{
    JoinForm["Round"], CapForm["Round"], #2,
    DiskBox[{0, 0}, 0.96],
    Thickness[0.04], Opacity[1], #3,
    CircleBox[{0, 0}, 0.96],
    Opacity[1], #4, #1
  }, ImageSize -> #5]
]];

ButtonRoundData = <|
  "Play" -> GraphicsGroupBox[{Thickness[0.08],
    PolygonBox[{{-0.2, -0.4}, {-0.2, 0.4}, {0.4, 0}}],
    LineBox[{{-0.2, -0.4}, {-0.2, 0.4}, {0.4, 0}, {-0.2, -0.4}}]
  }],
  "Return" -> GraphicsGroupBox[{Thickness[0.1],
    LineBox[{{-0.4, 0}, {0.4, 0}}],
    LineBox[{{0, -0.4}, {-0.4, 0}, {0, 0.4}}]
  }],
  "Enter" -> GraphicsGroupBox[{Thickness[0.1],
    LineBox[{{-0.4, 0}, {0.4, 0}}],
    LineBox[{{0, -0.4}, {0.4, 0}, {0, 0.4}}]
  }],
  "Exit" -> GraphicsGroupBox[{Thickness[0.08],
    LineBox[{{0.24, -0.24}, {0.24, -0.4}, {-0.36, -0.4}, {-0.36, 0.4}, {0.24, 0.4}, {0.24, 0.24}}],
    Thickness[0.06],
    LineBox[{{0, 0}, {0.52, 0}}],
    LineBox[{{0.4, 0.12}, {0.52, 0}, {0.4, -0.12}}]
  }],
  "About" -> GraphicsGroupBox[{Thickness[0.1], PointSize[0.1],
    LineBox[{{0, -0.44}, {0, 0.1}}],
    PointBox[{0, 0.44}]
  }],
  "Settings" -> GraphicsGroupBox[Evaluate @ {Thickness[0.12],
    CircleBox[{0, 0}, 0.3],
    GeometricTransformationBox[
      RectangleBox[{-0.15, 0.3}, {0.15, 0.53}, RoundingRadius -> {0.05, 0.05}],
      Evaluate @ Table[RotationMatrix[theta], {theta, 0, 5/3 Pi, 1/3 Pi}]
    ]
  }],
  "Tick" -> GraphicsGroupBox[{Thickness[0.12],
    LineBox[{{-0.4, -0.04}, {-0.12, -0.32}, {0.44, 0.24}}]
  }],
  "Cross" -> GraphicsGroupBox[{Thickness[0.12],
    LineBox[{{-0.3, -0.3}, {0.3, 0.3}}],
    LineBox[{{-0.3, 0.3}, {0.3, -0.3}}]
  }]
|>;

AssignTemplate["Button-Round", With[
  {rule = Map[
    Function[{name}, name -> TemplateBox[
      {ButtonRoundData[name], Slot[2], Slot[3], Slot[4], Slot[5]},
      "<Button-r-Template>"
    ]],
    Keys @ ButtonRoundData
  ]},
  PaneSelectorBox[rule, #1]&
]];

AssignTemplate["Pane", Function[
  FrameBox[
    PaneBox[
      StyleBox[#1, FontFamily -> "Calibri", FontSize -> 16],
      Scrollbars -> False,
      Alignment -> {Center, Center},
      ImageMargins -> {{2, 2}, {2, 2}},
      ImageSize -> {#3, Automatic}
    ],
    Background -> #2,
    RoundingRadius -> {8, 8},
    ContentPadding -> True,
    FrameStyle -> None
  ]
]];

AssignTemplate["Message", Function[
  FrameBox[
    AdjustmentBox[
      RowBox[{
         StyleBox[#2, FontSize -> 18],
         TemplateBox[{4}, "Spacer1"],
         StyleBox[#1, FontFamily -> "Calibri", FontSize -> 16]
      }],
      BoxBaselineShift -> 0,
      BoxMargins -> {{2, 2}, {2, 2}}
    ],
    Background -> #3,
    RoundingRadius -> {8, 8},
    ContentPadding -> True,
    FrameStyle -> None
  ]
]];

AssignTemplate["Setter", Function[
  TemplateBox[{
    PaneSelectorBox[{
      True -> #5,
      False -> PaneSelectorBox[{
        True -> TagBox[
          PaneSelectorBox[
            {True -> #6, False -> #7},
            Dynamic @ CurrentValue["MouseButtonTest"]
          ],
        EventHandlerTag @ {"MouseClicked" :> (#1 = #2)}],
      False -> #4
      }, Dynamic @ CurrentValue["MouseOver"]]
    }, Dynamic[#1] === #2],
    #3, 0.2
  }, "<Tooltip>"]
]];

AssignTemplate["List", Function[
  PaneSelectorBox[{
    True -> TemplateBox[{#2, #3, 0.2}, "<Tooltip>"],
    False -> #1
  }, Dynamic @ CurrentValue["MouseOver"]]
]];

AssignTemplate["Item", Function[
  FrameBox[
    PaneBox[
      StyleBox[#1, FontColor -> #2],
      Scrollbars -> False,
      Alignment -> {Center, Center},
      ImageMargins -> {{0, 0}, {0, 0}},
      ImageSize -> {#5, #6}
    ],
    Background -> #3,
    RoundingRadius -> 4,
    ContentPadding -> False,
    FrameStyle -> Directive[Thickness[1], #4]
  ]
]];

End[];

tmButton = Sequence[
  Include["Tooltip"],
  Include["Button"],
  Include["Button-Round"],
  Include["Button-r-Template"],
  Include["Button-no-Tooltip"],
  Include["Button-Default"]
];

tmList = Sequence[
  Include["Setter"],
  Include["Item"],
  Include["List"]
];

EndPackage[];


DumpSave[$LocalPath <> "library/Package/StyleSheet.mx", {"Thulium`StyleSheet`"}];


(* ::Input:: *)
(*ClearAll["Thulium`StyleSheet`*"]*)
(*ClearAll["Thulium`StyleSheet`*`*"]*)


(* ::Input:: *)
(*SetterBox//Options*)


(* ::Input:: *)
(*CurrentValue[{StyleDefinitions,"Setter"}]*)


(* ::Input:: *)
(*Names["Thulium`StyleSheet`*"]*)
