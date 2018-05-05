(* ::Package:: *)

BeginPackage["Thulium`Template`"];

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
        BoxMargins -> {{0.4, 0.4}, {0.2, 0.4}}
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
          PaneSelectorBox[
            {True -> #2, False -> #3},
            Dynamic @ CurrentValue["MouseButtonTest"]
          ],
        EventHandlerTag @ {"MouseClicked" :> ReleaseHold @ #4}],
      MouseAppearanceTag @ "LinkHand"],
      #5, 0.2
    }, "<Tooltip>"],
    False -> #1
  }, Dynamic @ CurrentValue["MouseOver"]]
]];

End[];

EndPackage[];
