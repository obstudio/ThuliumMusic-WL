(* ::Package:: *)

ClearAll["Thulium`StyleSheet`*"];
ClearAll["Thulium`StyleSheet`*`*"];


BeginPackage["Thulium`StyleSheet`"];

Include = <||>;

tmPageSelBox1::usage = "Thulium Page Seletor Box 1";
tmPageSelBox2::usage = "Thulium Page Seletor Box 2";

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

AssignTemplate["Button-Round", Function[
  PaneSelectorBox[{
    "Play" -> TemplateBox[{
      GraphicsGroupBox[{Thickness[0.08],
        PolygonBox[{{-0.2, -0.4}, {-0.2, 0.4}, {0.4, 0}}],
        LineBox[{{-0.2, -0.4}, {-0.2, 0.4}, {0.4, 0}, {-0.2, -0.4}}]
      }],
      #2, #3, #4, #5
    }, "<Button-r-Template>"],
    "Return" -> TemplateBox[{
      GraphicsGroupBox[{Thickness[0.1],
        LineBox[{{-0.4, 0}, {0.4, 0}}],
        LineBox[{{0, -0.4}, {-0.4, 0}, {0, 0.4}}]
      }],
      #2, #3, #4, #5
    }, "<Button-r-Template>"]
  }, #1]
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
  PaneSelectorBox[{
    True -> #5,
    False -> PaneSelectorBox[{
      True -> TemplateBox[{
        TagBox[
          PaneSelectorBox[
            {True -> #6, False -> #7},
            Dynamic @ CurrentValue["MouseButtonTest"]
          ],
        EventHandlerTag @ {"MouseClicked" :> (#1 = #2)}],
        #3, 0.2
      }, "<Tooltip>"],
      False -> #4
    }, Dynamic @ CurrentValue["MouseOver"]]
  }, Dynamic[#1] === #2]
]];

AssignTemplate["Setter-Item", Function[
  FrameBox[
    PaneBox[
      StyleBox[#1,
        FontFamily -> "Calibri",
        FontSize -> 16,
        FontColor -> #2
      ],
      Scrollbars -> False,
      Alignment -> {Center, Center},
      ImageMargins -> {{0, 0}, {0, 0}},
      ImageSize -> {#5, #6}
    ],
    Background -> #3,
    RoundingRadius -> {8, 8},
    ContentPadding -> False,
    FrameStyle -> Directive[Thickness[1], #4]
  ]
]];

AssignTemplate["PageSel-Template", With[{t = 0.3, r = 0.06}, Function[
  GraphicsBox[{
    Thickness[r], CapForm["Round"], #2,
    RectangleBox[{r - 1, r - 1}, {1 - r, 1 - r}, RoundingRadius -> {t - r, t - r}],
    Opacity[1], #3,
    CircleBox[{t - 1, t - 1}, t - r, {Pi, 3/2 Pi}], CircleBox[{1 - t, 1 - t}, t - r, {0, 1/2 Pi}],
    CircleBox[{1 - t, t - 1}, t - r, {-1/2 Pi, 0}], CircleBox[{t - 1, 1 - t}, t - r, {1/2 Pi, Pi}],
    LineBox[{{t - 1, r - 1}, {1 - t, r - 1}}], LineBox[{{t - 1, 1 - r}, {1 - t, 1 - r}}],
    LineBox[{{r - 1, t - 1}, {r - 1, 1 - t}}], LineBox[{{1 - r, t - 1}, {1 - r, 1 - t}}],
    Opacity[1], #4, #1
  }, ImageSize -> #5, ImageMargins -> 0]
]]];

AssignTemplate["PageSel-Data", Function[
  PaneSelectorBox[{
    "Prev" -> TemplateBox[{
      GraphicsGroupBox[{Thickness[0.08],
        LineBox[{{0.32, 0.48}, {-0.36, 0}, {0.32, -0.48}}]
      }],
      #2, #3, #4, #5
    }, "<PageSel-Template>"],
    "Next" -> TemplateBox[{
      GraphicsGroupBox[{Thickness[0.08],
        LineBox[{{-0.32, 0.48}, {0.36, 0}, {-0.32, -0.48}}]
      }],
      #2, #3, #4, #5
    }, "<PageSel-Template>"],
    "First" -> TemplateBox[{
      GraphicsGroupBox[{Thickness[0.08],
        LineBox[{{-0.4, -0.48}, {-0.4, 0.48}}],
        LineBox[{{0.44, 0.48}, {-0.16, 0}, {0.44, -0.48}}]
      }],
      #2, #3, #4, #5
    }, "<PageSel-Template>"],
    "Last" -> TemplateBox[{
      GraphicsGroupBox[{Thickness[0.08],
        LineBox[{{0.4, -0.48}, {0.4, 0.48}}],
        LineBox[{{-0.44, 0.48}, {0.16, 0}, {-0.44, -0.48}}]
      }],
      #2, #3, #4, #5
    }, "<PageSel-Template>"]
  }, #1, TemplateBox[{
    InsetBox[
      StyleBox[#1,
        FontSize -> 16,
        FontColor -> #4
      ],
      {0, 0}, Center
    ],
    #2, #3, #4, #5
  }, "<PageSel-Template>"]]
]];

AssignTemplate["PageSel-g-Button", Function[
  PaneSelectorBox[{
    True -> TemplateBox[{#1, Opacity[0], RGBColor[0.8, 0.8, 0.8], RGBColor[0.8, 0.8, 0.8], 28}, "<PageSel-Data>"],
    False -> TemplateBox[{
      TemplateBox[{#1, RGBColor[1, 1, 1], RGBColor[0.1, 0.5, 0.8], RGBColor[0.1, 0.5, 0.8], 28}, "<PageSel-Data>"],
      TemplateBox[{#1, RGBColor[0.1, 0.5, 0.8], RGBColor[0.1, 0.5, 0.8], RGBColor[1, 1, 1], 28}, "<PageSel-Data>"],
      TemplateBox[{#1, RGBColor[0.6, 0.8, 1], RGBColor[0.1, 0.5, 0.8], RGBColor[0.1, 0.5, 0.8], 28}, "<PageSel-Data>"],
      #4[#2]
    }, "<Button-no-Tooltip>"]
  }, Dynamic[#2] === #3]
]];

AssignTemplate["PageSel-n-Button", Function[
  PaneSelectorBox[{
    True -> TemplateBox[{#1, RGBColor[0.1, 0.5, 0.8], RGBColor[0.1, 0.5, 0.8], RGBColor[1, 1, 1], 28}, "<PageSel-Data>"],
    False -> TemplateBox[{
      TemplateBox[{#1, RGBColor[1, 1, 1], RGBColor[0.1, 0.5, 0.8], RGBColor[0.1, 0.5, 0.8], 28}, "<PageSel-Data>"],
      TemplateBox[{#1, RGBColor[0.1, 0.5, 0.8], RGBColor[0.1, 0.5, 0.8], RGBColor[1, 1, 1], 28}, "<PageSel-Data>"],
      TemplateBox[{#1, RGBColor[0.6, 0.8, 1], RGBColor[0.1, 0.5, 0.8], RGBColor[0.1, 0.5, 0.8], 28}, "<PageSel-Data>"],
      #2 = #1
    }, "<Button-no-Tooltip>"]
  }, Dynamic[#2] === #1]
]];

End[];

Begin["`PageSel`"];

PageSelSpacer = TemplateBox[{0.4}, "Spacer1"];

SetAttributes[tmPageSelBox1, HoldFirst];
tmPageSelBox1[page_, pageCount_] := RowBox[{
  TemplateBox[{"Prev", Unevaluated[page], 1, AddTo[#, -1]&}, "<PageSel-g-Button>"],
  TemplateBox[{2}, "Spacer1"],
  RowBox[Riffle[
    Array[TemplateBox[{#, Unevaluated[page]}, "<PageSel-n-Button>"]&, pageCount],
  PageSelSpacer]],
  TemplateBox[{2}, "Spacer1"],
  TemplateBox[{"Next", Unevaluated[page], pageCount, AddTo[#, 1]&}, "<PageSel-g-Button>"]
}];

SetAttributes[tmPageSelBox2Numbers, HoldRest];
tmPageSelBox2Numbers[center_, page_] := RowBox[Riffle[
  TemplateBox[{#, Unevaluated[page]}, "<PageSel-n-Button>"]& /@ Range[center - 3, center + 3],
PageSelSpacer]];

SetAttributes[tmPageSelBox2, HoldFirst];
tmPageSelBox2[page_, pageCount_] := RowBox[{
  TemplateBox[{"First", Unevaluated[page], 1, Set[#, 1]&}, "<PageSel-g-Button>"],
  PageSelSpacer,
  TemplateBox[{"Prev", Unevaluated[page], 1, AddTo[#, -1]&}, "<PageSel-g-Button>"],
  TemplateBox[{2}, "Spacer1"],
  PaneSelectorBox[
    Join[
      # -> tmPageSelBox2Numbers[4, page]& /@ Range[1, 3],
      # -> tmPageSelBox2Numbers[#, page]& /@ Range[4, pageCount - 4],
      # -> tmPageSelBox2Numbers[pageCount - 3, page]& /@ Range[pageCount - 3, pageCount]
    ],
  Dynamic[page]],
  TemplateBox[{2}, "Spacer1"],
  TemplateBox[{"Next", Unevaluated[page], pageCount, AddTo[#, 1]&}, "<PageSel-g-Button>"],
  PageSelSpacer,
  TemplateBox[{"Last", Unevaluated[page], pageCount, Set[#, pageCount]&}, "<PageSel-g-Button>"]
}];

End[];

tmButton = Sequence[
  Include["Tooltip"],
  Include["Button"],
  Include["Button-Round"],
  Include["Button-r-Template"],
  Include["Button-no-Tooltip"]
];

tmPageSel = Sequence[
  Include["PageSel-Data"],
  Include["PageSel-Template"],
  Include["PageSel-n-Button"],
  Include["PageSel-g-Button"]
];

tmSetter = Sequence[
  Include["Setter"],
  Include["Setter-Item"]
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
