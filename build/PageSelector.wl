(* ::Package:: *)

ClearAll["Thulium`PageSel`*"];
ClearAll["Thulium`PageSel`*`*"];


BeginPackage["Thulium`PageSel`"];

tmPageSelBox1::usage = "Thulium Page Seletor Box 1";
tmPageSelBox2::usage = "Thulium Page Seletor Box 2";

tmPageSel::usage = "Thulium Page Selector Style Sheet";

Begin["`Private`"];

tmPageSelData = <|
  "Prev" -> GraphicsGroupBox[{Thickness[0.08],
    LineBox[{{0.32, 0.48}, {-0.36, 0}, {0.32, -0.48}}]
  }],
  "Next" -> GraphicsGroupBox[{Thickness[0.08],
    LineBox[{{-0.32, 0.48}, {0.36, 0}, {-0.32, -0.48}}]
  }],
  "First" -> GraphicsGroupBox[{Thickness[0.08],
    LineBox[{{-0.4, -0.48}, {-0.4, 0.48}}],
    LineBox[{{0.44, 0.48}, {-0.16, 0}, {0.44, -0.48}}]
  }],
  "Last" -> GraphicsGroupBox[{Thickness[0.08],
    LineBox[{{0.4, -0.48}, {0.4, 0.48}}],
    LineBox[{{-0.44, 0.48}, {0.16, 0}, {-0.44, -0.48}}]
  }]
|>;

tmPageSel = Sequence[
  Cell[StyleData["<PageSel-Template>"], TemplateBoxOptions -> {
    DisplayFunction -> With[{t = 0.3, r = 0.06}, Function[
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
    ]]
  }],
  
  Cell[StyleData["<PageSel-Graphic-Display>"], TemplateBoxOptions -> {
    DisplayFunction -> With[
      {rule = Map[
        Function[{name}, name -> TemplateBox[
          {tmPageSelData[name], Slot[3], Slot[4], Slot[5], Slot[2]},
          "<PageSel-Template>"
        ]],
        Keys @ tmPageSelData
      ]},
      PaneSelectorBox[rule, #1]&
    ]
  }],

  Cell[StyleData["<PageSel-Graphic>"], TemplateBoxOptions -> {
    DisplayFunction -> With[{size = 28}, Function[
      PaneSelectorBox[{
        True -> TemplateBox[{#1, size,
          Opacity[0], RGBColor[0.8, 0.8, 0.8], RGBColor[0.8, 0.8, 0.8]
        }, "<PageSel-Graphic-Display>"],
        False -> TemplateBox[{
          TemplateBox[{#1, size,
            RGBColor[1, 1, 1], RGBColor[0.1, 0.5, 0.8], RGBColor[0.1, 0.5, 0.8]
          }, "<PageSel-Graphic-Display>"],
          TemplateBox[{#1, size,
            RGBColor[0.1, 0.5, 0.8], RGBColor[0.1, 0.5, 0.8], RGBColor[1, 1, 1]
          }, "<PageSel-Graphic-Display>"],
          TemplateBox[{#1, size,
            RGBColor[0.6, 0.8, 1], RGBColor[0.1, 0.5, 0.8], RGBColor[0.1, 0.5, 0.8]
          }, "<PageSel-Graphic-Display>"],
          #4[#2]
        }, "<Button-no-Tooltip>"]
      }, Dynamic[#2] === #3]
    ]]
  }],

  Cell[StyleData["<PageSel-Numeric-Display>"], TemplateBoxOptions -> {
    DisplayFunction -> Function[
      TemplateBox[{
        InsetBox[
          StyleBox[#1,
            FontSize -> 16,
            FontColor -> #5
          ],
          {0, -0.04}, Center
        ],
        #3, #4, #5, #2
      }, "<PageSel-Template>"]
    ]
  }],

  Cell[StyleData["<PageSel-Numeric>"], TemplateBoxOptions -> {
    DisplayFunction -> With[{size = 28}, Function[
      PaneSelectorBox[{
        True -> TemplateBox[{#1, size,
          RGBColor[0.1, 0.5, 0.8], RGBColor[0.1, 0.5, 0.8], RGBColor[1, 1, 1]
        }, "<PageSel-Numeric-Display>"],
        False -> TemplateBox[{
          TemplateBox[{#1, size,
            RGBColor[1, 1, 1], RGBColor[0.1, 0.5, 0.8], RGBColor[0.1, 0.5, 0.8]
          }, "<PageSel-Numeric-Display>"],
          TemplateBox[{#1, size,
            RGBColor[0.1, 0.5, 0.8], RGBColor[0.1, 0.5, 0.8], RGBColor[1, 1, 1]
          }, "<PageSel-Numeric-Display>"],
          TemplateBox[{#1, size,
            RGBColor[0.6, 0.8, 1], RGBColor[0.1, 0.5, 0.8], RGBColor[0.1, 0.5, 0.8]
          }, "<PageSel-Numeric-Display>"],
          #2 = #1
        }, "<Button-no-Tooltip>"]
      }, Dynamic[#2] === #1]
    ]]
  }]
];

PageSelSpacerBox = TemplateBox[{0.2}, "Spacer1"];

SetAttributes[tmPageSelBox1, HoldFirst];
tmPageSelBox1[page_, pageCount_] := RowBox[{
  TemplateBox[{"Prev", Unevaluated[page], 1, AddTo[#, -1]&}, "<PageSel-Graphic>"],
  TemplateBox[{2}, "Spacer1"],
  RowBox[Riffle[
    Array[TemplateBox[{#, Unevaluated[page]}, "<PageSel-Numeric>"]&, pageCount],
  PageSelSpacerBox]],
  TemplateBox[{2}, "Spacer1"],
  TemplateBox[{"Next", Unevaluated[page], pageCount, AddTo[#, 1]&}, "<PageSel-Graphic>"]
}];

tmPageSelBox2Numbers[center_, page_] := RowBox @ Riffle[
  TemplateBox[{#, Unevaluated[page]}, "<PageSel-Numeric>"]& /@ Range[center - 3, center + 3],
  PageSelSpacerBox
];

SetAttributes[tmPageSelBox2, HoldFirst];
tmPageSelBox2[page_, pageCount_] := RowBox[{
  TemplateBox[{"First", Unevaluated[page], 1, Set[#, 1]&}, "<PageSel-Graphic>"],
  PageSelSpacerBox,
  TemplateBox[{"Prev", Unevaluated[page], 1, AddTo[#, -1]&}, "<PageSel-Graphic>"],
  TemplateBox[{2}, "Spacer1"],
  PaneSelectorBox[Join[
    # -> tmPageSelBox2Numbers[4, Unevaluated[page]]& /@ Range[1, 3],
    # -> tmPageSelBox2Numbers[#, Unevaluated[page]]& /@ Range[4, pageCount - 4],
    # -> tmPageSelBox2Numbers[pageCount - 3, Unevaluated[page]]& /@ Range[pageCount - 3, pageCount]
  ], Dynamic[page]],
  TemplateBox[{2}, "Spacer1"],
  TemplateBox[{"Next", Unevaluated[page], pageCount, AddTo[#, 1]&}, "<PageSel-Graphic>"],
  PageSelSpacerBox,
  TemplateBox[{"Last", Unevaluated[page], pageCount, Set[#, pageCount]&}, "<PageSel-Graphic>"]
}];

End[];

EndPackage[];


DumpSave[$LocalPath <> "library/Package/PageSel.mx", {"Thulium`PageSel`"}];
