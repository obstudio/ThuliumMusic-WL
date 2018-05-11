(* ::Package:: *)

BeginPackage["Thulium`PageSelector`", {"Thulium`Graphics`"}];

PageSelector::usage = "PageSelector displays a page selector.";

Begin["`Private`"];

PageSelectorData = <|
  "Prev" -> GraphicsGroup[{
    Thickness[0.08], CapForm["Round"], JoinForm["Round"],
    Line[{{0.32, 0.48}, {-0.36, 0}, {0.32, -0.48}}]
  }],
  "Next" -> GraphicsGroup[{
    Thickness[0.08], CapForm["Round"], JoinForm["Round"],
    Line[{{-0.32, 0.48}, {0.36, 0}, {-0.32, -0.48}}]
  }],
  "First" -> GraphicsGroup[{
    Thickness[0.08], CapForm["Round"], JoinForm["Round"],
    Line[{{-0.4, -0.48}, {-0.4, 0.48}}],
    Line[{{0.44, 0.48}, {-0.16, 0}, {0.44, -0.48}}]
  }],
  "Last" -> GraphicsGroup[{
    Thickness[0.08], CapForm["Round"], JoinForm["Round"],
    Line[{{0.4, -0.48}, {0.4, 0.48}}],
    Line[{{-0.44, 0.48}, {0.16, 0}, {-0.44, -0.48}}]
  }]
|>;

PageSelectorColor = <|
  "Current" -> <|
    "Grounding" -> RGBColor[0.1, 0.5, 0.8],
    "Margin" -> RGBColor[0.1, 0.5, 0.8],
    "Body" -> RGBColor[1, 1, 1]
  |>,
  "Basic" -> <|
    "Grounding" -> RGBColor[1, 1, 1],
    "Margin" -> RGBColor[0.1, 0.5, 0.8],
    "Body" -> RGBColor[0.1, 0.5, 0.8]
  |>,
  "Clicked" -> <|
    "Grounding" -> RGBColor[0.1, 0.5, 0.8],
    "Margin" -> RGBColor[0.1, 0.5, 0.8],
    "Body" -> RGBColor[1, 1, 1]
  |>,
  "Mouseover" -> <|
    "Grounding" -> RGBColor[0.6, 0.8, 1],
    "Margin" -> RGBColor[0.1, 0.5, 0.8],
    "Body" -> RGBColor[0.1, 0.5, 0.8]
  |>,
  "Disabled" -> <|
    "Grounding" -> RGBColor[1, 1, 1],
    "Margin" -> RGBColor[0.8, 0.8, 0.8],
    "Body" -> RGBColor[0.8, 0.8, 0.8]
  |>
|>;

PageSelectorNumber[num_Integer, style_, size_] := If[style == "Default",
  Mouseover[
    PageSelectorNumber[num, "Basic", size],
    PageSelectorNumber[num, "Mouseover", size]
  ],
  With[{scheme = PageSelectorColor[style]}, Graphics[{
    squareRounded[0.06, 0.3, scheme],
    Text[num, BaseStyle -> {
      FontWeight -> If[style === "Current", Bold, Plain],
      FontSize -> size,
      FontColor -> scheme["Body"]
    }]
  }, ContentSelectable -> False]]
];

PageSelectorButton[name_String, style_] := If[style == "Default",
  Mouseover[
    PageSelectorButton[name, "Basic"], 
    PageSelectorButton[name, "Mouseover"]
  ],
  With[{scheme = PageSelectorColor[[style]]}, Graphics[{
    squareRounded[0.06, 0.3, scheme],
    scheme["Body"],
    PageSelectorData[name]
  }, ContentSelectable -> False]]
];

PageSelector[Dynamic[page_], pageCount_] := Block[{},
  Row[{
    Dynamic @ If[page <= 1,
      PageSelectorButton["Prev", "Disabled"],
      Module[{style = "Default"},
        EventHandler[Dynamic @ PageSelectorButton["Prev", style], {
          "MouseDown" :> (style = "Clicked"),
          "MouseUp" :> (style = "Default"; page--)
        }]
      ]
    ],
    Spacer[20],
    Row[Flatten @ Array[{
      Dynamic @ If[page == #,
        PageSelectorNumber[#, "Current", 32],
        Module[{style = "Default"},
          EventHandler[Dynamic @ PageSelectorNumber[#, style, 32], {
            "MouseDown" :> (style = "Clicked"),
            "MouseUp" :> (style = "Default"; page = #)
          }]
        ]
      ], Spacer[6]}&, pageCount]
    ],
	Spacer[14],
    Dynamic @ If[page >= pageCount,
      PageSelectorButton["Next", "Disabled"],
      Module[{style = "Default"},
        EventHandler[Dynamic @ PageSelectorButton["Next", style], {
          "MouseDown" :> (style = "Clicked"),
          "MouseUp" :> (style = "Default"; page++)
        }]
      ]
    ]
  }, ImageSize -> {500, 60}, Alignment -> Center]
];

End[];

EndPackage[];



DeclarePackage["Thulium`PageSelector`", {"PageSelector"}];


DumpSave[$LocalPath <> "library/Package/PageSelector.mx", "Thulium`PageSelector`"];
