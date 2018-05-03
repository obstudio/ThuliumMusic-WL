(* ::Package:: *)

BeginPackage["Thulium`SetterList`", {"Thulium`Graphics`"}];

SetterList::usage = "SetterList displays a list of items which can be selected.";

Begin["`Private`"];

ListItemColor = <|
  "Current" -> <|
    "Grounding" -> RGBColor[0.97, 0.94, 1],
    "Margin" -> RGBColor[0.8, 0.6, 1],
    "Body" -> RGBColor[0, 0, 0]
  |>,
  "Basic" -> <|
    "Grounding" -> RGBColor[1, 1, 1],
    "Margin" -> RGBColor[1, 1, 1, 0],
    "Body" -> RGBColor[0, 0, 0]
  |>,
  "Clicked" -> <|
    "Grounding" -> RGBColor[0.1, 0.5, 0.8],
    "Margin" -> RGBColor[0.1, 0.5, 0.8],
    "Body" -> RGBColor[0, 0, 0]
  |>,
  "Mouseover" -> <|
    "Grounding" -> RGBColor[0.98, 0.96, 1],
    "Margin" -> RGBColor[0.99, 0.97, 1],
    "Body" -> RGBColor[0, 0, 0]
  |>,
  "Disabled" -> <|
    "Grounding" -> RGBColor[1, 1, 1],
    "Margin" -> RGBColor[0.8, 0.8, 0.8],
    "Body" -> RGBColor[0.7, 0.7, 0.7]
  |>
|>;

ListItemDisplay[content_, style_] := If[style == "Default",
  Mouseover[
    ListItemDisplay[content, "Basic"],
    ListItemDisplay[content, "Mouseover"]
  ],
  With[{scheme = ListItemColor[style]}, 
    Framed[content, 
      FrameStyle -> {Thickness[1], scheme["Margin"]},
      Background -> scheme["Grounding"],
      RoundingRadius -> {4, 4},
      ContentPadding -> False
    ]
  ]
];

SetterList[Dynamic[sel_], data_] := Row[{Column[
  Array[If[# <= Length @ data,
    With[{item = data[[#]]},
      Dynamic @ If[sel === #,
        ListItemDisplay[item, "Current"],
        Module[{style = "Default"},
          EventHandler[Dynamic @ ListItemDisplay[item, style], {
            "MouseDown" :> (style = "Clicked"),
            "MouseUp" :> (style = "Default"; sel = #)
          }]
        ]
      ]
    ],
    Framed[
      Row[{""}, ImageSize -> {800, 20}, Background -> None],
      FrameStyle -> {Thickness[1], RGBColor[0, 0, 0, 0]},
      ContentPadding -> False
    ]
  ]&, 16],
  Spacings -> 0
]}, Alignment -> Center, ImageSize -> {900, 520}];

End[];

EndPackage[];



DeclarePackage["Thulium`SetterList`", {"SetterList"}];


DumpSave[$LocalPath <> "/library/Paclet/SetterList.mx", "Thulium`SetterList`"];


(* ::Input:: *)
(*tmpsel = 1;*)


(* ::Input:: *)
(*Pane[SetterList[Dynamic@tmpsel, {StringRepeat["123",10],StringRepeat["345",10],StringRepeat["567",10]}],BaseStyle->Background->White]*)
