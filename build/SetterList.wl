(* ::Package:: *)

BeginPackage["Thulium`SetterList`", {
  "Thulium`Graphics`"
}];

SetterList::usage = "SetterList displays a list of items which can be selected.";

Begin["`Private`"];

ListItemColor = <|
  "Current" -> <|
    "Background" -> RGBColor[0.96, 0.94, 1],
    "Margin" -> RGBColor[0.9, 0.85, 1],
    "Body" -> RGBColor[0, 0, 0, 1]
  |>,
  "Basic" -> <|
    "Background" -> RGBColor[1, 1, 1, 0],
    "Margin" -> RGBColor[1, 1, 1, 0],
    "Body" -> RGBColor[0, 0, 0, 1]
  |>,
  "Clicked" -> <|
    "Background" -> RGBColor[0.1, 0.5, 0.8],
    "Margin" -> RGBColor[0.1, 0.5, 0.8],
    "Body" -> RGBColor[0, 0, 0, 1]
  |>,
  "Mouseover" -> <|
    "Background" -> RGBColor[0.98, 0.97, 1],
    "Margin" -> RGBColor[0.97, 0.96, 1],
    "Body" -> RGBColor[0, 0, 0, 1]
  |>,
  "Disabled" -> <|
    "Background" -> RGBColor[1, 1, 1],
    "Margin" -> RGBColor[0.8, 0.8, 0.8],
    "Body" -> RGBColor[0.7, 0.7, 0.7]
  |>
|>;

ListItemDisplay[content_, style_, position_] := If[style == "Default",
  Mouseover[
    ListItemDisplay[content, "Basic", position],
    ListItemDisplay[content, "Mouseover", position]
  ],
  With[
    {
      scheme = ListItemColor[style],
      top = -50 * position,
      radius = 8
    },
    GraphicsGroup[{
      scheme["Background"],
      Rectangle[{-600, top}, {600, top + 48}, RoundingRadius -> radius],
      scheme["Margin"], AbsoluteThickness[1], CapForm["Round"],
      Line[{{-600 + radius, top}, {600 - radius, top}}],
      Line[{{-600 + radius, top + 48}, {600 - radius, top + 48}}],
      Line[{{-600, top + radius}, {-600, top + 48 - radius}}],
      Line[{{600, top + radius}, {600, top + 48 - radius}}],
      Circle[{-600 + radius, top + radius}, radius, {Pi, 3/2 Pi}],
      Circle[{-600 + radius, top + 48 - radius}, radius, {1/2 Pi, Pi}],
      Circle[{600 - radius, top + radius}, radius, {-1/2 Pi, 0}],
      Circle[{600 - radius, top + 48 - radius}, radius, {0, 1/2 Pi}],
      scheme["Body"],
      Inset[content, {0, top + 24}, Center]
    }]
  ]
];

SetterList[Dynamic[sel_], data_] := 
  Graphics[{Array[With[{item = data[[#]]},
    Dynamic @ If[sel === #,
      ListItemDisplay[item, "Current", #],
      Module[{style = "Default"},
        EventHandler[Dynamic @ ListItemDisplay[item, style, #], {
          "MouseDown" :> (style = "Clicked"),
          "MouseUp" :> (style = "Default"; sel = #)
        }]
      ]
    ]
  ]&, Length @ data]}, ImageSize -> Full];

End[];

EndPackage[];



DeclarePackage["Thulium`SetterList`", {"SetterList"}];


DumpSave[$LocalPath <> "library/Package/SetterList.mx", "Thulium`SetterList`"];


(* ::Input:: *)
(*Pane[SetterList[Dynamic[sel],{Style["1111111111111",36],Style["1111111111111",36],Style["1111111111111",36]}],BaseStyle->Background->White]*)
