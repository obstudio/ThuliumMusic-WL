(* ::Package:: *)

BeginPackage["Thulium`SetterList`", {"Thulium`Graphics`"}];

SetterList::usage = "SetterList displays a list of items which can be selected.";

Begin["`Private`"];

ListItemColor = <|
  "Current" -> <|
    "Grounding" -> RGBColor[0.96, 0.94, 1],
    "Margin" -> RGBColor[0.9, 0.85, 1],
    "Body" -> RGBColor[0, 0, 0, 1]
  |>,
  "Basic" -> <|
    "Grounding" -> RGBColor[1, 1, 1, 0],
    "Margin" -> RGBColor[1, 1, 1, 0],
    "Body" -> RGBColor[0, 0, 0, 1]
  |>,
  "Clicked" -> <|
    "Grounding" -> RGBColor[0.1, 0.5, 0.8],
    "Margin" -> RGBColor[0.1, 0.5, 0.8],
    "Body" -> RGBColor[0, 0, 0, 1]
  |>,
  "Mouseover" -> <|
    "Grounding" -> RGBColor[0.98, 0.97, 1],
    "Margin" -> RGBColor[0.97, 0.96, 1],
    "Body" -> RGBColor[0, 0, 0, 1]
  |>,
  "Disabled" -> <|
    "Grounding" -> RGBColor[1, 1, 1],
    "Margin" -> RGBColor[0.8, 0.8, 0.8],
    "Body" -> RGBColor[0.7, 0.7, 0.7]
  |>
|>;

ListItemDisplay[content_, style_, position_] := If[style == "Default",
  Mouseover[
    ListItemDisplay[content, "Basic", position],
    ListItemDisplay[content, "Mouseover", position]
  ],
  With[{scheme = ListItemColor[style]}, GraphicsGroup[{
    scheme["Grounding"],
    Rectangle[{-600, -48 * position}, {600, -48 * (position - 1)}],
    scheme["Margin"], AbsoluteThickness[1], CapForm["Round"],
    Line[{{-600, -48 * position}, {600, -48 * (position - 1)}}],
    scheme["Body"],
    Inset[content, {0, -48 * position + 24}, Center]
  }]]
];

SetterList[Dynamic[sel_], data_] := Row[{
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
  ]&, Length @ data]}, ImageSize -> Full]
}, Alignment -> Center, ImageSize -> {960, 520}];

End[];

EndPackage[];



DeclarePackage["Thulium`SetterList`", {"SetterList"}];


DumpSave[$LocalPath <> "/library/Paclet/SetterList.mx", "Thulium`SetterList`"];


(* ::Input:: *)
(*Thulium`SetterList`Private`ListItemDisplay[111111,"Default",0]//Graphics*)


(* ::Input:: *)
(*tmpsel = 1;*)


(* ::Input:: *)
(*Pane[SetterList[Dynamic@tmpsel, {Row[{StringRepeat["123",10]}],StringRepeat["345",10],StringRepeat["567",10]}],BaseStyle->Background->White]*)
