(* ::Package:: *)

BeginPackage["Thulium`SmartButton`", {
  "Thulium`Graphics`",
  "Thulium`Assets`"
}];

SmartButton::usage = "a smart display of button.";
SwitchButton::usage = "SwitchButton";

Begin["`Private`"];

SmartButtonData = <|
  "Play" -> GraphicsGroup[{
    Thickness[0.08],JoinForm["Round"],CapForm["Round"],
    Triangle[{{-0.2,-0.4},{-0.2,0.4},{0.4,0}}],
    Line[{{-0.2,-0.4},{-0.2,0.4},{0.4,0},{-0.2,-0.4}}]
  }],
  "Pause" -> GraphicsGroup[{
    Rectangle[{-0.4,-0.4},{-0.08,0.4},RoundingRadius->{0.1,0.1}],
    Rectangle[{0.08,-0.4},{0.4,0.4},RoundingRadius->{0.1,0.1}]
  }],
  "Stop"->GraphicsGroup[{
    Rectangle[{-0.4,-0.4},{0.4,0.4},RoundingRadius->{0.1,0.1}]
  }],
  "Return"->GraphicsGroup[{
    Thickness[0.1],CapForm["Round"],JoinForm["Round"],
    Line[{{-0.4,0},{0.4,0}}],
    Line[{{0,-0.4},{-0.4,0},{0,0.4}}]
  }],
  "ArrowR"->GraphicsGroup[{
    Thickness[0.1],CapForm["Round"],JoinForm["Round"],
    Line[{{-0.4,0},{0.4,0}}],
    Line[{{0,-0.4},{0.4,0},{0,0.4}}]
  }],
  "Settings"->GraphicsGroup[{
    Thickness[0.12],
    Circle[{0,0},0.3],
    Table[Rotate[
      Rectangle[{-0.15,0.3},{0.15,0.53},RoundingRadius->{0.05,0.05}],
    theta,{0,0}],{theta,0,2Pi,Pi/3}]
  }],
  "Add"->GraphicsGroup[{
    Thickness[0.12],CapForm["Round"],
    Line[{{0,-0.4},{0,0.4}}],
    Line[{{-0.4,0},{0.4,0}}]
  }],
  "About"->GraphicsGroup[{
    Thickness[0.1],CapForm["Round"],
    Line[{{0,-0.44},{0,0.1}}],
    PointSize[0.1],
    Point[{0,0.44}]
  }],
  "Exit"->GraphicsGroup[{
    Thickness[0.08],JoinForm["Round"],CapForm["Round"],
    Line[{{0.24,-0.24},{0.24,-0.4},{-0.36,-0.4},{-0.36,0.4},{0.24,0.4},{0.24,0.24}}],
    Thickness[0.06],
    Line[{{0,0},{0.52,0}}],
    Line[{{0.4,0.12},{0.52,0},{0.4,-0.12}}]
  }],
  "Modify"->GraphicsGroup[{
    Thickness[0.06],JoinForm["Round"],CapForm["Round"],
    Line[{{-0.4,-0.4},{-0.36,-0.16},{0.24,0.44},{0.44,0.24},{-0.16,-0.36},{-0.4,-0.4}}],
    Line[{{0.12,-0.4},{0.4,-0.4}}],
    Thickness[0.04],
    Line[{{0.12,0.32},{0.32,0.12}}],
    Line[{{-0.12,-0.32},{-0.32,-0.12}}]
  }],
  "PrevSong"->GraphicsGroup[{
    Thickness[0.06],CapForm["Round"],JoinForm["Round"],
    Line[{{-0.32,-0.36},{-0.32,0.36}}],
    Triangle[{{0.36,-0.36},{0.36,0.36},{-0.16,0}}],
    Line[{{0.36,-0.36},{0.36,0.36},{-0.16,0},{0.36,-0.36}}]
  }],
  "NextSong"->GraphicsGroup[{
    Thickness[0.06],CapForm["Round"],JoinForm["Round"],
    Line[{{0.32,-0.36},{0.32,0.36}}],
    Triangle[{{-0.36,-0.36},{-0.36,0.36},{0.16,0}}],
    Line[{{-0.36,-0.36},{-0.36,0.36},{0.16,0},{-0.36,-0.36}}]
  }],
  "EnterPlaylist-Old"->With[
    {l=0.36,w=0.44,a=0.12,q=0.08,p=-0.16,l1=-0.16,w1=0.24,w2=0.08},
    GraphicsGroup[{
      Thickness[0.06],CapForm["Round"],JoinForm["Round"],
      Line[{{l,p-0.16},{l,-w},{-l,-w},{-l,w},{l-0.2,w},{l,w-0.2},{l,p+0.16}}],
      Thickness[0.04],
      Line[{{q,p},{0.52,p}}],Line[{{q+a,p-a},{q,p},{q+a,p+a}}],
      Line[{{l1,w1},{0.08,w1}}],Line[{{l1,w2},{0.08,w2}}],
      Line[{{l1,-w2},{-0.04,-w2}}],Line[{{l1,-w1},{-0.04,-w1}}]
    }]
  ],
  "Tick"->GraphicsGroup[{
    Thickness[0.12],CapForm["Round"],JoinForm["Round"],
    Line[{{-0.4,-0.04},{-0.12,-0.32},{0.44,0.24}}]
  }],
  "Cross"->GraphicsGroup[{
    Thickness[0.12],CapForm["Round"],JoinForm["Round"],
    Line[{{-0.3,-0.3},{0.3,0.3}}],
    Line[{{-0.3,0.3},{0.3,-0.3}}]
  }],
  "Browse"->GraphicsGroup[{
    Disk[{0,0},0.12],
    Disk[{-0.4,0},0.12],
    Disk[{0.4,0},0.12]
  }],
  "EnterPlaylist"->With[
    {t=Pi/8,s={0.12,0.02},h={0,0.64},i={0,0.04},a={-0.36,-0.36},b={0.28,-0.16}},
    GraphicsGroup[{
      Rotate[Disk[a,{0.16,0.12}],t],
      Rotate[Disk[b,{0.16,0.12}],t],
      Thickness[0.04],CapForm["Round"],JoinForm["Round"],
      Line[{b+h+s,b+s}],
      Line[{a+s,a+h+s}],
      Line[{b+h+s,a+h+s}],
      Line[{b+h+s-i,a+h+s-i}],
      Line[{b+h+s-2i,a+h+s-2i}]
    }]
  ],
  "Search"->With[{c={-0.12,0.12},r=0.32,s=Sqrt[2]/2},GraphicsGroup[{
    Thickness[0.08],CapForm["Round"],JoinForm["Round"],
    Circle[c,r],
    Line[{c+s*r*{1,-1},c+s*(r+0.4)*{1,-1}}]
  }]],
  "SearchList"->With[{cx=0.16,cy=-0.08,r=0.24,h=0.24,s=Sqrt[2]/2},GraphicsGroup[{
    Thickness[0.06],CapForm["Round"],JoinForm["Round"],
    Circle[{cx,cy},r],
    Line[{{cx,cy}+s*r*{1,-1},{cx,cy}+s*(r+0.24)*{1,-1}}],
    Line[{{-0.4,cy-h},{-0.22,cy-h}}],
    Line[{{-0.4,cy},{-0.28,cy}}],
    Line[{{-0.4,cy+h},{-0.22,cy+h}}],
    Line[{{-0.4,cy+2h},{0.32,cy+2h}}]
  }]],
  "SearchDir"->With[{c={0.26,-0.2},r=0.2,s=Sqrt[2]/2},
    GraphicsGroup[{
      Thickness[0.06],CapForm["Round"],JoinForm["Round"],
      Circle[c,r],
      Line[{c+s*r*{1,-1},c+s*(r+0.2)*{1,-1}}],
      Line[{{-0.06,-0.36},{-0.44,-0.36},{-0.44,0.36},{-0.08,0.36},{0,0.24},{0.44,0.24},{0.44,0.12}}]
    }]
  ],
  "Single" -> With[{x = 0.48, y = 0.2, d = 0.2},
    GraphicsGroup[{
      Thickness[0.08], CapForm["Round"], JoinForm["Round"],
      Line[{{-x, y}, {x, y}, {x - d, y + d}}],
      Line[{{x, -y}, {-x, -y}, {d - x, - y - d}}]
    }]
  ],
  "Loop" -> With[{r = 0.48},
    GraphicsGroup[{
      Thickness[0.08], CapForm["Round"], JoinForm["Round"],
      Circle[{0, 0}, r, {0, 7/4 Pi}],
      Line[{{r, 0}, {r - 0.15, 0.1}}],
      Line[{{r, 0}, {r + 0.1, 0.15}}]
    }]
  ],
  "Directory"->With[{l=0.44,w=0.36,dw=0.12,x1=-0.08,x2=0},GraphicsGroup[{
    Thickness[0.06],CapForm["Round"],JoinForm["Round"],
    Line[{{l,w-dw},{l,-w},{-l,-w},{-l,w},{x1,w},{x2,w-dw},{l,w-dw}}]
  }]],
  "Volume"->With[{l=-0.48,m=-0.32,r=0.08,t=0.4,b=0.16},GraphicsGroup[{
    Thickness[0.06],CapForm["Round"],JoinForm["Round"],
    Line[{{l,b},{m,b},{r,t},{r,-t},{m,-b},{l,-b},{l,b}}],
    FilledCurve[Line[{{l,b},{m,b},{r,t},{r,-t},{m,-b},{l,-b},{l,b}}]],
    Circle[{0,0},0.32,{-Pi/6,Pi/6}],
    Circle[{0,0},0.52,{-Pi/6,Pi/6}]
  }]]
|>;

SmartButtonColor = <|
  "Basic" -> <|
    "Grounding" -> RGBColor[1, 1, 1],
    "Margin" -> RGBColor[0, 0.7, 0.94],
    "Body" -> RGBColor[0, 0.7, 0.94]
  |>,
  "Clicked" -> <|
    "Grounding" -> RGBColor[0, 0.7, 0.94],
    "Margin" -> RGBColor[0, 0.7, 0.94],
    "Body" -> RGBColor[1, 1, 1]
  |>,
  "Mouseover" -> <|
    "Grounding" -> RGBColor[0.7, 0.9, 1],
    "Margin" -> RGBColor[0, 0.7, 0.94],
    "Body" -> RGBColor[0, 0.7, 0.94]
  |>,
  "Disabled" -> <|
    "Grounding" -> RGBColor[1, 1, 1],
    "Margin" -> RGBColor[0.6, 0.6, 0.6],
    "Body" -> RGBColor[0.8, 0.8, 0.8]
  |>
|>;

SmartButtonDisplay[name_, style_] := If[style == "Default",
  Mouseover[
    SmartButtonDisplay[name, "Basic"],
    SmartButtonDisplay[name, "Mouseover"]
  ],
  With[{scheme = SmartButtonColor[style]}, 
    Graphics[{
      squareRounded[0.06, 1, scheme],
      scheme[["Body"]], 
      SmartButtonData[name]
    }]
  ]
];

SetAttributes[SmartButton, HoldRest];
SmartButton[name_String, action_] := SmartButton[name, name, action];
SmartButton[name_String, info_String, action_] := DynamicModule[{style = "Default"},
  EventHandler[Dynamic @ TooltipDisplay[
    SmartButtonDisplay[name, style],
    TextDict[info]
  ], {
    "MouseDown" :> (style = "Clicked"),
    "MouseUp" :> (style = "Default"; action)
  }]
];

SwitchButton[source_, items__] := DynamicModule[{style = "Default"},
  PaneSelector[
    #[[1]] -> EventHandler[Dynamic @ TooltipDisplay[
      SmartButtonDisplay[#[[2]], style],
      TextDict[#[[2]]]
    ], {
      "MouseDown" :> (style = "Clicked"),
      "MouseUp" :> (style = "Default"; ReleaseHold @ #[[3]])
    }]& /@ {items},
    source
  ]
];

End[];

EndPackage[];

DeclarePackage["Thulium`SmartButton`", {"SmartButton", "SwitchButton"}];


DumpSave[$LocalPath <> "library/Package/SmartButton.mx", {"Thulium`SmartButton`"}];


(* ::Input:: *)
(*ClearAll["Thulium`SmartButton`*"]*)


(* ::Input:: *)
(*buttonNames=Keys[Thulium`SmartButton`Private`SmartButtonData];*)
(*buttonNamePaged=Partition[buttonNames,UpTo@Ceiling[Length@buttonNames/Ceiling[Length@buttonNames/9]]];*)
(*Grid[Thulium`SmartButton`Private`SmartButtonDisplay/@#&/@buttonNamePaged,ItemSize->{6,6},Spacings->{.5,0}]*)


(* ::Input:: *)
(*stat=1;*)
(*SwitchButton[Dynamic[stat,UpdateInterval->0.1],*)
(*{1,"Play",Hold[stat=2]},*)
(*{2,"Stop",Hold[stat=1]}*)
(*]//List*)
