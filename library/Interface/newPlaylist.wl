(* ::Package:: *)

BeginPackage["Thulium`Interface`Playlist`", {
  "Thulium`System`",
  "Thulium`Assets`"
}];

newPlaylist::usage = "newPlaylist";

Begin["`Private`"];

newPlaylist[playlist_] := Block[
  {info, length, songList, indexList, pageCount},
  info = PlaylistIndex[playlist];
  length = Length @ info["SongList"];
  pageCount = Ceiling[length / ListSize];
  songList = Partition["Song" /. info["SongList"], UpTo @ Ceiling[length / pageCount]];
  indexList = Partition["Index" /. info["SongList"], UpTo @ Ceiling[length / pageCount]];
  If[Thulium`PageIndex[playlist] > pageCount, Thulium`PageIndex[playlist] = pageCount];
  Module[{page = Thulium`PageIndex[playlist], index = 1},
  With[{ChsFont = ChsFont, TextDict = TextDict},
    CreateDialog[
      {
        Cell[BoxData @ GridBox[{
          {TemplateBox[{Unevaluated[index], 1, "booo", "booo"}, "<Setter-Local>"]},
          {TemplateBox[{Unevaluated[index], 2, "fooo", "fooo"}, "<Setter-Local>"]},
          {TemplateBox[{Unevaluated[index], 3, "wooo", "wooo"}, "<Setter-Local>"]}
        }], "SetterList"],
        TextCell[Dynamic @ index]
      },
      
      StyleDefinitions -> Notebook[{
        Thulium`StyleSheet`Include["Tooltip"],
        Thulium`StyleSheet`tmSetter,
        Thulium`StyleSheet`tmButton,
        
        Cell[StyleData["SetterList"],
          CellMargins -> {{20, 20}, {20, 20}},
          TextAlignment -> Center,
          GridBoxOptions -> {RowSpacings -> 0}
        ],
        
        Cell[StyleData["<Button-Local>"],
          TemplateBoxOptions -> {DisplayFunction -> Function[
            TemplateBox[{
              PaneSelectorBox[{
                True -> TemplateBox[{#1, Opacity[0], RGBColor[0, 0.7, 0.94], RGBColor[0, 0.7, 0.94], 20}, "<Button-Round>"],
                False -> TemplateBox[{#1, Opacity[0], Opacity[0], Opacity[0], 20}, "<Button-Round>"]
              }, #3],
              TemplateBox[{#1, RGBColor[0, 0.7, 0.94], RGBColor[0, 0.7, 0.94], RGBColor[1, 1, 1], 20}, "<Button-Round>"],
              TemplateBox[{#1, RGBColor[0, 0.7, 0.94, 0.3], RGBColor[0, 0.7, 0.94], RGBColor[0, 0.7, 0.94], 20}, "<Button-Round>"],
              #2, StyleBox[TextDict[#1], FontFamily -> ChsFont]
            }, "<Button>"]
          ]}
        ],
        
        Cell[StyleData["<Setter-Item-Local>"],
          TemplateBoxOptions -> {DisplayFunction -> Function[
            TemplateBox[{
              GridBox[
                {{
                  AdjustmentBox[StyleBox[#1,
                    FontSize -> 14,
                    FontFamily -> ChsFont
                  ], BoxBaselineShift -> -0.6],
                  AdjustmentBox[TemplateBox[
                    {"Play", Null, #4},
                    "<Button-Local>"
                  ], BoxBaselineShift -> -1]
                }},
                RowAlignments -> Baseline,
                ColumnAlignments -> {Left, Right},
                ColumnWidths -> {20, 8},
                ColumnSpacings -> 0
              ],
              RGBColor[0, 0, 0], #2, #3, 400, 20
            }, "<Setter-Item>"]
          ]}
        ],
        
        Cell[StyleData["<Setter-Local>"],
          TemplateBoxOptions -> {DisplayFunction -> Function[
            TemplateBox[{
              #1, #2, #3,
              TemplateBox[{#4, RGBColor[0.96, 0.98, 1], RGBColor[0.96, 0.98, 1], False}, "<Setter-Item-Local>"],
              TemplateBox[{#4, RGBColor[0.96, 1, 0.98], RGBColor[0.94, 1, 0.98], True}, "<Setter-Item-Local>"],
              TemplateBox[{#4, RGBColor[0.92, 1, 0.98], RGBColor[0.88, 1, 0.98], False}, "<Setter-Item-Local>"],
              TemplateBox[{#4, RGBColor[0.97, 1, 0.97], RGBColor[0.97, 1, 0.97], False}, "<Setter-Item-Local>"]
            }, "<Setter>"]
          ]}
        ]
      }],
      
      ShowCellLabel -> False,
      ShowCellTags -> False,
      ShowCellBracket -> False,
      CellGrouping -> Manual,
      Background -> RGBColor[1, 1, 1],
      WindowTitle -> TagName[info["Type"]] <> " - " <> info["Title"],
      WindowElements -> {},
      WindowFrameElements -> {"CloseBox", "MinimizeBox"},
      WindowSize -> {1024, 768},
      WindowFrame -> "ModelessDialog",
      Magnification -> 2,
      Saveable -> False,
      Evaluatable -> False,
      Editable -> False,
      Deployed -> True
    ];
  ]];
];

End[];

EndPackage[];


(* ::Input:: *)
(*newPlaylist["All"];*)


(* ::Input:: *)
(*Graphics[{Black,Thickness[0.1],CapForm["Round"],Circle[]}]*)
