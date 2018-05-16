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
    CreateDialog[
      {
        Cell[BoxData @ GridBox[{
          {TemplateBox[{Unevaluated[index], 1, "booo", "booo"}, "<Setter-Local>"]},
          {TemplateBox[{Unevaluated[index], 2, "fooo", "fooo"}, "<Setter-Local>"]},
          {TemplateBox[{Unevaluated[index], 3, "wooo", "wooo"}, "<Setter-Local>"]}
        }, RowSpacings -> 0]],
        TextCell[Dynamic @ index]
      },
      
      StyleDefinitions -> Notebook[{
        Thulium`StyleSheet`Include["Setter-Item"],
        Thulium`StyleSheet`Include["Setter"],
        Thulium`StyleSheet`Include["Tooltip"],
        
        Cell[StyleData["<Setter-Local>"],
          TemplateBoxOptions -> {DisplayFunction -> Function[
            TemplateBox[{
              #1, #2, #3,
              TemplateBox[{#4, RGBColor[0, 0, 0], RGBColor[0.96, 0.98, 1], RGBColor[0.96, 0.98, 1], 200, 36}, "<Setter-Item>"],
              TemplateBox[{#4, RGBColor[0, 0, 0], RGBColor[0.96, 1, 0.98], RGBColor[0.94, 1, 0.98], 200, 36}, "<Setter-Item>"],
              TemplateBox[{#4, RGBColor[0, 0, 0], RGBColor[0.92, 1, 0.98], RGBColor[0.88, 1, 0.98], 200, 36}, "<Setter-Item>"],
              TemplateBox[{#4, RGBColor[0, 0, 0], RGBColor[0.97, 1, 0.97], RGBColor[0.97, 1, 0.97], 200, 36}, "<Setter-Item>"]
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
      WindowSize -> {800, 600},
      WindowFrame -> "ModelessDialog",
      Magnification -> 2,
      Saveable -> False,
      Evaluatable -> False,
      Editable -> False,
      Deployed -> True,
      DynamicEvaluationTimeout -> 30
    ];
  ];
];

End[];

EndPackage[];


(* ::Input:: *)
(*newPlaylist["All"];*)
