(* ::Package:: *)

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
      Container[With[
        {
          songList = songList, pageCount = pageCount, indexList = indexList,
          indexWidth = info["IndexWidth"],
          $Epi = Unevaluated[Thulium`PageIndex[playlist] = page]
        },
        Column[{
          Row[{
            Row[{
              Caption[info[["Title"]], "BigTitle"]
            }, Alignment -> Left, ImageSize -> 400],
            Row[{
              SmartButton["Play", DialogReturn[$Epi; Thulium`Player[songList[[page, index]]]]],
              Spacer[10],
              SmartButton["Return", DialogReturn[$Epi; Thulium`homepage]]
            }, Alignment -> Right, ImageSize -> {400, 60}]
          }],
          If[info["Comment"] == "", Nothing,
            Row[{
              Caption[info[["Comment"]], "Subtitle"]
            }, Alignment -> Left, ImageSize -> 800]
          ],
          Spacer[20],
          Dynamic[
            Row[{SetterList[Dynamic[index], Table[
              Row[{
                Row[{
                  Spacer[8],
                  If[indexWidth > 0,
                    Row[{
                      Caption[indexList[[page, id]], "SongIndex"],
                      Spacer[16]
                    }, ImageSize -> indexWidth, Alignment -> Center],
                    Spacer[4]
                  ],
                  Caption[SongIndex[songList[[page, id]], "SongName"], "SongName"]
                }, Alignment -> Left, ImageSize -> 480],
                Row[{
                  Row[{"Right"}, Alignment -> Center],
                  Spacer[8]
                }, Alignment -> Right, ImageSize -> 480]
              }, Alignment -> Center, ImageSize -> {960, 32}],
            {id, Length @ songList[[page]]}]]},
            ImageSize -> {960, 600}],
          TrackedSymbols :> {page}],
          Spacer[20],
          PageSelector[Dynamic[page], pageCount]
        }, Center]
      ], 100, 40],
      WindowTitle -> TagName[info["Type"]] <> " - " <> info["Title"],
      Background -> WindowBackground
    ];
  ];
];


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
          {TemplateBox[{index, 2, "wooo"}, "<SetterBar>"]}
        }]]
      },
      StyleDefinitions -> Notebook[{
        Thulium`StyleSheet`Include["Setter-Item"],
        Thulium`StyleSheet`Include["Setter"],
        Thulium`StyleSheet`Include["Tooltip"],
        
        Cell[StyleData["<SetterBar>"],
          TemplateBoxOptions -> {DisplayFunction -> Function[
  PaneSelectorBox[{
    True -> TemplateBox[{#3, RGBColor[0.96, 0.96, 1], 40}, "<Setter-Item>"],
    False -> PaneSelectorBox[{
      True -> 
        TagBox[
          PaneSelectorBox[
            {True -> TemplateBox[{#3, RGBColor[0.92, 1, 0.92], 40}, "<Setter-Item>"],
            False -> TemplateBox[{#3, RGBColor[0.96, 1, 0.96], 40}, "<Setter-Item>"]},
            Dynamic @ CurrentValue["MouseButtonTest"]
          ],
        EventHandlerTag @ {"MouseClicked" :> #1 = #2}],
      False -> TemplateBox[{#3, RGBColor[1, 0.96, 0.96], 40}, "<Setter-Item>"]
    }, Dynamic @ CurrentValue["MouseOver"]]
  }, Dynamic[#1 === #2]]
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


(* ::Input:: *)
(*newPlaylist["All"];*)
