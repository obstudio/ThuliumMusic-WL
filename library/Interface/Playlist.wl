(* ::Package:: *)

BeginPackage["Thulium`Interface`Playlist`", {
  "Thulium`System`",
  "Thulium`Assets`",
  "Thulium`StyleSheet`"
}];

Playlist::usage = "Thulium Music Playlist Page";

Begin["`Private`"];

Playlist[playlist_] := Block[
  {
    info, length, songList, indexList, indexWidth, pageCount, display
  },
  
  Thulium`CurrentPlaylist = playlist;
  info = PlaylistIndex[playlist];
  length = Length @ info["SongList"];
  pageCount = Ceiling[length / 10];
  songList = Partition["Song" /. info["SongList"], UpTo @ Ceiling[length / pageCount]];
  indexList = Partition["Index" /. info["SongList"], UpTo @ Ceiling[length / pageCount]];
  indexWidth = 8 * Max[0, TextLength /@ DeleteCases[indexList, "Index", Infinity]];
  If[PageIndex[playlist] > pageCount, PageIndex[playlist] = pageCount];
  
  Module[{page = Thulium`PageIndex[playlist], index = 1},
    With[
      {
        ChsFont = ChsFont, TextDict = TextDict,
        tmPageSelBox = If[pageCount > 7, tmPageSelBox2, tmPageSelBox1]
      },
      
      display = Table[Table[
        With[
          {
            songInfo = SongIndex[info["Path"] <> songList[[pg, id]]],
            indexName = If[indexList[[pg, id]] =!= "Index", indexList[[pg, id]], ""]
          },
          {TemplateBox[{
            RowBox[{
              If[indexName =!= "",
                PaneBox[
                  AdjustmentBox[
                    StyleBox[indexList[[pg, id]],
                      FontSize -> 12,
                      FontFamily -> ChsFont,
                      FontColor -> GrayLevel[0.3]
                    ],
                    BoxMargins -> {{0, 0.6}, {0, 0}},
                    BoxBaselineShift -> -0.4
                  ],
                  ImageSize -> indexWidth,
                  Alignment -> Center
                ],
                Nothing
              ],
              StyleBox[songInfo["SongName"],
                FontSize -> 14,
                FontFamily -> ChsFont
              ],
              TemplateBox[{12}, "Spacer1"],
              If[Total @ TextLength[{indexName, songInfo["SongName"], songInfo["Comment"]}] <= 64,
                StyleBox[songInfo["Comment"],
                  FontSize -> 12,
                  FontFamily -> ChsFont,
                  FontColor -> GrayLevel[0.3]
                ],
                Nothing
              ]
            }],
            StyleBox[
              GridBox[
                {
                  {TagName["FileName"] <> ":", songList[[pg, id]]},
                  {TagName["SongName"] <> ":", songInfo["SongName"]},
                  If[songInfo["Uploader"] =!= "",
                    {TagName["Uploader"] <> ":", songInfo["Uploader"]},
                  Nothing]
                },
                ColumnSpacings -> 1,
                ColumnAlignments -> {Center, Left}
              ],
              FontSize -> 20,
              FontFamily -> ChsFont
            ],
            With[{song = info["Path"] <> songList[[pg, id]]},
              Hold @ DialogReturn[
                PageIndex[playlist] = page;
                Thulium`Player[song];
              ]
            ]
          }, "<List-Local>"]}
        ],
      {id, Length @ songList[[pg]]}], {pg, pageCount}];
  
      CreateDialog[
        {
          Cell[BoxData @ RowBox[{
            StyleBox[info["Title"], "Title"],
            TemplateBox[{280}, "Spacer1"],
            AdjustmentBox[
              TemplateBox[{"Return", Hold @ DialogReturn[PageIndex[playlist] = page; Thulium`Homepage[]]}, "<Button-Local>"],
              BoxBaselineShift -> -0.2
            ]
          }], "Title"],
          
          If[info["Comment"] =!= "", Cell[BoxData @ PaneBox[
            StyleBox[info["Comment"], "Subtitle"]
          ], "Subtitle"], Nothing],
          
          Cell[BoxData @ PaneBox[PaneSelectorBox[
            Array[Function[{pg},
              pg -> GridBox[Array[
                Function[{id}, display[[pg, id]]],
              Length @ songList[[pg]]]]
            ], pageCount],
          Dynamic[page]]], "SetterList"],
          
          Cell[BoxData @ tmPageSelBox[page, pageCount], "PageSelector"]
        },
        
        StyleDefinitions -> Notebook[{
          tmList,
          tmButton,
          tmPageSel,
          
          Cell[StyleData["Title"],
            CellMargins -> {{0, 0}, {0, 24}},
            TextAlignment -> Center,
            FontFamily -> ChsFont,
            FontSize -> 24,
            FontWeight -> Bold
          ],
          
          Cell[StyleData["Subtitle"],
            CellMargins -> {{0, 0}, {0, 0}},
            TextAlignment -> Center,
            FontFamily -> ChsFont,
            FontSize -> 14,
            PaneBoxOptions -> {Alignment -> Left, ImageSize -> 440}
          ],
          
          Cell[StyleData["SetterList"],
            CellMargins -> {{0, 0}, {16, 16}},
            TextAlignment -> Center,
            PaneBoxOptions -> {
              Alignment -> {Center, Center},
              ImageSize -> {Automatic, 280}
            },
            GridBoxOptions -> {RowSpacings -> 0}
          ],
          
          Cell[StyleData["PageSelector"],
            CellMargins -> {{0, 0}, {24, 0}},
            TextAlignment -> Center
          ],
          
          Cell[StyleData["<Button-Local>"],
            TemplateBoxOptions -> {DisplayFunction -> Function[
              TemplateBox[{
                TemplateBox[{#1, Opacity[0], RGBColor[0, 0.7, 0.94], RGBColor[0, 0.7, 0.94], 32}, "<Button-Round>"],
                TemplateBox[{#1, RGBColor[0, 0.7, 0.94], RGBColor[0, 0.7, 0.94], RGBColor[1, 1, 1], 32}, "<Button-Round>"],
                TemplateBox[{#1, RGBColor[0, 0.7, 0.94, 0.3], RGBColor[0, 0.7, 0.94], RGBColor[0, 0.7, 0.94], 32}, "<Button-Round>"],
                #2, StyleBox[TextDict[#1], FontFamily -> ChsFont]
              }, "<Button>"]
            ]}
          ],
          
          Cell[StyleData["<List-Button>"],
            TemplateBoxOptions -> {DisplayFunction -> Function[
              TemplateBox[{
                PaneSelectorBox[{
                  True -> TemplateBox[{#1, Opacity[0], RGBColor[0, 0.7, 0.94], RGBColor[0, 0.7, 0.94], 18}, "<Button-Round>"],
                  False -> TemplateBox[{#1, Opacity[0], Opacity[0], Opacity[0], 18}, "<Button-Round>"]
                }, #3],
                TemplateBox[{#1, RGBColor[0, 0.7, 0.94], RGBColor[0, 0.7, 0.94], RGBColor[1, 1, 1], 18}, "<Button-Round>"],
                TemplateBox[{#1, RGBColor[0, 0.7, 0.94, 0.3], RGBColor[0, 0.7, 0.94], RGBColor[0, 0.7, 0.94], 18}, "<Button-Round>"],
                #2
              }, "<Button-no-Tooltip>"]
            ]}
          ],
          
          Cell[StyleData["<Item-Local>"],
            TemplateBoxOptions -> {DisplayFunction -> Function[
              TemplateBox[{
                RowBox[
                  {
                    PaneBox[
                      StyleBox[#1, FontSize -> 14, FontFamily -> ChsFont],
                      ImageSize -> 450,
                      Alignment -> Left
                    ],
                    AdjustmentBox[
                      TemplateBox[{"Play", #5, #4}, "<List-Button>"],
                      BoxBaselineShift -> -0.1
                    ]
                  }
                ],
                RGBColor[0, 0, 0], #2, #3, 480, 18
              }, "<Item>"]
            ]}
          ],
          
          Cell[StyleData["<List-Local>"],
            TemplateBoxOptions -> {DisplayFunction -> Function[
              TemplateBox[{
                TemplateBox[{#1, RGBColor[0.96, 0.98, 1], RGBColor[0.96, 0.98, 1], False, #3}, "<Item-Local>"],
                TemplateBox[{#1, RGBColor[0.96, 1, 0.98], RGBColor[0.94, 1, 0.98], True, #3}, "<Item-Local>"],
                #2
              }, "<List>"]
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
        WindowSize -> {1200, 900},
        WindowFrame -> "ModelessDialog",
        Magnification -> 2,
        Saveable -> False,
        Editable -> False,
        Deployed -> True,
        Evaluatable -> False
      ];
    ];
    
    Evaluate[Unique[]] := index;
    Evaluate[Unique[]] := page;
  ];
];

End[];

Thulium`Playlist = Thulium`Interface`Playlist`Playlist;

EndPackage[];


(* ::Input:: *)
(*Thulium`Update`CheckUpdate;*)


(* ::Input:: *)
(*Playlist["All"];*)


(* ::Input:: *)
(*Playlist["TH11-Chireiden.qyl"];*)


(* ::Input:: *)
(*Playlist["Clannad.qyl"];*)


(* ::Input:: *)
(*SongIndex["Touhou/TH11-Chireiden/3rd_Eye"]*)


(* ::Input:: *)
(*PlaylistIndex["Clannad.qyl"]*)
