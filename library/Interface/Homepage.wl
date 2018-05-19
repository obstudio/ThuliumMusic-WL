(* ::Package:: *)

BeginPackage["Thulium`Interface`Homepage`", {
  "Thulium`System`",
  "Thulium`Assets`",
  "Thulium`StyleSheet`",
  "Thulium`PageSel`"
}];

Homepage::usage = "Thulium Music Home Page";

Begin["`Private`"];

Homepage[] := Block[
  {
    length, playlists, pageCount, display
  },
  
  length = Length @ Keys @ PlaylistIndex;
  pageCount = Ceiling[length / 10];
  playlists = Partition[Keys @ PlaylistIndex, UpTo @ Ceiling[length / pageCount]];
  If[PageIndex["Main"] > pageCount, PageIndex["Main"] = pageCount];
  
  Module[{page = PageIndex["Main"], index = 1},
    With[
      {
        ChsFont = ChsFont, TextDict = TextDict,
        tmPageSelBox = If[pageCount > 7, tmPageSelBox2, tmPageSelBox1]
      },
      
      display = Table[Table[
        With[{info = PlaylistIndex[playlists[[pg, id]]]},
          {TemplateBox[{
            RowBox[{
              PaneBox[
                AdjustmentBox[
                  StyleBox[TagName[info["Type"]],
                    FontSize -> 12,
                    FontFamily -> ChsFont,
                    FontColor -> GrayLevel[0.3]
                  ],
                  BoxMargins -> {{0, 0.6}, {0, 0}},
                  BoxBaselineShift -> -0.4
                ],
                ImageSize -> 32,
                Alignment -> Center
              ],
              StyleBox[info["Title"],
                FontSize -> 14,
                FontFamily -> ChsFont
              ],
              TemplateBox[{12}, "Spacer1"],
              If[Total @ TextLength[{TagName[info["Type"]], info["Title"], info["Comment"]}] <= 64,
                StyleBox[info["Comment"],
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
                  {TagName["Type"] <> ":", TagName[info["Type"]]},
                  {TagName["Title"] <> ":", info["Title"]}
                },
                ColumnSpacings -> 1,
                ColumnAlignments -> {Center, Left}
              ],
              FontSize -> 20,
              FontFamily -> ChsFont
            ],
            With[{playlist = playlists[[pg, id]]},
              Hold @ DialogReturn[PageIndex["Main"] = page; Thulium`Playlist[playlist]]
            ]
          }, "<List-Local>"]}
        ],
      {id, Length @ playlists[[pg]]}], {pg, pageCount}];
  
      CreateDialog[
        {
          Cell[BoxData @ RowBox[{
            StyleBox[TextDict["Thulium"], "Title"],
            TemplateBox[{200}, "Spacer1"],
            AdjustmentBox[
              TemplateBox[{"Settings", Hold @ DialogReturn[PageIndex["Main"] = page; Thulium`Settings[]]}, "<Button-Local>"],
              BoxBaselineShift -> -0.2
            ],
            TemplateBox[{2}, "Spacer1"],
            AdjustmentBox[
              TemplateBox[{"About", Hold @ DialogReturn[PageIndex["Main"] = page; Thulium`About[]]}, "<Button-Local>"],
              BoxBaselineShift -> -0.2
            ],
            TemplateBox[{2}, "Spacer1"],
            AdjustmentBox[
              TemplateBox[{"Exit", Hold @ DialogReturn[PageIndex["Main"] = page]}, "<Button-Local>"],
              BoxBaselineShift -> -0.2
            ]
          }], "Title"],
          
          Cell[BoxData @ PaneBox[PaneSelectorBox[
            Array[Function[{pg},
              pg -> GridBox[Array[
                Function[{id}, display[[pg, id]]],
              Length @ playlists[[pg]]]]
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
                      TemplateBox[{"Enter", #5, #4}, "<List-Button>"],
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
        WindowTitle -> TextDict["Thulium"],
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

Thulium`Homepage = Thulium`Interface`Homepage`Homepage;

EndPackage[];


(* ::Input:: *)
(*Thulium`Update`CheckUpdate;*)


(* ::Input:: *)
(*Thulium`Interface`Homepage`Homepage[];*)
