(* ::Package:: *)

Thulium`Interface`Playlist[playlist_] := Block[
  {info, length, songList, indexList, pageCount},
  info = Thulium`PlaylistIndex[playlist];
  length = Length @ info["SongList"];
  pageCount = Ceiling[length / 16];
  songList = Partition["Song" /. info["SongList"], UpTo @ Ceiling[length / pageCount]];
  indexList = Partition["Index" /. info["SongList"], UpTo @ Ceiling[length / pageCount]];
  If[Thulium`PageIndex[playlist] > pageCount, Thulium`PageIndex[playlist] = pageCount];
  Module[
    {page = Thulium`PageIndex[playlist], index = 1},
    CreateDialog[
      Thulium`Assets`Container[With[
        {
          songList = songList, pageCount = pageCount, indexList = indexList,
          indexWidth = info["IndexWidth"],
          $Epi = Unevaluated[Thulium`PageIndex[playlist] = Dynamic[page]]
        },
        Column[{
          Row[{
            Row[{
              caption[info[["Title"]], "BigTitle"]
            }, Alignment -> Left, ImageSize -> 400],
            Row[{
              button["Play", DialogReturn[$Epi; uiPlayer[songList[[page, index]]]]],
              Spacer[10],
              button["ArrowL", DialogReturn[$Epi; homepage]]
            }, Alignment -> Right, ImageSize -> {400, 60}]
          }],
          If[info["Comment"] == "", Nothing,
            Row[{
              caption[info[["Comment"]], "Subtitle"]
            }, Alignment -> Left, ImageSize -> 800]
          ],
          Spacer[20],
          Dynamic @ With[{
            data = Table[1
              Row[{
                Row[{
                  Spacer[8],
                  If[indexWidth > 0,
                    Row[{
                      caption[indexList[[page, id]], "SongIndex"],
                      Spacer[16]
                    }, ImageSize -> indexWidth, Alignment -> Center],
                    Spacer[4]
                  ],
                  caption[Thulium`SongIndex[songList[[page, id]], "SongName"], "SongName"]
                }]
              }, Alignment -> Center, ImageSize -> {880, 20}],
            {id, Length @ songList[[page]]}]
          }, SetterList[Dynamic[index], data]],
          Spacer[20],
          PageSelector[Dynamic[page], pageCount]
        }, Center]
      ], 100, 40],
      WindowTitle -> tagName[info["Type"]] <> " - " <> info["Title"],
      Background -> Thulium`Assets`Color["Window"]
    ];
  ];
];


(* ::Input:: *)
(*Thulium`Interface`Playlist["All"];*)
