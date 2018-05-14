(* ::Package:: *)

(*BeginPackage["Thulium`Interface`Homepage`", {
  "Thulium`System`",
  "Thulium`Assets`",
  "Thulium`SmartButton`",
  "Thulium`PageSelector`"
}];

Homepage::usage = "Thulium Music Homepage Interface";
Playlist::usage = "Thulium Music Playlist Interface";

Begin["`Private`"];*)


Thulium`Homepage[] := Block[{pageCount, playlistsPaged},
  pageCount = Ceiling[Length @ Keys @ PlaylistIndex / 16];
  If[PageIndex["Main"] > pageCount, PageIndex["Main"] = pageCount];
  playlistsPaged = Partition[Keys @ PlaylistIndex, UpTo @ Ceiling[Length @ Keys @ PlaylistIndex / pageCount]];
  Module[{playlist = Keys[PlaylistIndex][[1]], page = PageIndex["Main"]},
    CreateDialog[With[{playlistsPaged = playlistsPaged},
      Column[{Spacer[{40, 40}],
        Row[{
          Row[{Spacer[40], Caption[TextDict["Thulium"], "BigTitle"]}, Alignment -> Left, ImageSize -> 320],
          Row[{
            SmartButton["EnterPlaylist", DialogReturn[PageIndex["Main"] = page; playlist; Thulium`Playlist[playlist]]],
            Spacer[10],
            SmartButton["About", DialogReturn[PageIndex["Main"] = page; Thulium`About[]]],
            Spacer[10],
            SmartButton["Settings", DialogReturn[PageIndex["Main"] = page; Thulium`Settings[]]],
            Spacer[10],
            SmartButton["Exit", DialogReturn[PageIndex["Main"] = page]],
            Spacer[40]
          }, Alignment -> Right, ImageSize -> {400, 60}]
        }],
        Spacer[1],
        Dynamic @ Row[{
          Spacer[60],
          Dynamic[With[{playlists = playlistsPaged[[page]]}, SetterBar[Dynamic@playlist,
            #->Row[{
              Row[{
                Caption[TagName[[PlaylistIndex[[#,"Type"]]]],"SongComment"]
              },Alignment->{Center,Top},ImageSize->{80,38}],
              Caption[PlaylistIndex[[#,"Title"]], "SongName"],
              Spacer[24],
              Caption[PlaylistIndex[[#,"Comment"]], "SongComment"]				
            },ImageSize->{720,30}]& /@ playlists,
            Appearance->"Vertical"
          ]],TrackedSymbols:>{page}],Spacer[60]
        }],
        Spacer[1],
        PageSelector[Dynamic[page], pageCount],
        Spacer[{40,40}]
      }, Center, ItemSize->Full]],
      WindowTitle->TextDict["Thulium"],
      Background->WindowBackground
    ];
  ]
];


(* ::Input:: *)
(*Thulium`Homepage;*)


Thulium`Playlist[playlist_] := Block[{info, songList, songListPaged, pageCount},
  Thulium`CurrentPlaylist = playlist;
  info = PlaylistIndex[playlist];
  songList = If[info["IndexWidth"] > 0,
    <|"Song" -> info["Path"] <> #Song, "Index" -> #Index|>& /@ Association /@ info["SongList"],
    <|"Song" -> info["Path"] <> #Song|>& /@ Association /@ info["SongList"]
  ];
  pageCount = Ceiling[Length @ songList / 16];
  songListPaged = Partition[songList, UpTo @ Ceiling[Length @ songList / pageCount]];
  If[PageIndex[[playlist]] > pageCount, PageIndex[playlist] = pageCount];
  
  Module[{song = songList[[1, "Song"]], page = PageIndex[playlist]},
    CreateDialog[With[{songListPaged = songListPaged, info = info},
      Column[{
        Spacer[{40, 40}],
        Row[{
          Row[{
            Spacer[40], Caption[info["Title"], "BigTitle"]
          }, Alignment -> Left, ImageSize -> 480],
          Row[{
            SmartButton["Play", DialogReturn[PageIndex[[playlist]] = page; Thulium`Player[song]]],
            Spacer[10],
            If[UserInfo["Developer"] && playlist == "All", Row[{
              SmartButton["Modify", DialogReturn[PageIndex[[playlist]] = page; Thulium`ModifySong[song]]],
              Spacer[10],
              SmartButton["Add", DialogReturn[PageIndex[[playlist]] = page; Thulium`AddSong[]]],
              Spacer[10]}],
            Nothing],
            SmartButton["ArrowL", DialogReturn[PageIndex[[playlist]] = page; Thulium`Homepage[]]],
            Spacer[40]
          }, Alignment -> Right, ImageSize -> {480, 56}]
        }],
        If[info["Comment"] != "",
          Row[{Spacer[40], Caption[info["Comment"], "Subtitle"]}, Alignment -> Left,ImageSize -> 960],
          Nothing
        ],
        Spacer[1],
        Row[{Spacer[60], Dynamic[With[{songs = songListPaged[[page]], indexWidth = info["IndexWidth"]},
          SetterBar[Dynamic @ song,
            #["Song"] -> Row[{
              Spacer[8],
              If[indexWidth>0,
                Row[{
                  Caption[#["Index"],"SongIndex"],
                  Spacer[16]
                },ImageSize -> indexWidth,Alignment->Center],
                Spacer[4]
              ],
              Caption[SongIndex[#["Song"], "SongName"], "SongName"],
              If[KeyExistsQ[SongIndex[#["Song"]], "Comment"],
                Row[{Spacer[24], Caption[SongIndex[#["Song"], "Comment"], "SongComment"]}],
                Nothing
              ]
            }, ImageSize -> {960, 30}]& /@ songs,
            Appearance -> "Vertical"
          ]], TrackedSymbols :> {page}
        ], Spacer[60]}],
        Spacer[1],
        PageSelector[Dynamic[page], pageCount],
        Spacer[{40, 40}]
      }, Center, ItemSize -> Full]],
      WindowTitle -> TagName[info["Type"]]<>" - "<>info["Title"],
      Background -> WindowBackground
    ]
  ];
];


(*End[];

EndPackage[];

Thulium`Homepage = Thulium`Interface`Homepage`Homepage;
Thulium`Playlist = Thulium`Interface`Homepage`Playlist;*)


(* ::Input:: *)
(*Thulium`Homepage[];*)


(* ::Input:: *)
(*Thulium`Playlist["TH15-Kanjuden.qyl"];*)


(* ::Input:: *)
(*Thulium`Playlist["All"];*)
