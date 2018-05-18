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


(*End[];

EndPackage[];

Thulium`Homepage = Thulium`Interface`Homepage`Homepage;
Thulium`Playlist = Thulium`Interface`Homepage`Playlist;*)


(* ::Input:: *)
(*Thulium`Homepage[];*)
