(* ::Package:: *)

uiSettings:=DynamicModule[{choices},
	choices=UserInfo;
	CreateDialog[Column[{Spacer[{40,40}],
		Caption[TextDict["Settings"],"Title"],Spacer[1],
		Row[{Spacer[40],Grid[{
			{Caption[TextDict["ChooseIdentity"],"Text"],
				RadioButtonBar[Dynamic@choices[["Developer"]],{
					False->Caption[TextDict["NormalUser"],"Text"],
					True->Caption[TextDict["Developer"],"Text"]
				}]
			},
			{Caption[TextDict["ChoosePlayer"],"Text"],
				RadioButtonBar[Dynamic@choices[["Player"]],{
					"Old"->Caption[TextDict["OldVersion"],"Text"],
					"New"->Caption[TextDict["NewVersion"],"Text"]
				}]
			},
			{Caption[TextDict["ChooseLanguage"],"Text"],
				RadioButtonBar[Dynamic@choices[["Language"]],LangDict]}
			}
		],Spacer[40]}],Spacer[1],
		Row[{
			Button[TextDict[["Save"]],
				UserInfo=choices;
				Export[Thulium`System`$UserPath<>"Default.json",UserInfo];
				RefreshLanguage;
				DialogReturn[homepage],
			ImageSize->150],
			Spacer[10],
			Button[TextDict[["Return"]],DialogReturn[homepage],ImageSize->150]
		}],Spacer[{40,40}]
	},Center,ItemSize->Full],
	Background->WindowBackground,WindowTitle->TextDict[["Settings"]]]
];


(* ::Input:: *)
(*uiSettings;*)


(* ::Input:: *)
(*uiPlayer["Touhou/TH11-Chireiden/3rd_Eye"]*)


uiPlayer[song_]:=Block[
	{
		image, audio,
		imageExist=False,
		aspectRatio
	},
	Quiet @ Check[
		audio=Import[$DataPath<>"Buffer/"<>song<>".buffer","MP3"],
        Return[uiPlaylist[currentPlaylist]],
	Import::nffil];
	AudioStop[];
	If[Thulium`SongIndex[[song,"Image"]]!="",
		imageExist=True;
		image=Import[$DataPath<>"Images/"<>Thulium`SongIndex[[song,"Image"]]];
		aspectRatio=ImageAspectRatio[image];
	];
	$CurrentDuration=Duration[audio];
	$CurrentStream=AudioPlay[audio];
	CreateDialog[Row[{
		If[imageExist,Row[{Spacer[48],Column[{Spacer[{40,40}],
			Tooltip[ImageEffect[Image[image,ImageSize->Piecewise[{
					{{Automatic,600},aspectRatio>2},
					{{480,Automatic},aspectRatio<1/2},
					{{Automatic,400},aspectRatio<=1&&aspectRatio>1/2},
					{{360,Automatic},aspectRatio>1&&aspectRatio<2}
				}]],{"FadedFrame"}],
				If[Thulium`ImageIndex[[Thulium`SongIndex[[song,"Image"]]]]!=<||>,
					Column[If[KeyExistsQ[Thulium`ImageIndex[[Thulium`SongIndex[[song,"Image"]]]],#],
						TagName[[#]]<>": "<>Thulium`ImageIndex[[Thulium`SongIndex[[song,"Image"]],#]],
						Nothing
					]&/@imageTags],
					TextDict[["NoImageInfo"]]
				]
			],
		Spacer[{40,40}]}]}],Nothing],Spacer[48],
		Column[Join[{Spacer[{60,60}],
			If[Thulium`SongIndex[[song,"Comment"]]!="",
				If[textLength@Thulium`SongIndex[[song,"Comment"]]>16,
					Column,Row
				][{
					Caption[Thulium`SongIndex[[song,"SongName"]],"Title"],
					Caption[" ("<>Thulium`SongIndex[[song,"Comment"]]<>")","TitleCmt"]
				},Alignment->Center],
				Caption[Thulium`SongIndex[[song,"SongName"]],"Title"]
			],
			Spacer[1],
			Column[If[Thulium`SongIndex[[song,#]]!="",
				Caption[TagName[[#]]<>": "<>Thulium`SongIndex[[song,#]],"Text"],
				Nothing
			]&/@{"Origin","Composer","Lyricist","Adapter"},Alignment->Center],
			Spacer[1],
			If[Thulium`SongIndex[[song,"Abstract"]]!="",
				Column[Caption[#,"Text"]&/@StringSplit[Thulium`SongIndex[[song,"Abstract"]],"\n"],Center],
				Nothing
			],
			Spacer[1]},
			Switch[UserInfo[["Player"]],
				"Old",uiPlayerControlsOld,
				"New",uiPlayerControlsNew
			],
			{Spacer[{60,60}]}
		],Alignment->Center,ItemSize->Full],
	Spacer[48]},Alignment->Center,ImageSize->Full],
	Background->WindowBackground,WindowTitle->TextDict[["Playing"]]<>": "<>Thulium`SongIndex[[song,"SongName"]]];
];


uiAbout:=CreateDialog[Column[{Spacer[{40,40}],
	Caption[TextDict["About"],"Title"],
	Spacer[{20,20}],
	Row[{Spacer[60],Column[{
		Caption[TextDict["Thulium"],"Subtitle"],
		Spacer[4],
		Grid[{
			{Caption[TagName[["Version"]],"Text"],Caption["2.3","Text"]},
			{Caption[TagName[["Producer"]],"Text"],Caption[TextDict["Obstudio"],"Text"]},
			{Caption[TagName[["Website"]],"Text"],Caption["qymp.ob-studio.cn","Text"]}
		},Alignment->Left]
	},Alignment->Left,ItemSize->Full],Spacer[60]}],
	Spacer[{20,20}],
	Button[TextDict[["Return"]],DialogReturn[homepage],ImageSize->100],
Spacer[{40,40}]},Center,ItemSize->Full],
WindowTitle->TextDict[["About"]],Background->WindowBackground];


(* ::Input:: *)
(*uiAbout;*)


homepage:=Block[{pageCount, playlistsPaged},
  pageCount=Ceiling[Length@Keys@Thulium`PlaylistIndex/16];
  If[Thulium`PageIndex[["Main"]]>pageCount,Thulium`PageIndex[["Main"]]=pageCount];
  playlistsPaged=Partition[Keys@Thulium`PlaylistIndex,UpTo@Ceiling[Length@Keys@Thulium`PlaylistIndex/pageCount]];
  Module[{playlist = Keys[Thulium`PlaylistIndex][[1]], page = Thulium`PageIndex[["Main"]]},
    CreateDialog[With[{playlistsPaged = playlistsPaged},
      Column[{Spacer[{40,40}],
        Row[{
          Row[{Spacer[40],Caption[TextDict["Thulium"],"BigTitle"]},Alignment->Left,ImageSize->320],
          Row[{
            SmartButton["EnterPlaylist",DialogReturn[Thulium`PageIndex[["Main"]]=page;playlist;uiPlaylist[playlist]]],
            Spacer[10],
            SmartButton["About",DialogReturn[Thulium`PageIndex[["Main"]]=page;uiAbout]],
            Spacer[10],
            SmartButton["Settings",DialogReturn[Thulium`PageIndex[["Main"]]=page;uiSettings]],
            Spacer[10],
            SmartButton["Exit",DialogReturn[Thulium`PageIndex[["Main"]]=page;]],
            Spacer[40]
          },Alignment->Right,ImageSize->{400,60}]
        }],
        Spacer[1],
        Dynamic@Row[{
          Spacer[60],
          Dynamic[With[{playlists = playlistsPaged[[page]]},SetterBar[Dynamic@playlist,
            #->Row[{
              Row[{
                Caption[TagName[[Thulium`PlaylistIndex[[#,"Type"]]]],"SongComment"]
              },Alignment->{Center,Top},ImageSize->{80,38}],
              Caption[Thulium`PlaylistIndex[[#,"Title"]],"SongName"],
              Spacer[24],
              Caption[Thulium`PlaylistIndex[[#,"Comment"]],"SongComment"]				
            },ImageSize->{720,30}]&/@playlists,
            Appearance->"Vertical"
          ]],TrackedSymbols:>{page}],Spacer[60]
        }],
        Spacer[1],
        PageSelector[Dynamic[page], pageCount],
        Spacer[{40,40}]
      }, Center, ItemSize->Full]],
      WindowTitle->TextDict[["Thulium"]],
      Background->WindowBackground
    ];
  ]
];


(* ::Input:: *)
(*homepage;*)


uiPlaylist[playlist_] := Block[{info, songList, songListPaged, pageCount},
  currentPlaylist = playlist;
  info = Thulium`PlaylistIndex[[playlist]];
  songList = If[info[["IndexWidth"]]>0,
    <|"Song" -> info["Path"] <> #Song, "Index" -> #Index|>& /@ Association /@ info["SongList"],
    <|"Song" -> info["Path"] <> #Song|>& /@ Association /@ info["SongList"]
  ];
  pageCount = Ceiling[Length @ songList / 16];
  songListPaged = Partition[songList, UpTo @ Ceiling[Length @ songList / pageCount]];
  If[Thulium`PageIndex[[playlist]] > pageCount, Thulium`PageIndex[[playlist]] = pageCount];
  
  Module[{song = songList[[1, "Song"]], page = Thulium`PageIndex[[playlist]]},
    CreateDialog[With[{songListPaged = songListPaged, info = info},
      Column[{
        Spacer[{40, 40}],
        Row[{
          Row[{
            Spacer[40], Caption[info[["Title"]], "BigTitle"]
          }, Alignment -> Left, ImageSize -> 480],
          Row[{
            SmartButton["Play", DialogReturn[Thulium`PageIndex[[playlist]] = page; uiPlayer[song]]],
            Spacer[10],
            If[UserInfo[["Developer"]] && playlist == "All", Row[{
              SmartButton["Modify", DialogReturn[Thulium`PageIndex[[playlist]] = page; uiModifySong[song]]],
              Spacer[10],
              SmartButton["Add", DialogReturn[Thulium`PageIndex[[playlist]] = page; uiAddSong]],
              Spacer[10]}],
            Nothing],
            SmartButton["ArrowL", DialogReturn[Thulium`PageIndex[[playlist]] = page; homepage]],
            Spacer[40]
          }, Alignment -> Right, ImageSize -> {480, 56}]
        }],
        If[info[["Comment"]] != "",
          Row[{Spacer[40], Caption[info[["Comment"]], "Subtitle"]}, Alignment -> Left,ImageSize -> 960],
          Nothing
        ],
        Spacer[1],
        Row[{Spacer[60], Dynamic[With[{songs = songListPaged[[page]], indexWidth = info[["IndexWidth"]]},
          SetterBar[Dynamic @ song,
            #[["Song"]] -> Row[{
              Spacer[8],
              If[indexWidth>0,
                Row[{
                  Caption[#[["Index"]],"SongIndex"],
                  Spacer[16]
                },ImageSize -> indexWidth,Alignment->Center],
                Spacer[4]
              ],
              Caption[Thulium`SongIndex[[#[["Song"]], "SongName"]], "SongName"],
              If[KeyExistsQ[Thulium`SongIndex[[#[["Song"]]]], "Comment"],
                Row[{Spacer[24], Caption[Thulium`SongIndex[[#[["Song"]], "Comment"]], "SongComment"]}],
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
      WindowTitle -> TagName[[info[["Type"]]]]<>" - "<>info[["Title"]],
      Background -> WindowBackground
    ]
  ];
];


(* ::Input:: *)
(*uiPlaylist["TH15-Kanjuden.qyl"];*)


(* ::Input:: *)
(*uiPlaylist["All"];*)


(* ::Input:: *)
(*uiPageSelector[Dynamic[page], pageCount]*)
