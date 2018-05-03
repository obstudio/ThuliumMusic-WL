(* ::Package:: *)

(* ::Input:: *)
(*uiSetPath;*)


uiSetPath:=DynamicModule[{path=defaultDataPath},
	CreateDialog[Row[{
		Spacer[96],
		Column[{
			Spacer[{48,48}],
			Graphics[{logo},ImageSize->{512,Automatic}],
			Spacer[1],
			caption["_ChooseBasePath","Title"],
			Row[{
				FileNameSetter[Dynamic[path],"Directory",
					Appearance->buttonDisplay["Browse","Default"],
					WindowTitle->text[["ChooseBasePath"]]
				],
				Spacer[8],
				InputField[
					Dynamic[path],String,
					BaseStyle->{FontSize->20},
					ImageSize->{384,40},
					ContinuousAction->True
				],
				Spacer[8],
				button["Tick",userInfo[["DataPath"]]=path;DialogReturn[]]
			},ImageSize->{512,48},Alignment->Center,ImageMargins->4],
			Spacer[{48,48}]
		},Alignment->Center],
		Spacer[96]
	}],WindowTitle->text[["BasicSettings"]],Background->styleColor[["Background"]]];
];


uiSettings:=DynamicModule[{choices},
	choices=userInfo;
	CreateDialog[Column[{Spacer[{40,40}],
		caption["_Settings","Title"],Spacer[1],
		Row[{Spacer[40],Grid[{
			{caption["_ChooseIdentity","Text"],
				RadioButtonBar[Dynamic@choices[["Developer"]],{
					False->caption["_NormalUser","Text"],
					True->caption["_Developer","Text"]
				}]
			},
			{caption["_ChoosePlayer","Text"],
				RadioButtonBar[Dynamic@choices[["Player"]],{
					"Old"->caption["_OldVersion","Text"],
					"New"->caption["_NewVersion","Text"]
				}]
			},
			{caption["_ChooseLanguage","Text"],
				RadioButtonBar[Dynamic@choices[["Language"]],langDict]}
			}
		],Spacer[40]}],Spacer[1],
		Row[{
			Button[text[["Save"]],
				userInfo=choices;
				Export[userPath<>"Default.json",Normal@userInfo];
				refreshLanguage;
				DialogReturn[homepage],
			ImageSize->150],
			Spacer[10],
			Button[text[["Return"]],DialogReturn[homepage],ImageSize->150]
		}],Spacer[{40,40}]
	},Center,ItemSize->Full],
	Background->styleColor[["Background"]],WindowTitle->text[["Settings"]]]
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
	Quiet@Check[
		audio=Import[dataPath<>"Buffer/"<>song<>".buffer","MP3"],
		Return[uiPlaylist[currentPlaylist]],
	Import::nffil];
	AudioStop[];
	If[Thulium`SongIndex[[song,"Image"]]!="",
		imageExist=True;
		image=Import[dataPath<>"Images/"<>Thulium`SongIndex[[song,"Image"]]];
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
				If[imageData[[Thulium`SongIndex[[song,"Image"]]]]!=<||>,
					Column[If[KeyExistsQ[imageData[[Thulium`SongIndex[[song,"Image"]]]],#],
						TagName[[#]]<>": "<>imageData[[Thulium`SongIndex[[song,"Image"]],#]],
						Nothing
					]&/@imageTags],
					text[["NoImageInfo"]]
				]
			],
		Spacer[{40,40}]}]}],Nothing],Spacer[48],
		Column[Join[{Spacer[{60,60}],
			If[Thulium`SongIndex[[song,"Comment"]]!="",
				If[textLength@Thulium`SongIndex[[song,"Comment"]]>16,
					Column,Row
				][{
					caption[Thulium`SongIndex[[song,"SongName"]],"Title"],
					caption[" ("<>Thulium`SongIndex[[song,"Comment"]]<>")","TitleCmt"]
				},Alignment->Center],
				caption[Thulium`SongIndex[[song,"SongName"]],"Title"]
			],
			Spacer[1],
			Column[If[Thulium`SongIndex[[song,#]]!="",
				caption[TagName[[#]]<>": "<>Thulium`SongIndex[[song,#]],"Text"],
				Nothing
			]&/@{"Origin","Composer","Lyricist","Adapter"},Alignment->Center],
			Spacer[1],
			If[Thulium`SongIndex[[song,"Abstract"]]!="",
				Column[caption[#,"Text"]&/@StringSplit[Thulium`SongIndex[[song,"Abstract"]],"\n"],Center],
				Nothing
			],
			Spacer[1]},
			Switch[userInfo[["Player"]],
				"Old",uiPlayerControlsOld,
				"New",uiPlayerControlsNew
			],
			{Spacer[{60,60}]}
		],Alignment->Center,ItemSize->Full],
	Spacer[48]},Alignment->Center,ImageSize->Full],
	Background->styleColor[["Background"]],WindowTitle->text[["Playing"]]<>": "<>Thulium`SongIndex[[song,"SongName"]]];
];


uiAbout:=CreateDialog[Column[{Spacer[{40,40}],
	caption["_About","Title"],
	Spacer[{20,20}],
	Row[{Spacer[60],Column[{
		caption["_Thulium","Subtitle"],
		Spacer[4],
		Grid[{
			{caption[TagName[["Version"]],"Text"],caption["2.2","Text"]},
			{caption[TagName[["Producer"]],"Text"],caption["_Obstudio","Text"]},
			{caption[TagName[["Website"]],"Text"],caption["qymp.ob-studio.cn","Text"]}
		},Alignment->Left]
	},Alignment->Left,ItemSize->Full],Spacer[60]}],
	Spacer[{20,20}],
	Button[text[["Return"]],DialogReturn[homepage],ImageSize->100],
Spacer[{40,40}]},Center,ItemSize->Full],
WindowTitle->text[["About"]],Background->styleColor[["Background"]]];


(* ::Input:: *)
(*uiAbout;*)


homepage:=DynamicModule[{playlist},
	pageCount=Ceiling[Length@Keys@Thulium`PlaylistIndex/16];
	If[Thulium`PageIndex[["Main"]]>pageCount,Thulium`PageIndex[["Main"]]=pageCount];
	playlistsPaged=Partition[Keys@Thulium`PlaylistIndex,UpTo@Ceiling[Length@Keys@Thulium`PlaylistIndex/pageCount]];
	page=Thulium`PageIndex[["Main"]];
	CreateDialog[Column[{Spacer[{40,40}],
		Row[{
			Row[{Spacer[40],caption["_Thulium","BigTitle"]},Alignment->Left,ImageSize->320],
			Row[{
				button["EnterPlaylist",DialogReturn[Thulium`PageIndex[["Main"]]=page;playlist;uiPlaylist[playlist]]],
				Spacer[10],
				button["About",DialogReturn[Thulium`PageIndex[["Main"]]=page;uiAbout]],
				Spacer[10],
				button["Settings",DialogReturn[Thulium`PageIndex[["Main"]]=page;uiSettings]],
				Spacer[10],
				button["Exit",DialogReturn[Thulium`PageIndex[["Main"]]=page;]],
			Spacer[40]},Alignment->Right,ImageSize->{400,60}]
		}],
		Spacer[1],
		Dynamic@Row[{Spacer[60],SetterBar[Dynamic@playlist,
			#->Row[{
				Row[{
					caption[TagName[[Thulium`PlaylistIndex[[#,"Type"]]]],"SongComment"]
				},Alignment->{Center,Top},ImageSize->{80,38}],
				caption[Thulium`PlaylistIndex[[#,"Title"]],"SongName"],
				Spacer[24],
				caption[Thulium`PlaylistIndex[[#,"Comment"]],"SongComment"]				
			},ImageSize->{720,30}]&/@playlistsPaged[[page]],
			Appearance->"Vertical"
		],Spacer[60]}],Spacer[1],
		uiPageSelector,
		Spacer[{40,40}]
	},Center,ItemSize->Full],
	WindowTitle->text[["Thulium"]],Background->styleColor[["Background"]]];
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
  
  DynamicModule[{song = songList[[1, "Song"]], page = Thulium`PageIndex[[playlist]]},
    CreateDialog[
      Column[{
        Spacer[{40, 40}],
        Row[{
          Row[{
            Spacer[40], caption[info[["Title"]], "BigTitle"]
          }, Alignment -> Left, ImageSize -> 480],
          Row[{
            button["Play", DialogReturn[Thulium`PageIndex[[playlist]] = page; uiPlayer[song]]],
            Spacer[10],
            If[userInfo[["Developer"]] && playlist == "All",Row[{
              button["Modify", DialogReturn[Thulium`PageIndex[[playlist]] = page; uiModifySong[song]]],
              Spacer[10],
              button["Add", DialogReturn[Thulium`PageIndex[[playlist]] = page; uiAddSong]],
              Spacer[10]}],
            Nothing],
            button["ArrowL", DialogReturn[Thulium`PageIndex[[playlist]] = page; homepage]],
            Spacer[40]
          }, Alignment -> Right, ImageSize -> {480, 56}]
        }],
        If[info[["Comment"]] != "",
          Row[{Spacer[40], caption[info[["Comment"]], "Subtitle"]}, Alignment -> Left,ImageSize -> 960],
          Nothing
        ],
        Spacer[1],
        Row[{Spacer[60],SetterBar[Dynamic @ song,
          #[["Song"]]->Row[{
            Spacer[8],
            If[info[["IndexWidth"]]>0,
              Row[{
                caption[#[["Index"]],"SongIndex"],
                Spacer[16]
              },ImageSize -> info[["IndexWidth"]],Alignment->Center],
              Spacer[4]
            ],
            If[MemberQ[Thulium`SongIndex[[#[["Song"]],"Tags"]],"$INCOMP"],
              Row[{
                caption["["<>If[KeyExistsQ[tagDict[["$INCOMP"]],userInfo[["Language"]]],
                  tagDict[["$INCOMP",userInfo[["Language"]]]],
                  tagDict[["$INCOMP",tagDict[["$INCOMP","Origin"]]]]
                ]<>"]","SongComment"],
                Spacer[20],
                caption[Thulium`SongIndex[[#[["Song"]], "SongName"]], "SongComment"]
              }],
              caption[Thulium`SongIndex[[#[["Song"]], "SongName"]], "SongName"]
            ],
            If[KeyExistsQ[Thulium`SongIndex[[#[["Song"]]]], "Comment"],
              Row[{Spacer[24], caption[Thulium`SongIndex[[#[["Song"]], "Comment"]], "SongComment"]}],
              Nothing
            ]
          }, ImageSize -> {960, 30}]& /@ songListPaged[[page]],
          Appearance -> "Vertical"
        ], Spacer[60]}],
        Spacer[1],
        uiPageSelector[Dynamic[page], pageCount],
        Spacer[{40, 40}]
      }, Center, ItemSize -> Full],
      WindowTitle -> TagName[[info[["Type"]]]]<>" - "<>info[["Title"]],
      Background -> styleColor[["Background"]]
    ]
  ];
];


(* ::Input:: *)
(*uiPlaylist["TH15-Kanjuden.qyl"];*)


(* ::Input:: *)
(*uiPlaylist["All"];*)


(* ::Input:: *)
(*uiPageSelector[Dynamic[page], pageCount]*)
