(* ::Package:: *)

(* ::Input:: *)
(*uiSetPath;*)


uiSetPath:=DynamicModule[{path=dataPathTemplate},
	CreateDialog[Row[{
		Spacer[96],
		Column[{
			Spacer[{48,48}],
			Image[logo,ImageSize->{512,Automatic}],
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
				DialogReturn[QYMP],
			ImageSize->150],
			Spacer[10],
			Button[text[["Return"]],DialogReturn[QYMP],ImageSize->150]
		}],Spacer[{40,40}]
	},Center,ItemSize->Full],
	Background->styleColor[["Background"]],WindowTitle->text[["Settings"]]]
];


(* ::Input:: *)
(*uiSettings;*)


(* ::Input:: *)
(*uiPlayer["Touhou\\Dream_Battle"]*)


imageData


uiPlayer[song_]:=Module[{image,audio,imageExist=False,aspectRatio},
	AudioStop[];
	If[index[[song,"Image"]]!="",
		imageExist=True;
		image=Import[dataPath<>"Images\\"<>index[[song,"Image"]]];
		aspectRatio=ImageAspectRatio[image];
	];
	audio=Import[dataPath<>"Buffer\\"<>song<>".buffer","MP3"];
	duration=Duration[audio];
	current=AudioPlay[audio];
	CreateDialog[Row[{
		If[imageExist,Row[{Spacer[48],Column[{Spacer[{40,40}],
			Tooltip[ImageEffect[Image[image,ImageSize->Piecewise[{
					{{Automatic,600},aspectRatio>2},
					{{480,Automatic},aspectRatio<1/2},
					{{Automatic,400},aspectRatio<1&&aspectRatio>1/2},
					{{360,Automatic},aspectRatio>1&&aspectRatio<2}
				}]],{"FadedFrame"}],
				If[imageData[[index[[song,"Image"]]]]!=<||>,
					Column[If[KeyExistsQ[imageData[[index[[song,"Image"]]]],#],
						tagName[[#]]<>": "<>imageData[[index[[song,"Image"]],#]],
						Nothing
					]&/@imageTags],
					text[["NoImageInfo"]]
				]
			],
		Spacer[{40,40}]}]}],Nothing],Spacer[48],
		Column[Join[{Spacer[{60,60}],
			If[index[[song,"Comment"]]!="",
				If[textLength@index[[song,"Comment"]]>16,
					Column,Row
				][{
					caption[index[[song,"SongName"]],"Title"],
					caption[" ("<>index[[song,"Comment"]]<>")","TitleComment"]
				},Alignment->Center],
				caption[index[[song,"SongName"]],"Title"]
			],
			Spacer[1],
			Column[If[index[[song,#]]!="",
				caption[tagName[[#]]<>": "<>index[[song,#]],"Text"],
				Nothing
			]&/@{"Origin","Composer","Lyricist","Adapter"},Alignment->Center],
			Spacer[1],
			If[index[[song,"Abstract"]]!="",
				Column[caption[#,"Text"]&/@StringSplit[index[[song,"Abstract"]],"\n"],Center],
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
	Background->styleColor[["Background"]],WindowTitle->text[["Playing"]]<>": "<>index[[song,"SongName"]]];
];


uiAbout:=CreateDialog[Column[{Spacer[{40,40}],
	caption["_About","Title"],
	Spacer[{20,20}],
	Row[{Spacer[60],Column[{
		caption["_QYMP","Subtitle"],
		Spacer[4],
		Grid[{
			{caption[tagName[["Version"]],"Text"],caption["1.7","Text"]},
			{caption[tagName[["Producer"]],"Text"],caption["_Obstudio","Text"]},
			{caption[tagName[["Website"]],"Text"],caption["qymp.ob-studio.cn","Text"]}
		},Alignment->Left]
	},Alignment->Left,ItemSize->Full],Spacer[60]}],
	Spacer[{20,20}],
	Button[text[["Return"]],DialogReturn[QYMP],ImageSize->100],
Spacer[{40,40}]},Center,ItemSize->Full],
WindowTitle->text[["About"]],Background->styleColor[["Background"]]];


(* ::Input:: *)
(*uiAbout;*)


QYMP:=DynamicModule[{playlist},
	pageCount=Ceiling[Length@playlists/16];
	If[pageData[["Main"]]>pageCount,pageData[["Main"]]=pageCount];
	playlistsPaged=Partition[playlists,UpTo@Ceiling[Length@playlists/pageCount]];
	page=pageData[["Main"]];
	CreateDialog[Column[{Spacer[{40,40}],
		Row[{
			Row[{Spacer[40],caption["_QYMP","BigTitle"]},Alignment->Left,ImageSize->320],
			Row[{
				button["EnterPlaylist",DialogReturn[pageData[["Main"]]=page;playlist;uiPlaylist[playlist]]],
				Spacer[10],
				button["About",DialogReturn[pageData[["Main"]]=page;uiAbout]],
				Spacer[10],
				button["Settings",DialogReturn[pageData[["Main"]]=page;uiSettings]],
				Spacer[10],
				button["Exit",DialogReturn[pageData[["Main"]]=page;]],
				Spacer[40]
			},Alignment->Right,ImageSize->{320,56}]
		}],
		Spacer[1],
		Dynamic@Row[{Spacer[60],SetterBar[Dynamic@playlist,
			#->Row[{
				Spacer[8],
				caption[playlistData[[#,"Title"]],"SongName"],
				Row[{Spacer[24],caption[playlistData[[#,"Comment"]],"SongComment"]}]				
			},ImageSize->{640,30}]&/@playlistsPaged[[page]],
			Appearance->"Vertical"
		],Spacer[60]}],Spacer[1],
		uiPageSelector,
		Spacer[{40,40}]
	},Center,ItemSize->Full],
	WindowTitle->text[["QYMP"]],Background->styleColor[["Background"]]]
];


uiPlaylist[playlist_]:=DynamicModule[{song},
	If[!MemberQ[playlists,playlist],Return[]];
	currentPlaylist=playlist;
	playlistInfo=playlistData[[playlist]];
	songList=If[playlistInfo[["IndexWidth"]]>0,
		<|"Song"->playlistInfo[["Path"]]<>#Song,"Index"->#Index|>&/@Association/@playlistInfo[["SongList"]],
		<|"Song"->playlistInfo[["Path"]]<>#Song|>&/@Association/@playlistInfo[["SongList"]]
	];
	pageCount=Ceiling[Length@songList/16];
	songListPaged=Partition[songList,UpTo@Ceiling[Length@songList/pageCount]];
	If[pageData[[playlist]]>pageCount,pageData[[playlist]]=pageCount];
	page=pageData[[playlist]];
	CreateDialog[Column[{Spacer[{40,40}],
		Row[{
			Row[{
				Spacer[40],caption[playlistInfo[["Title"]],"BigTitle"]
			},Alignment->Left,ImageSize->480],
			Row[{
				button["Play",DialogReturn[pageData[[playlist]]=page;uiPlayer[song]]],
				Spacer[10],
				If[userInfo[["Developer"]]&&playlist=="All",Row[{
					button["Modify",DialogReturn[pageData[[playlist]]=page;uiModifySong[song]]],
					Spacer[10],
					button["Add",DialogReturn[pageData[[playlist]]=page;uiAddSong]],
					Spacer[10]}],
				Nothing],
				button["ArrowL",DialogReturn[pageData[[playlist]]=page;QYMP]],
			Spacer[40]},Alignment->Right,ImageSize->{480,56}]
		}],
		If[playlistInfo[["Comment"]]!="",
			Row[{Spacer[40],caption[playlistInfo[["Comment"]],"Subtitle"]},Alignment->Left,ImageSize->960],
			Nothing
		],
		Spacer[1],
		Dynamic@Row[{Spacer[60],SetterBar[Dynamic@song,
			#[["Song"]]->Row[{
				Spacer[8],
				If[playlistInfo[["IndexWidth"]]>0,
					Row[{
						caption[#[["Index"]],"SongIndex"],
						Spacer[16]
					},ImageSize->playlistInfo[["IndexWidth"]],Alignment->Center],
					Spacer[4]
				],
				caption[index[[#[["Song"]],"SongName"]],"SongName"],
				If[KeyExistsQ[index[[#[["Song"]]]],"Comment"],
					Row[{Spacer[24],caption[index[[#[["Song"]],"Comment"]],"SongComment"]}],
					Nothing
				]
			},ImageSize->{960,30}]&/@songListPaged[[page]],
			Appearance->"Vertical"
		],Spacer[60]}],
		Spacer[1],
		uiPageSelector,
		Spacer[{40,40}]
	},Center,ItemSize->Full],
	WindowTitle->text[["Playlist"]]<>" - "<>playlistInfo[["Title"]],Background->styleColor[["Background"]]]
];


(* ::Input:: *)
(*QYMP;*)


(* ::Input:: *)
(*uiPlaylist["TH15-Kanjuden"];*)


(* ::Input:: *)
(*uiPlaylist["All"];*)
