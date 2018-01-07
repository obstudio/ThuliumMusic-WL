(* ::Package:: *)

(* ::Text:: *)
(*Qingyun Music Player*)


path=NotebookDirectory[];
<<(path<>"initial.wl")
<<(path<>"developer.wl")
<<(path<>"qysParse.wl")
<<(path<>"qymParse.wl")


uiSettings:=DynamicModule[{choices},
	choices=userInfo;
	CreateDialog[Column[{Spacer[{40,40}],
		Style[display[["Settings"]],Bold,28],,
		Grid[{
			{Style[display[["Identity"]]<>": ",20],
				RadioButtonBar[Dynamic@choices[["Developer"]],{False->display[["NormalUser"]],True->display[["Developer"]]}]
			},
			{Style[display[["Language"]]<>": ",20],
				RadioButtonBar[Dynamic@choices[["Language"]],langList]}
			}
		],,
		Row[{
			Button[display[["Save"]],
				userInfo=choices;
				Export[userPath<>"Default.json",Normal@userInfo];
				DialogReturn[QYMP],
			ImageSize->150],
			Spacer[10],
			Button[display[["Return"]],DialogReturn[QYMP],ImageSize->150]
		}],Spacer[{40,40}]
	},Center,ItemSize->40],
	WindowTitle->display[["Settings"]]]
];


PlayerPalette[song_]:={Spacer[{40,40}],
	Row[{Style[index[[song,"SongName"]],Bold,28],
		If[KeyExistsQ[index[[song]],"Comment"],
			Style[" ("<>index[[song,"Comment"]]<>")",Gray,28],
			Nothing
		]
	}],"",
	If[KeyExistsQ[index[[song]],"Composer"],tagName[["Composer"]]<>": "<>index[[song,"Composer"]],Nothing],
	If[KeyExistsQ[index[[song]],"Lyricist"],tagName[["Lyricist"]]<>": "<>index[[song,"Lyricist"]],Nothing],
	If[KeyExistsQ[index[[song]],"Adapter"],tagName[["Adapter"]]<>": "<>index[[song,"Adapter"]],Nothing],"",
	If[KeyExistsQ[index[[song]],"Abstract"],
		Column[StringSplit[index[[song,"Abstract"]],"\n"],Left],
		Nothing
	],"",
	Row[{
		Dynamic[timeDisplay[current["Position"]]],
		Spacer[8],
		ProgressIndicator[Dynamic[current["Position"]/duration],ImageSize->{240,16}],
		Spacer[8],
		timeDisplay[duration]
	}],"",
	Row[{Button[
		Dynamic[Switch[current["State"],
			"Playing",display[["Pause"]],
			"Paused"|"Stopped",display[["Play"]]
		]],
		Switch[current["State"],
			"Playing",current["State"]="Paused",
			"Paused"|"Stopped",current["State"]="Playing"
		],
		ImageSize->80],
		Spacer[20],
		Button[display[["Stop"]],current["State"]="Stopped",ImageSize->80],
		Spacer[20],
		Button[display[["Return"]],AudioStop[];DialogReturn[QYMP],ImageSize->80]			
	}],Spacer[{40,40}]
};


uiPlayer[song_]:=Module[{image,audio,imageExist},
	AudioStop[];
	If[KeyExistsQ[index[[song]],"Image"],
		imageExist=True;image=Import[userPath<>"Images\\"<>index[[song,"Image"]]],
		imageExist=False
	];
	audio=Import[userPath<>"Buffer\\"<>song<>".buffer","MP3"];
	duration=Duration[audio];
	current=AudioPlay[audio];
	CreateDialog[If[imageExist,
		Row[{Spacer[50],
			Column[{
				Spacer[{40,40}],
				Tooltip[Image[image,ImageSize->If[ImageAspectRatio[image]>1,{360,UpTo[720]},{UpTo[800],400}]],
					If[KeyExistsQ[imageData,index[[song,"Image"]]],
						Column[If[KeyExistsQ[imageData[[index[[song,"Image"]]]],#],
							tagName[[#]]<>": "<>imageData[[index[[song,"Image"]],#]],
							Nothing
						]&/@imageTags],
						"\:6682\:65e0\:8be5\:56fe\:7247\:7684\:4fe1\:606f"
					]
				],
				Spacer[{40,40}]
			}],
			Spacer[50],
			Column[PlayerPalette[song],Alignment->Center,ItemSize->30],
		Spacer[50]},Alignment->Center],
		(* no image *)
		Column[PlayerPalette[song],Alignment->Center,ItemSize->50]
	],WindowTitle->display[["Playing"]]<>": "<>index[[song,"SongName"]]];
];


QYMP:=DynamicModule[{song},
	refresh;
	AudioStop[];
	CreateDialog[Column[{Spacer[{40,40}],
		Row[{Style[display[["QYMP"]],Bold,32],Style[" (\:7b2c"<>ToString[page]<>"\:9875)",Gray,32]}],,
		SetterBar[Dynamic@song,
			#->Row[{
				Style[index[[#,"SongName"]],24,FontFamily->"\:5fae\:8f6f\:96c5\:9ed1"],
				Spacer[20],
				If[KeyExistsQ[index[[#]],"Comment"],Style[index[[#,"Comment"]],20,Gray,FontFamily->"\:5fae\:8f6f\:96c5\:9ed1"],Nothing]
			}]&/@songListPaged[[page]],
			Appearance->"Vertical"
		],"",
		Row[{
			Button[display[["PgPrev"]],DialogReturn[page--;QYMP],ImageSize->200,Enabled->(page>1)],
			Spacer[10],
			Button[display[["PgNext"]],DialogReturn[page++;QYMP],ImageSize->200,Enabled->(page<pageCount)]
		}],
		If[userInfo[["Developer"]],Row[{
			Button[display[["AddSong"]],DialogReturn[uiAddSong],ImageSize->200],
			Spacer[10],
			Button[display[["ModifySong"]],DialogReturn[uiModifySong[song]],ImageSize->200]
		}],Nothing],
		Row[{
			Button[display[["PlaySong"]],DialogReturn[uiPlayer[song]],ImageSize->200],
			Spacer[10],
			Button[display[["Settings"]],DialogReturn[uiSettings],ImageSize->200]
		}],
		Row[{
			Button[display[["About"]],DialogReturn[],ImageSize->200,Enabled->False],
			Spacer[10],
			Button[display[["Exit"]],DialogReturn[],ImageSize->200]
		}],Spacer[{40,40}]
	},Center,ItemSize->50],
	WindowTitle->display[["QYMP"]]]
];


(* ::Input::Initialization:: *)
refresh;updateImage;updateBuffer;


(* ::Input::Initialization:: *)
page=1;QYMP;


(* ::Input:: *)
(*AudioStop[];*)


(* ::Input:: *)
(*AudioStop[];AudioPlay@QYSParse[path<>"Songs\\Nagisa.qys"];*)


(* ::Input:: *)
(*Options[QYSParse[path<>"Songs\\Touhou\\Phantom_Ensemble.qys"]]*)


(* ::Input:: *)
(*Print[index["Touhou\\Hartmann_No_Youkai_Otome","Comment"]];*)


(* ::Input:: *)
(*EmitSound@Sound@SoundNote[12,1,"Harp"]*)
