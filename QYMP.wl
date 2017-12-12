(* ::Package:: *)

(* ::Text:: *)
(*Qingyun Music Player*)


version="1.0.2";
userPath="C:\\Users\\"<>$UserName<>"\\AppData\\Local\\QYMP\\";
If[!DirectoryQ[userPath],CreateDirectory[userPath]];
If[!FileExistsQ[userPath<>"Default.json"],Export[userPath<>"Default.json",{"Language"->"chs"}]];
default=Association@Import[userPath<>"Default.json"];
language=default[["Language"]];
If[!FileExistsQ[userPath<>"Instrument.json"],Export[userPath<>"Instrument.json",{"Piano","Violin","Guitar","Flute"}]];
If[!DirectoryQ[userPath<>"export\\"],CreateDirectory[userPath<>"export\\"]];
path=NotebookDirectory[];
<<(path<>"meta.wl")
<<(path<>"qysParse.wl")
<<(path<>"qymParse.wl")
langData=Association@Import[path<>"Lang\\"<>language<>".json"];
tagName=Association@langData[["TagName"]];
instrName=Association@langData[["InstrumentName"]];
textInfoTags={"SongName","Lyricist","Composer","Adapter","Comment","Abstract"};
metaInfoTags={"Format","TrackCount","Duration","Instruments"};
refresh;


ModifySongInfo[song_]:=DynamicModule[{textInfo},
	textInfo=getTextInfo[song];
	CreateDialog[Column[{,
		Style[textInfo[["SongName"]],FontSize->28,Bold],,
		Grid[{tagName[[#]],Spacer[20],InputField[Dynamic@textInfo[[#]],String]}&/@textInfoTags],,
		Grid[{
			{Button["\:4fdd\:5b58\:4fee\:6539",putTextInfo[song,textInfo],ImageSize->150],
			Button["\:64a4\:9500",textInfo=getTextInfo[song],ImageSize->150]},
			{Button["\:8c03\:8bd5",DialogReturn[Debugger[song]],ImageSize->150],
			Button["\:8fd4\:56de",DialogReturn[Management],ImageSize->150]}
		}],
	},Center,ItemSize->30,Spacings->1],
	WindowTitle->"\:4fee\:6539\:6b4c\:66f2\:4fe1\:606f"];
];


Debugger[song_]:=Module[{filename,audio},DynamicModule[{playing=False,current},
	AudioStop[];
	filename=path<>"Songs\\"<>song<>"."<>index[[song,"Format"]];
	audio=parse[filename,index[[song,"Format"]]];
	current=AudioPlay[audio];AudioPause[];
	CreateDialog[Column[{,
		Grid[{
			{"\:603b\:65f6\:957f",ToString[index[[song,"Duration"]]]},
			{"\:97f3\:8f68\:6570",ToString[index[[song,"TrackCount"]]]},
			{"\:4f7f\:7528\:4e50\:5668",StringRiffle[index[[song,"Instruments"]],", "]}
		},Alignment->Left],,
		Row[{
			Dynamic@If[playing,
				Button["\:6682\:505c",AudioPause[current];playing=False,ImageSize->150],
				Button["\:64ad\:653e",AudioPlay[current];playing=True,ImageSize->150]
			],
			Spacer[20],
			Button["\:4e2d\:6b62",AudioStop[current];playing=False,ImageSize->150]
		}],
		Row[{
			Button["\:5bfc\:51fa\:6587\:4ef6",
				Export[userPath<>"export\\"<>song<>".mp3",audio];
				SystemOpen[userPath<>"export\\"],
			ImageSize->150],
			Spacer[20],
			Button["\:8fd4\:56de",DialogReturn[ModifySongInfo[song]],ImageSize->150]
		}],
	},Center,ItemSize->30,Spacings->1],
	WindowTitle->"\:8c03\:8bd5\:ff1a"<>index[[song,"SongName"]]];
]];


AddNewSong:=Module[{metaInfo,audio,song},
	DynamicModule[{songPath,textInfo=AssociationMap[""&,textInfoTags]},
		SetDirectory[path<>"Songs\\"];
		CreateDialog[Column[{,
			Style["\:6dfb\:52a0\:65b0\:66f2\:76ee",FontSize->28,Bold],,
			Row[{"\:4f4d\:7f6e",Spacer[20],PopupMenu[Dynamic@songPath,
				Complement[FileNames[],#<>"."<>index[[#,"Format"]]&/@songList],
			ImageSize->200]}],
			Grid[{tagName[[#]],Spacer[20],InputField[Dynamic@textInfo[[#]],String]}&/@textInfoTags],,
			Row[{Button["\:6dfb\:52a0",
				song=StringDrop[songPath,-4];
				audio=parse[path<>"Songs\\"<>songPath,StringTake[songPath,-3]];
				metaInfo=Values[Options[audio,MetaInformation]][[1]];
				metaInfo[["TrackCount"]]=ToString[metaInfo[["TrackCount"]]];
				metaInfo[["Duration"]]=ToString[metaInfo[["Duration"]]];
				metaInfo[["Instruments"]]=ToString[metaInfo[["Instruments"]],InputForm];
				AppendTo[index,song->metaInfo];
				putTextInfo[song,textInfo];
				DialogReturn[Management],
			ImageSize->150],
			Spacer[20],
			Button["\:8fd4\:56de",DialogReturn[Management],ImageSize->150]}],
		},Center,ItemSize->30,Spacings->1],
		WindowTitle->"\:6dfb\:52a0\:65b0\:66f2\:76ee"]
	]
];


DeleteSong[song_]:=CreateDialog[Column[{,
	"\:4f60\:786e\:5b9a\:8981\:5c06\:6b4c\:66f2\:300a"<>index[[song,"SongName"]]<>"\:300b\:4ece\:6b4c\:5355\:4e2d\:79fb\:9664\:5417\:ff1f",,
	Row[{
		Button["\:786e\:8ba4",
			index=Delete[index,song];
			DeleteFile[path<>"Meta\\"<>song<>".meta"];
			DialogReturn[Management],
		ImageSize->100],
		Spacer[20],
		Button["\:8fd4\:56de",DialogReturn[Management],ImageSize->100]			
	}],
},Center,ItemSize->36],
WindowTitle->"\:5220\:9664\:66f2\:76ee"];


Management:=(refresh;
CreateDialog[Column[{"",
	Style["\:6b4c\:5355\:7ba1\:7406",Bold,32],,
	Grid[{index[[#,"SongName"]],
		Button["\:4fee\:6539",DialogReturn[ModifySongInfo[#]],ImageSize->Tiny],
		Button["\:5220\:9664",DialogReturn[DeleteSong[#]],ImageSize->Tiny]
	}&/@songList],,
	Button["\:6dfb\:52a0\:65b0\:66f2\:76ee",DialogReturn[AddNewSong],ImageSize->150],
	Button["\:8fd4\:56de\:4e3b\:754c\:9762",DialogReturn[QYMP],ImageSize->150],
},Center,ItemSize->20],
WindowTitle->"\:6b4c\:5355\:7ba1\:7406"]);


Player[song_]:=Module[{filename,audio},DynamicModule[{playing=True,current},
	AudioStop[];
	filename=path<>"Songs\\"<>song<>"."<>index[[song,"Format"]];
	audio=parse[filename,index[[song,"Format"]]];
	current=AudioPlay[audio];
	CreateDialog[Column[{,,
		Row[{Style[index[[song,"SongName"]],Bold,28],
			If[KeyExistsQ[index[[song]],"Comment"],
				Style[" ("<>index[[song,"Comment"]]<>")",Gray,28],
				Nothing
			]
		}],,
		If[KeyExistsQ[index[[song]],"Composer"],"\:4f5c\:66f2\:ff1a"<>index[[song,"Composer"]],Nothing],
		If[KeyExistsQ[index[[song]],"Lyricist"],"\:4f5c\:8bcd\:ff1a"<>index[[song,"Lyricist"]],Nothing],
		If[KeyExistsQ[index[[song]],"Adapter"],"\:6539\:7f16\:ff1a"<>index[[song,"Adapter"]],Nothing],,
		If[KeyExistsQ[index[[song]],"Abstract"],
			Column[StringSplit[index[[song,"Abstract"]],"\n"],Left],
			Nothing
		],,
		Row[{
			Dynamic@If[playing,
				Button["\:6682\:505c",AudioPause[current];playing=False,ImageSize->80],
				Button["\:64ad\:653e",AudioPlay[current];playing=True,ImageSize->80],
			],
			Spacer[20],
			Button["\:4e2d\:6b62",AudioStop[];playing=False,ImageSize->80],
			Spacer[20],
			Button["\:8fd4\:56de",AudioStop[];DialogReturn[QYMP],ImageSize->80]			
		}],,
	},Center,ItemSize->50],
	WindowTitle->"\:6b63\:5728\:64ad\:653e\:ff1a"<>index[[song,"SongName"]]];
]];


QYMP:=DynamicModule[{song},
	refresh;
	AudioStop[];
	CreateDialog[Column[{"",
		Style["\:9752\:4e91\:64ad\:653e\:5668",Bold,32],,
		Style["\:9009\:62e9\:66f2\:76ee",Bold,24],
		SetterBar[Dynamic[song],index[[songList,"SongName"]],Appearance->"Vertical"->{Automatic,2}],,
		Button["\:64ad\:653e\:66f2\:76ee",DialogReturn[Player[song]],ImageSize->150],
		Button["\:6b4c\:5355\:7ba1\:7406",DialogReturn[Management],ImageSize->150],
		Button["\:9000\:51fa",DialogReturn[],ImageSize->150],
	},Center,ItemSize->20],
	WindowTitle->"\:9752\:4e91\:64ad\:653e\:5668"]
];


(* ::Input:: *)
(*QYMP;*)
