(* ::Package:: *)

ModifySongInfo[song_]:=DynamicModule[{textInfo},
	textInfo=getTextInfo[song];
	CreateDialog[Column[{"",
		Style[textInfo[["SongName"]],FontSize->28,Bold],"",
		Grid[{tagName[[#]],Spacer[20],InputField[Dynamic@textInfo[[#]],String]}&/@textInfoTags],"",
		Grid[{
			{Button[display[["Save"]],putTextInfo[song,textInfo],ImageSize->150,Enabled->Dynamic[textInfo[["SongName"]]!=""]],
			Button[display[["Undo"]],textInfo=getTextInfo[song],ImageSize->150]},
			{Button[display[["Debug"]],DialogReturn[Debugger[song]],ImageSize->150,Enabled->False],
			Button[display[["Return"]],DialogReturn[Management],ImageSize->150]}
		}],""
	},Center,ItemSize->30,Spacings->1],
	WindowTitle->"\:4fee\:6539\:6b4c\:66f2\:4fe1\:606f"];
];


AddSongUI:=DynamicModule[{songPath,textInfo},
	textInfo=AssociationMap[""&,textInfoTags];
	SetDirectory[path<>"Songs\\"];
	CreateDialog[Column[{"",
		Style["\:6dfb\:52a0\:65b0\:66f2\:76ee",FontSize->28,Bold],"",
		Row[{"\:4f4d\:7f6e",Spacer[20],PopupMenu[Dynamic@songPath,
			Complement[FileNames[],#<>"."<>index[[#,"Format"]]&/@songList],
		ImageSize->200]}],
		Grid[{tagName[[#]],Spacer[20],InputField[Dynamic@textInfo[[#]],String]}&/@textInfoTags],"",
		Row[{Button[display[["Add"]],
			AddSong[songPath,textInfo];
			DialogReturn[Management],
		ImageSize->150,Enabled->Dynamic[textInfo[["SongName"]]!=""]],
		Spacer[20],
		Button[display[["Return"]],DialogReturn[Management],ImageSize->150]}],""
	},Center,ItemSize->30,Spacings->1],
	WindowTitle->"\:6dfb\:52a0\:65b0\:66f2\:76ee"]
];


(* ::Input:: *)
(*AddSong["TouHou\\Dream_Battle.qys",<|"SongName"->"\:5c11\:5973\:7eee\:60f3\:66f2","Composer"->"ZUN","Comment"->"\:5c11\:5973\:7dba\:60f3\:66f2","Lyricist"->"","Adapter"->"","Abstract"->"\:4e1c\:65b9\:6c38\:591c\:62844\:9762BOSS\n\:535a\:4e3d\:7075\:68a6\:7684\:4e3b\:9898\:66f2"|>]*)


(* ::Input:: *)
(*refresh;Column[songList]*)


AddSong[songPath_,textInfo_]:=Module[{metaInfo,audio,song},
	song=StringDrop[songPath,-4];
	AppendTo[bufferHash,song->toBase32@FileHash[path<>"Songs\\"<>songPath]];
	Export[userPath<>"Buffer.json",Normal@bufferHash];
	audio=If[StringTake[songPath,-3]=="qys",QYSParse,QYMParse][path<>"Songs\\"<>songPath];
	Export[userPath<>"Buffer\\"<>song<>".buffer",audio,"MP3"];
	metaInfo=Values[Options[audio,MetaInformation]][[1]];
	metaInfo[["TrackCount"]]=ToString[metaInfo[["TrackCount"]]];
	metaInfo[["Duration"]]=ToString[metaInfo[["Duration"]],InputForm];
	metaInfo[["Instruments"]]=ToString[metaInfo[["Instruments"]],InputForm];
	AppendTo[index,song->metaInfo];
	putTextInfo[song,textInfo];
];


Debugger[song_]:=Module[{filename,audio},DynamicModule[{playing=False,current},
	AudioStop[];
	filename=path<>"Songs\\"<>song<>"."<>index[[song,"Format"]];
	audio=parse[filename,index[[song,"Format"]]];
	current=AudioPlay[audio];AudioPause[];
	CreateDialog[Column[{"",
		Grid[{
			{"\:603b\:65f6\:957f",ToString[index[[song,"Duration"]]]},
			{"\:97f3\:8f68\:6570",ToString[index[[song,"TrackCount"]]]},
			{"\:4f7f\:7528\:4e50\:5668",StringRiffle[index[[song,"Instruments"]],", "]}
		},Alignment->Left],"",
		Row[{
			Dynamic@If[playing,
				Button[display[["Pause"]],AudioPause[current];playing=False,ImageSize->150],
				Button[display[["Play"]],AudioPlay[current];playing=True,ImageSize->150]
			],
			Spacer[20],
			Button[display[["Stop"]],AudioStop[current];playing=False,ImageSize->150]
		}],
		Row[{
			Button[display[["Export"]],
				Export[userPath<>"export\\"<>song<>".mp3",audio];
				SystemOpen[userPath<>"export\\"],
			ImageSize->150],
			Spacer[20],
			Button[display[["Return"]],DialogReturn[ModifySongInfo[song]],ImageSize->150]
		}],""
	},Center,ItemSize->30,Spacings->1],
	WindowTitle->"\:8c03\:8bd5\:ff1a"<>index[[song,"SongName"]]];
]];


DeleteSong[song_]:=CreateDialog[Column[{"",
	"\:4f60\:786e\:5b9a\:8981\:5c06\:6b4c\:66f2\:300a"<>index[[song,"SongName"]]<>"\:300b\:4ece\:6b4c\:5355\:4e2d\:79fb\:9664\:5417\:ff1f","",
	Row[{
		Button[display[["Confirm"]],
			index=Delete[index,song];
			DeleteFile[path<>"Meta\\"<>song<>".meta"];
			DialogReturn[Management],
		ImageSize->100],
		Spacer[20],
		Button[display[["Return"]],DialogReturn[Management],ImageSize->100]			
	}],""
},Center,ItemSize->36],
WindowTitle->"\:5220\:9664\:66f2\:76ee"];


Management:=Module[{},refresh;
CreateDialog[Column[{"",
	Row[{Style[display[["Manage"]],Bold,32],Style[" (\:7b2c"<>ToString[page]<>"\:9875)",Gray,32]}],"",
	Grid[{index[[#,"SongName"]],
		Button[display[["Modify"]],DialogReturn[ModifySongInfo[#]],ImageSize->Tiny],
		Button[display[["Delete"]],DialogReturn[DeleteSong[#]],ImageSize->Tiny]
	}&/@songListPaged[[page]]],"",
	Row[{
		Button[display[["PgPrev"]],DialogReturn[page--;Management],ImageSize->100,Enabled->(page>1)],
		Spacer[10],
		Button[display[["PgNext"]],DialogReturn[page++;Management],ImageSize->100,Enabled->(page<pageCount)]
	}],
	Button[display[["AddSong"]],DialogReturn[AddSongUI],ImageSize->150],
	Button[display[["Return"]],DialogReturn[QYMP],ImageSize->150],""
},Center,ItemSize->20],
WindowTitle->"\:6b4c\:5355\:7ba1\:7406"]];
