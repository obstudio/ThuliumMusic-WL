(* ::Package:: *)

ModifySongInfo[song_]:=DynamicModule[{textInfo},
	textInfo=getTextInfo[song];
	CreateDialog[Column[{"",
		Style[textInfo[["SongName"]],FontSize->28,Bold],"",
		Grid[{tagName[[#]],Spacer[20],InputField[Dynamic@textInfo[[#]],String]}&/@textInfoTags],"",
		Grid[{
			{Button[buttonName[["Save"]],putTextInfo[song,textInfo],ImageSize->150],
			Button[buttonName[["Undo"]],textInfo=getTextInfo[song],ImageSize->150]},
			{Button[buttonName[["Debug"]],DialogReturn[Debugger[song]],ImageSize->150],
			Button[buttonName[["Return"]],DialogReturn[Management],ImageSize->150]}
		}],""
	},Center,ItemSize->30,Spacings->1],
	WindowTitle->"\:4fee\:6539\:6b4c\:66f2\:4fe1\:606f"];
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
				Button[buttonName[["Pause"]],AudioPause[current];playing=False,ImageSize->150],
				Button[buttonName[["Play"]],AudioPlay[current];playing=True,ImageSize->150]
			],
			Spacer[20],
			Button[buttonName[["Stop"]],AudioStop[current];playing=False,ImageSize->150]
		}],
		Row[{
			Button[buttonName[["Export"]],
				Export[userPath<>"export\\"<>song<>".mp3",audio];
				SystemOpen[userPath<>"export\\"],
			ImageSize->150],
			Spacer[20],
			Button[buttonName[["Return"]],DialogReturn[ModifySongInfo[song]],ImageSize->150]
		}],""
	},Center,ItemSize->30,Spacings->1],
	WindowTitle->"\:8c03\:8bd5\:ff1a"<>index[[song,"SongName"]]];
]];


AddNewSong:=Module[{metaInfo,audio,song},
	DynamicModule[{songPath,textInfo=AssociationMap[""&,textInfoTags]},
		SetDirectory[path<>"Songs\\"];
		CreateDialog[Column[{"",
			Style["\:6dfb\:52a0\:65b0\:66f2\:76ee",FontSize->28,Bold],"",
			Row[{"\:4f4d\:7f6e",Spacer[20],PopupMenu[Dynamic@songPath,
				Complement[FileNames[],#<>"."<>index[[#,"Format"]]&/@songList],
			ImageSize->200]}],
			Grid[{tagName[[#]],Spacer[20],InputField[Dynamic@textInfo[[#]],String]}&/@textInfoTags],"",
			Row[{Button[buttonName[["Add"]],
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
			Button[buttonName[["Return"]],DialogReturn[Management],ImageSize->150]}],""
		},Center,ItemSize->30,Spacings->1],
		WindowTitle->"\:6dfb\:52a0\:65b0\:66f2\:76ee"]
	]
];


DeleteSong[song_]:=CreateDialog[Column[{"",
	"\:4f60\:786e\:5b9a\:8981\:5c06\:6b4c\:66f2\:300a"<>index[[song,"SongName"]]<>"\:300b\:4ece\:6b4c\:5355\:4e2d\:79fb\:9664\:5417\:ff1f","",
	Row[{
		Button[buttonName[["Confirm"]],
			index=Delete[index,song];
			DeleteFile[path<>"Meta\\"<>song<>".meta"];
			DialogReturn[Management],
		ImageSize->100],
		Spacer[20],
		Button[buttonName[["Return"]],DialogReturn[Management],ImageSize->100]			
	}],""
},Center,ItemSize->36],
WindowTitle->"\:5220\:9664\:66f2\:76ee"];


Management:=(refresh;
CreateDialog[Column[{"",
	Style[buttonName[["Manage"]],Bold,32],"",
	Grid[{index[[#,"SongName"]],
		Button[buttonName[["Modify"]],DialogReturn[ModifySongInfo[#]],ImageSize->Tiny],
		Button[buttonName[["Delete"]],DialogReturn[DeleteSong[#]],ImageSize->Tiny]
	}&/@songList],"",
	Button[buttonName[["AddSong"]],DialogReturn[AddNewSong],ImageSize->150],
	Button[buttonName[["Return"]],DialogReturn[QYMP],ImageSize->150],""
},Center,ItemSize->20],
WindowTitle->"\:6b4c\:5355\:7ba1\:7406"]);
