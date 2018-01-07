(* ::Package:: *)

uiModifySong[song_]:=DynamicModule[{textInfo},
	textInfo=getTextInfo[song];
	CreateDialog[Column[{"",
		Style[textInfo[["SongName"]],FontSize->28,Bold],"",
		Grid[{tagName[[#]],Spacer[20],InputField[Dynamic@textInfo[[#]],String]}&/@textInfoTags],"",
		Grid[{
			{Button[display[["Save"]],putTextInfo[song,textInfo],ImageSize->150,Enabled->Dynamic[textInfo[["SongName"]]!=""]],
			Button[display[["Undo"]],textInfo=getTextInfo[song],ImageSize->150]},
			{Button[display[["Delete"]],DialogReturn[uiDeleteSong[song]],ImageSize->150],
			Button[display[["Return"]],DialogReturn[QYMP],ImageSize->150]}
		}],""
	},Center,ItemSize->30,Spacings->1],
	WindowTitle->"\:4fee\:6539\:6b4c\:66f2\:4fe1\:606f"];
];


ignoreList={"temp.qys","test.qys"};
uiAddSong:=DynamicModule[{songPath,textInfo,candidates},
	textInfo=AssociationMap[""&,textInfoTags];
	candidates=Complement[StringDrop[FileNames["*.qys"|"*.qym","Songs",Infinity],6],
		#<>"."<>index[[#,"Format"]]&/@songList,
		ignoreList
	];
	CreateDialog[Column[{"",
		Style["\:6dfb\:52a0\:65b0\:66f2\:76ee",FontSize->28,Bold],"",
		Row[{"\:4f4d\:7f6e",Spacer[20],PopupMenu[Dynamic@songPath,candidates,ImageSize->200]}],
		Grid[{tagName[[#]],Spacer[20],InputField[Dynamic@textInfo[[#]],String]}&/@textInfoTags],"",
		Row[{Button[display[["Add"]],
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
			DialogReturn[QYMP],
		ImageSize->150,Enabled->Dynamic[textInfo[["SongName"]]!=""]],
		Spacer[20],
		Button[display[["Return"]],DialogReturn[QYMP],ImageSize->150]}],""
	},Center,ItemSize->30,Spacings->1],
	WindowTitle->"\:6dfb\:52a0\:65b0\:66f2\:76ee"]
];


uiDeleteSong[song_]:=CreateDialog[Column[{"",
	"\:4f60\:786e\:5b9a\:8981\:5c06\:6b4c\:66f2\:300a"<>index[[song,"SongName"]]<>"\:300b\:4ece\:6b4c\:5355\:4e2d\:79fb\:9664\:5417\:ff1f","",
	Row[{
		Button[display[["Confirm"]],
			index=Delete[index,song];
			DeleteFile[path<>"Meta\\"<>song<>".meta"];
			DialogReturn[QYMP],
		ImageSize->100],
		Spacer[20],
		Button[display[["Return"]],DialogReturn[QYMP],ImageSize->100]			
	}],""
},Center,ItemSize->36],
WindowTitle->"\:5220\:9664\:66f2\:76ee"];
