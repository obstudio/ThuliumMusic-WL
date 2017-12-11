(* ::Package:: *)

(* ::Text:: *)
(*Qingyun Music Player*)


version="1.0.2";
userPath="C:\\Users\\"<>$UserName<>"\\AppData\\Local\\QYMP\\";
If[!DirectoryQ[userPath],CreateDirectory[userPath]];
If[!FileExistsQ[userPath<>"Instrument.json"],Export[userPath<>"Instrument.json",{"Piano","Violin","Guitar","Flute"}]];
If[!DirectoryQ[userPath<>"export\\"],CreateDirectory[userPath<>"export\\"]];
path=NotebookDirectory[];
<<(path<>"qysPlay.wl")
<<(path<>"qymPlay.wl")
index=Association/@Association@Import[path<>"Index.json"];
songList=Keys[index];
tagList={"lyricist","composer","adapter","comment","abstract"};
SetDirectory[path<>"Songs\\"];


getSongInfo[song_]:=(
	songPath=index[[song,"path"]];
	songLyricist=If[KeyExistsQ[index[[song]],"lyricist"],index[[song,"lyricist"]],""];
	songComposer=If[KeyExistsQ[index[[song]],"composer"],index[[song,"composer"]],""];
	songAdapter=If[KeyExistsQ[index[[song]],"adapter"],index[[song,"adapter"]],""];
	songComment=If[KeyExistsQ[index[[song]],"comment"],index[[song,"comment"]],""];
	songAbstract=If[KeyExistsQ[index[[song]],"abstract"],index[[song,"abstract"]],""];
);


putSongInfo[song_]:=(
	If[!KeyExistsQ[index,song],AppendTo[index,song->Null]];
	index[[song]]=<|
		If[songLyricist!="","lyricist"->songLyricist,Nothing],
		If[songComposer!="","composer"->songComposer,Nothing],
		If[songAdapter!="","adapter"->songAdapter,Nothing],
		If[songComment!="","comment"->songComment,Nothing],
		If[songAbstract!="","abstract"->songAbstract,Nothing],
		"path"->songPath
	|>;
	Export[path<>"Index.json",Normal/@Normal@index];
);


ModifySongInfo[song_]:=(
	getSongInfo[song];
	CreateDialog[Column[{,
		Style[song,FontSize->28,Bold],,
		Row[{"\:4f5c\:8bcd",Spacer[20],InputField[Dynamic@songLyricist,String]}],
		Row[{"\:4f5c\:66f2",Spacer[20],InputField[Dynamic@songComposer,String]}],
		Row[{"\:6539\:7f16",Spacer[20],InputField[Dynamic@songAdapter,String]}],
		Row[{"\:5907\:6ce8",Spacer[20],InputField[Dynamic@songComment,String]}],
		Row[{"\:6458\:8981",Spacer[20],InputField[Dynamic@songAbstract,String]}],,
		Row[{
			Button["\:4fdd\:5b58\:4fee\:6539",putSongInfo[song],ImageSize->150],
			Spacer[20],
			Button["\:64a4\:9500",getSongInfo[song],ImageSize->150]
		}],
		Row[{
			Button["\:8c03\:8bd5",DialogReturn[Debugger[song]],ImageSize->150],
			Spacer[20],
			Button["\:8fd4\:56de",DialogReturn[Management],ImageSize->150]
		}],
	},Center,ItemSize->30,Spacings->1],
	WindowTitle->"\:4fee\:6539\:300a"<>song<>"\:300b\:7684\:6b4c\:66f2\:4fe1\:606f"];
);


Debugger[song_]:=(
	songPath=index[[song,"path"]];
	audio=If[StringTake[songPath,-1]=="m",qymPlay,qysPlay][path<>"Songs\\"<>songPath];
	info=MetaInformation/.Options[audio,MetaInformation];
	playing=False;
	CreateDialog[Column[{,
		Grid[{
			{"\:603b\:65f6\:957f",ToString[info[["Duration"]]]},
			{"\:97f3\:8f68\:6570",ToString[info[["TrackCount"]]]},
			{"\:4f7f\:7528\:4e50\:5668",StringRiffle[info[["Instruments"]],", "]}
		},Alignment->Left],,
		Row[{
			Dynamic@If[playing,
				Button["\:6682\:505c",AudioPause[current];playing=False,ImageSize->150],
				Button["\:64ad\:653e",AudioPlay[current];playing=True,ImageSize->150]
			],
			StringRepeat[" ",5],
			Button["\:4e2d\:6b62",AudioStop[],ImageSize->150]
		}],
		Row[{
			Button["\:5bfc\:51fa\:6587\:4ef6",
				Export[userPath<>"export\\"<>song<>".mp3",audio];
				SystemOpen[userPath<>"export\\"],
			ImageSize->150],
			StringRepeat[" ",5],
			Button["\:8fd4\:56de",DialogReturn[ModifySongInfo[song]],ImageSize->150]
		}],
	},Center,ItemSize->30,Spacings->1],
	WindowTitle->"\:8c03\:8bd5\:ff1a"<>song];
);


AddNewSong:=(
	songName="";
	songPath="";
	songLyricist="";
	songComposer="";
	songAdapter="";
	songComment="";
	songAbstract="";
	CreateDialog[Column[{,
		Style["\:6dfb\:52a0\:65b0\:66f2\:76ee",FontSize->28,Bold],,
		Row[{"\:4f4d\:7f6e",Spacer[20],PopupMenu[Dynamic@songPath,Complement[FileNames[],index[[#,"path"]]&/@songList],ImageSize->200]}],
		Row[{"\:66f2\:540d",Spacer[20],InputField[Dynamic@songName,String]}],
		Row[{"\:4f5c\:8bcd",Spacer[20],InputField[Dynamic@songLyricist,String]}],
		Row[{"\:4f5c\:66f2",Spacer[20],InputField[Dynamic@songComposer,String]}],
		Row[{"\:6539\:7f16",Spacer[20],InputField[Dynamic@songAdapter,String]}],
		Row[{"\:5907\:6ce8",Spacer[20],InputField[Dynamic@songComment,String]}],
		Row[{"\:6458\:8981",Spacer[20],InputField[Dynamic@songAbstract,String]}],,
		Row[{Button["\:6dfb\:52a0",
			If[MemberQ[songList,songName],
				MessageDialog["\:8981\:6dfb\:52a0\:7684\:6b4c\:66f2\:4e0e\:73b0\:6709\:6b4c\:66f2\:91cd\:540d\:3002",WindowTitle->"\:8b66\:544a"],
				DialogReturn[putSongInfo[songName]];
			];
			Management,
		ImageSize->150],
		Spacer[20],
		Button["\:8fd4\:56de",DialogReturn[Management],ImageSize->150]}],
	},Center,ItemSize->30,Spacings->1],
	WindowTitle->"\:6dfb\:52a0\:65b0\:66f2\:76ee"]
);


DeleteSong[song_]:=CreateDialog[Column[{,
	"\:4f60\:786e\:5b9a\:8981\:5c06\:6b4c\:66f2\:300a"<>song<>"\:300b\:4ece\:6b4c\:5355\:4e2d\:79fb\:9664\:5417\:ff1f",,
	Row[{
		Button["\:786e\:8ba4",DialogReturn[
			index=Delete[index,Flatten@Position[songList,song]];
			Export[path<>"Index.json",Normal/@Normal@index];
			Management;
		],ImageSize->100],
		StringRepeat[" ",5],
		Button["\:8fd4\:56de",DialogReturn[Management],ImageSize->100]			
	}],
},Center,ItemSize->36],
WindowTitle->"\:5220\:9664\:66f2\:76ee"];


Management:=CreateDialog[Column[{"",
	Style["\:6b4c\:5355\:7ba1\:7406",Bold,32],,
	Grid[{#,
		Button["\:4fee\:6539",DialogReturn[ModifySongInfo[#]],ImageSize->Tiny],
		Button["\:5220\:9664",DialogReturn[DeleteSong[#]],ImageSize->Tiny]
	}&/@songList],,
	Button["\:6dfb\:52a0\:65b0\:66f2\:76ee",DialogReturn[AddNewSong],ImageSize->150],
	Button["\:8fd4\:56de\:4e3b\:754c\:9762",DialogReturn[QYMP],ImageSize->150],
},Center,ItemSize->20],
WindowTitle->"\:6b4c\:5355\:7ba1\:7406"];


Player[song_]:=(
	AudioStop[];
	filename=path<>"Songs\\"<>index[[song,"path"]];
	audio=If[StringTake[filename,-1]=="m",qymPlay,qysPlay][filename];
	current=AudioPlay[audio];
	playing=True;
	CreateDialog[Column[{,,
		Row[{Style[song,Bold,28],
			If[KeyExistsQ[index[[song]],"comment"],
				Style[" ("<>index[[song,"comment"]]<>")",Gray,28],
				Nothing
			]
		}],,
		If[KeyExistsQ[index[[song]],"composer"],"\:4f5c\:66f2\:ff1a"<>index[[song,"composer"]],Nothing],
		If[KeyExistsQ[index[[song]],"lyricist"],"\:4f5c\:8bcd\:ff1a"<>index[[song,"lyricist"]],Nothing],
		If[KeyExistsQ[index[[song]],"adapter"],"\:6539\:7f16\:ff1a"<>index[[song,"adapter"]],Nothing],,
		If[KeyExistsQ[index[[song]],"abstract"],
			Column[StringSplit[index[[song,"abstract"]],"\n"],Left],
			Nothing
		],,
		Row[{
			Dynamic@If[playing,
				Button["\:6682\:505c",AudioPause[current];playing=False,ImageSize->80],
				Button["\:64ad\:653e",AudioPlay[current];playing=True,ImageSize->80],
			],
			StringRepeat[" ",5],
			Button["\:4e2d\:6b62",AudioStop[],ImageSize->80],
			StringRepeat[" ",5],
			Button["\:8fd4\:56de",AudioStop[];DialogReturn[QYMP],ImageSize->80]			
		}],,
	},Center,ItemSize->50],
	WindowTitle->"\:6b63\:5728\:64ad\:653e\:ff1a"<>song];
);


QYMP:=CreateDialog[Column[{"",
	Style["\:9752\:4e91\:64ad\:653e\:5668",Bold,32],,
	Style["\:9009\:62e9\:66f2\:76ee",Bold,24],
	SetterBar[Dynamic[choice],songList,Appearance->"Vertical"->{Automatic,2}],,
	Button["\:64ad\:653e\:66f2\:76ee",DialogReturn[Player[choice]],ImageSize->150],
	Button["\:6b4c\:5355\:7ba1\:7406",DialogReturn[Management],ImageSize->150],
	Button["\:9000\:51fa",DialogReturn[],ImageSize->150],
},Center,ItemSize->20],
WindowTitle->"\:9752\:4e91\:64ad\:653e\:5668"];


(* ::Input:: *)
(*QYMP;*)
