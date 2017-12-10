(* ::Package:: *)

(* ::Text:: *)
(*Qingyun Music Player*)


version="1.0.2";
userPath="C:\\Users\\"<>$UserName<>"\\AppData\\Local\\QYMP\\";
If[!DirectoryQ[userPath],
	CreateDirectory[userPath];
	Export[userPath<>"Instrument.json",{"Piano","Violin","Guitar","Flute"}];
];
path=NotebookDirectory[];
<<(path<>"qysPlay.wl")
<<(path<>"qymPlay.wl")
index=Association/@Association@Import[path<>"Index.json"];
songList=Keys[index];


ModifySongInfo[song_]:=Module[{},
	songPath=index[[song,"path"]];
	songLyricist=If[KeyExistsQ[index[[song]],"lyricist"],index[[song,"lyricist"]],""];
	songComposer=If[KeyExistsQ[index[[song]],"composer"],index[[song,"composer"]],""];
	songComment=If[KeyExistsQ[index[[song]],"comment"],index[[song,"comment"]],""];
	songAbstract=If[KeyExistsQ[index[[song]],"abstract"],index[[song,"abstract"]],""];
	CreateDialog[Column[{,
		Style[song,FontSize->28,Bold],,
		Row[{"\:4f5c\:8bcd  ",InputField[Dynamic@songLyricist,String]}],
		Row[{"\:4f5c\:66f2  ",InputField[Dynamic@songComposer,String]}],
		Row[{"\:5907\:6ce8  ",InputField[Dynamic@songComment,String]}],
		Row[{"\:6458\:8981  ",InputField[Dynamic@songAbstract,String]}],,
		Row[{Button["\:4fdd\:5b58\:4fee\:6539",
			index[[song]]=<|
				If[songLyricist!="","lyricist"->songLyricist,Nothing],
				If[songComposer!="","composer"->songComposer,Nothing],
				If[songComment!="","comment"->songComment,Nothing],
				If[songAbstract!="","abstract"->songAbstract,Nothing],
				"path"->songPath
			|>;
			Export[path<>"Index.json",Normal/@Normal@index],
		ImageSize->150],
		Spacer[20],
		Button["\:8fd4\:56de",DialogReturn[Management],ImageSize->150]}],
	},Center,ItemSize->30,Spacings->1],
	WindowTitle->"\:4fee\:6539\:300a"<>song<>"\:300b\:7684\:6b4c\:66f2\:4fe1\:606f"];
];
Management:=CreateDialog[Column[{"",
	Style["\:6b4c\:5355\:7ba1\:7406",Bold,32],,
	Grid[{#,Button["\:4fee\:6539",DialogReturn[ModifySongInfo[#]],ImageSize->Tiny]}&/@songList],,
	Button["\:8fd4\:56de",DialogReturn[QYMP],ImageSize->150],
},Center,ItemSize->20],
WindowTitle->"\:6b4c\:5355\:7ba1\:7406"];
Player[song_]:=Module[{filename,audio,paused=False,current},
	AudioStop[];
	filename=path<>"Songs\\"<>index[[song,"path"]];
	audio=If[StringTake[filename,-1]=="m",qymPlay,qysPlay][filename];
	current=AudioPlay[audio];
	CreateDialog[Column[{,,
		Row[{Style[song,Bold,28],
			If[KeyExistsQ[index[[song]],"comment"],
				Style[" ("<>index[[song,"comment"]]<>")",Gray,28],
				Nothing
			]
		}],,
		If[KeyExistsQ[index[[song]],"composer"],"\:4f5c\:66f2\:ff1a"<>index[[song,"composer"]],Nothing],
		If[KeyExistsQ[index[[song]],"lyricist"],"\:4f5c\:8bcd\:ff1a"<>index[[song,"lyricist"]],Nothing],,
		If[KeyExistsQ[index[[song]],"abstract"],
			Column[StringSplit[index[[song,"abstract"]],"\n"],Left],
			Nothing
		],,
		Row[{
			Button["\:4e2d\:6b62",AudioStop[],ImageSize->100],
			StringRepeat[" ",5],
			Button["\:8fd4\:56de",AudioStop[];DialogReturn[QYMP],ImageSize->100]			
		}],,
	},Center,ItemSize->50],
	WindowTitle->"\:6b63\:5728\:64ad\:653e\:ff1a"<>song];
];
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
