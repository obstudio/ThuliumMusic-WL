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


player[song_]:=Module[{filename,audio,paused=False,current},
	AudioStop[];
	filename=path<>"Songs\\"<>index[[choice,"path"]];
	audio=If[StringTake[filename,-1]=="m",qymPlay,qysPlay][filename];
	current=AudioPlay[audio];
	CreateDialog[Column[{,,
		Row[{StringRepeat[" ",5],Style[song,Bold,28],
		If[KeyExistsQ[index[[song]],"comment"],
			Style[" ("<>index[[song,"comment"]]<>")",Gray,28],
			Nothing
		]}],,
		If[KeyExistsQ[index[[song]],"composer"],"\:4f5c\:66f2\:ff1a"<>index[[song,"composer"]],Nothing],
		If[KeyExistsQ[index[[song]],"lyricist"],"\:4f5c\:8bcd\:ff1a"<>index[[song,"lyricist"]],Nothing],,
		If[KeyExistsQ[index[[song]],"abstract"],
			Column[StringSplit[index[[song,"abstract"]],"\n"],Left],
			Nothing
		],,
		Button["\:4e2d\:6b62",AudioStop[],ImageSize->100],
	"","",""},Center,ItemSize->50],
	WindowTitle->"\:6b63\:5728\:64ad\:653e\:ff1a"<>song];
];
QYMP:=CreateDialog[Column[{"",
	Style["\:9752\:4e91\:64ad\:653e\:5668",Bold,32],,
	Style["\:9009\:62e9\:66f2\:76ee",Bold,24],
	SetterBar[Dynamic[choice],songList,Appearance->"Vertical"->{Automatic,2}],,
	Button["\:64ad\:653e",player[choice],ImageSize->150],
""},Center,ItemSize->20],
WindowTitle->"\:9752\:4e91\:64ad\:653e\:5668"];


(* ::Input:: *)
(*QYMP;*)
