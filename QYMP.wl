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
songInfo=AssociationMap[Column[{
	If[KeyExistsQ[index[[#]],"comment"],#<>" ("<>index[[#,"comment"]]<>")",#],
	If[KeyExistsQ[index[[#]],"composer"],"\:4f5c\:66f2\:ff1a"<>index[[#,"composer"]],Nothing],
	If[KeyExistsQ[index[[#]],"lyricist"],"\:4f5c\:8bcd\:ff1a"<>index[[#,"lyricist"]],Nothing]
},ItemSize->30]&,songList];


QingyunPlay[song_,default_]:=Module[{filename},
	filename=path<>"Songs\\"<>song;
	If[FileExistsQ[filename],
		If[StringTake[filename,-3]=="qym",
			qymPlay[filename,default],
			qysPlay[filename]
		],
		MessageDialog[TextCell["File not found!"],WindowTitle->"Error"];
		Return[];
	]
];
QingyunPlay[]:=CreateDialog[Column[{,
	Style["\:9752\:4e91\:64ad\:653e\:5668",Bold,32],,
	Style["\:9009\:62e9\:66f2\:76ee",Bold,24],
	SetterBar[Dynamic[choice],songList,Appearance->"Vertical"->{Automatic,2}],,
	Style["\:9009\:62e9\:9ed8\:8ba4\:4e50\:5668(\:4ec5\:9650qym\:6587\:4ef6)",Bold,20],
	RadioButtonBar[Dynamic[instrument],{"Sine","Piano","Violin","Guitar"},Appearance->"Vertical"->{Automatic,2}],,
	Button["\:64ad\:653e",
		CreateDialog[songInfo[[choice]],WindowTitle->"\:6b63\:5728\:64ad\:653e..."];
		QingyunPlay[index[[choice]][["path"]],{Dynamic[instrument],1}],
	ImageSize->150],
},Center,ItemSize->20],
WindowTitle->"\:9752\:4e91\:64ad\:653e\:5668"];


(* ::Input:: *)
(*QingyunPlay[];*)
