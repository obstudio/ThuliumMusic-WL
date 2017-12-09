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
songInfo=AssociationMap[Column[{"","",
	Row[{Style["   \:6b63\:5728\:64ad\:653e\:ff1a",28],Style[#,Bold,28],
	If[KeyExistsQ[index[[#]],"comment"],
		Style[" ("<>index[[#,"comment"]]<>")",Gray,28],
		Nothing
	]}]
	,
	If[KeyExistsQ[index[[#]],"composer"],"     \:4f5c\:66f2\:ff1a"<>index[[#,"composer"]],Nothing],
	If[KeyExistsQ[index[[#]],"lyricist"],"     \:4f5c\:8bcd\:ff1a"<>index[[#,"lyricist"]],Nothing],
	"","",""
},ItemSize->50]&,songList];


QingyunPlay[]:=Module[{filename},
	CreateDialog[Column[{"",
		Style["\:9752\:4e91\:64ad\:653e\:5668",Bold,32],,
		Style["\:9009\:62e9\:66f2\:76ee",Bold,24],
		SetterBar[Dynamic[choice],songList,Appearance->"Vertical"->{Automatic,2}],,
		Button["\:64ad\:653e",
			CreateDialog[songInfo[[choice]],WindowTitle->"\:6b63\:5728\:64ad\:653e..."];
			filename=path<>"Songs\\"<>index[[choice,"path"]];
			If[StringTake[filename,-3]=="qym",
				qymPlay[filename],
				qysPlay[filename]
			],
		ImageSize->150],""
	},Center,ItemSize->20],
	WindowTitle->"\:9752\:4e91\:64ad\:653e\:5668"]
];


(* ::Input:: *)
(*QingyunPlay[];*)


(* ::Input:: *)
(*CloseKernels[];*)
