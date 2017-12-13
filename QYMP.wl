(* ::Package:: *)

(* ::Text:: *)
(*Qingyun Music Player*)


(* Manipulating user data *)
version="84";
userPath="C:\\Users\\"<>$UserName<>"\\AppData\\Local\\QYMP\\";
If[!DirectoryQ[userPath],CreateDirectory[userPath]];
If[!FileExistsQ[userPath<>"Default.json"],Export[userPath<>"Default.json",{"Version"->version,"Language"->"chs"}]];
default=Association@Import[userPath<>"Default.json"];
If[!KeyExistsQ[default,"Version"],PrependTo[default,"Version"->version];Export[userPath<>"Default.json",Normal@default]];
If[!FileExistsQ[userPath<>"Instrument.json"],Export[userPath<>"Instrument.json",{"Piano","Violin","Guitar","Flute"}]];
If[!FileExistsQ[userPath<>"Buffer.json"],Export[userPath<>"Buffer.json",{}]];
bufferHash=Association@Import[userPath<>"Buffer.json"];
If[!DirectoryQ[userPath<>"export\\"],CreateDirectory[userPath<>"export\\"]];
If[!DirectoryQ[userPath<>"Buffer\\"],CreateDirectory[userPath<>"Buffer\\"]];


(* Initialization *)
path=NotebookDirectory[];
<<(path<>"information.wl")
<<(path<>"advanced.wl")
<<(path<>"qysParse.wl")
<<(path<>"qymParse.wl")
langData=Association@Import[path<>"Lang\\"<>default[["Language"]]<>".json"];
tagName=Association@langData[["TagName"]];
instrName=Association@langData[["Instrument"]];
buttonName=Association@langData[["Button"]];
textInfoTags={"SongName","Lyricist","Composer","Adapter","Comment","Abstract"};
metaInfoTags={"Format","TrackCount","Duration","Instruments"};
refresh;


parse[song_]:=Module[{filename,hash,audio},
	filename=path<>"Songs\\"<>song<>"."<>index[[song,"Format"]];
	hash=toBase32@FileHash[filename];
	If[KeyExistsQ[bufferHash,song],
		If[bufferHash[[song]]==hash,
			Return[Import[userPath<>"Buffer\\"<>song<>".buffer","MP3"]],
			bufferHash[[song]]=hash;
		],
		AppendTo[bufferHash,song->hash];
	];
	Export[userPath<>"Buffer.json",Normal@bufferHash];
	audio=parse[filename,index[[song,"Format"]]];
	Export[userPath<>"Buffer\\"<>song<>".buffer",audio,"MP3"];
	Return[audio];
];


Player[song_]:=Module[{filename,audio},DynamicModule[{playing=True,current},
	AudioStop[];
	audio=parse[song];
	current=AudioPlay[audio];
	CreateDialog[Column[{"","",
		Row[{Style[index[[song,"SongName"]],Bold,28],
			If[KeyExistsQ[index[[song]],"Comment"],
				Style[" ("<>index[[song,"Comment"]]<>")",Gray,28],
				Nothing
			]
		}],"",
		If[KeyExistsQ[index[[song]],"Composer"],"\:4f5c\:66f2\:ff1a"<>index[[song,"Composer"]],Nothing],
		If[KeyExistsQ[index[[song]],"Lyricist"],"\:4f5c\:8bcd\:ff1a"<>index[[song,"Lyricist"]],Nothing],
		If[KeyExistsQ[index[[song]],"Adapter"],"\:6539\:7f16\:ff1a"<>index[[song,"Adapter"]],Nothing],"",
		If[KeyExistsQ[index[[song]],"Abstract"],
			Column[StringSplit[index[[song,"Abstract"]],"\n"],Left],
			Nothing
		],"",
		Row[{
			Dynamic@If[playing,
				Button[buttonName[["Pause"]],AudioPause[current];playing=False,ImageSize->80],
				Button[buttonName[["Play"]],AudioPlay[current];playing=True,ImageSize->80]
			],
			Spacer[20],
			Button[buttonName[["Stop"]],AudioStop[];playing=False,ImageSize->80],
			Spacer[20],
			Button[buttonName[["Return"]],AudioStop[];DialogReturn[QYMP],ImageSize->80]			
		}],"",""
	},Center,ItemSize->50],
	WindowTitle->"\:6b63\:5728\:64ad\:653e\:ff1a"<>index[[song,"SongName"]]];
]];


QYMP:=DynamicModule[{song,page=1},
	refresh;
	AudioStop[];
	CreateDialog[Column[{"",
		Style["\:9752\:4e91\:64ad\:653e\:5668",Bold,32],,
		Dynamic@SetterBar[Dynamic@song,
			Style[StringPadRight[#,48],24,FontFamily->"\:5fae\:8f6f\:96c5\:9ed1"]&/@index[[songList10[[page]],"SongName"]],
			Appearance->"Vertical"
		],"",
		Dynamic@Row[{
			Button[buttonName[["PgPrev"]],page--,ImageSize->200,Enabled->(page>1)],
			Spacer[10],
			Button[buttonName[["PgNext"]],page++,ImageSize->200,Enabled->(page<Length@songList10)]
		}],
		Row[{
			Button[buttonName[["PlaySong"]],DialogReturn[Player[song]],ImageSize->200],
			Spacer[10],
			Button[buttonName[["Manage"]],DialogReturn[Management],ImageSize->200]
		}],
		Row[{
			Button[buttonName[["Settings"]],DialogReturn[],ImageSize->200,Enabled->False],
			Spacer[10],
			Button[buttonName[["Exit"]],DialogReturn[],ImageSize->200]
		}],""
	},Center,ItemSize->30],
	WindowTitle->"\:9752\:4e91\:64ad\:653e\:5668"]
];


(* ::Input:: *)
(*QYMP;*)
