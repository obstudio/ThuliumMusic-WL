(* ::Package:: *)

(* ::Text:: *)
(*Qingyun Music Player*)


version=91;
userPath="C:\\Users\\"<>$UserName<>"\\AppData\\Local\\QYMP\\";
If[!DirectoryQ[userPath],CreateDirectory[userPath]];
If[!DirectoryQ[userPath<>"export\\"],CreateDirectory[userPath<>"export\\"]];
If[!DirectoryQ[userPath<>"buffer\\"],CreateDirectory[userPath<>"buffer\\"]];
template={"Version"->version,"Language"->"chs","Developer"->False};
If[!FileExistsQ[userPath<>"Default.json"],Export[userPath<>"Default.json",template]];
default=Association@Import[userPath<>"Default.json"];
If[default[["Version"]]<version,
	Do[
		If[!KeyExistsQ[default,tag],AppendTo[default,tag->template[[tag]]]],
	{tag,Keys@template}];
	Export[userPath<>"Default.json",default];
];
If[!FileExistsQ[userPath<>"Instrument.json"],Export[userPath<>"Instrument.json",{"Piano","Violin","Guitar","Flute"}]];
If[!FileExistsQ[userPath<>"Buffer.json"],Export[userPath<>"Buffer.json",{}]];
bufferHash=Association@Import[userPath<>"Buffer.json"];
If[!FileExistsQ[userPath<>"Favorite.json"],Export[userPath<>"Favorite.json",{}]];
favorite=Import[userPath<>"Favorite.json"];


path=NotebookDirectory[];
<<(path<>"information.wl")
<<(path<>"advanced.wl")
<<(path<>"qysParse.wl")
<<(path<>"qymParse.wl")
langList={"chs"->"\:7b80\:4f53\:4e2d\:6587","eng"->"\:82f1\:8bed"};
langData=Association@Import[path<>"Lang\\"<>default[["Language"]]<>".json"];
tagName=Association@langData[["TagName"]];
instrName=Association@langData[["Instrument"]];
buttonName=Association@langData[["Button"]];
textInfoTags={"SongName","Lyricist","Composer","Adapter","Comment","Abstract"};
metaInfoTags={"Format","TrackCount","Duration","Instruments"};
refresh;


settings:=DynamicModule[{},
	CreateDialog[Column[{"",
		Style["\:8bbe\:7f6e",Bold,28],,
		Grid[{
		{"\:4f60\:7684\:8eab\:4efd\:ff1a",RadioButtonBar[Dynamic@deveChoice,{False->"\:666e\:901a\:7528\:6237",True->"\:5f00\:53d1\:8005"},Appearance->"Horizonal"]},
		{"\:9009\:62e9\:8bed\:8a00\:ff1a",RadioButtonBar[Dynamic@langChoice,langList,Appearance->"Horizonal"]}}],,
		Row[{
			Button["\:4fdd\:5b58\:66f4\:6539",DialogReturn[],ImageSize->150,Enabled->False],
			Spacer[10],
			Button[buttonName[["Return"]],DialogReturn[],ImageSize->150]
		}],""
	},Center,ItemSize->40],
	WindowTitle->"\:8bbe\:7f6e"]
];


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


Player[song_]:=DynamicModule[{playing=True,current},
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
];


QYMP:=DynamicModule[{song,page=1},
	refresh;
	AudioStop[];
	CreateDialog[Column[{"",
		Style["\:9752\:4e91\:64ad\:653e\:5668",Bold,32],,
		Dynamic@SetterBar[Dynamic@song,
			#->Row[{
				Style[index[[#,"SongName"]],24,FontFamily->"\:5fae\:8f6f\:96c5\:9ed1"],
				Spacer[20],
				If[KeyExistsQ[index[[#]],"Comment"],Style[index[[#,"Comment"]],20,Gray,FontFamily->"\:5fae\:8f6f\:96c5\:9ed1"],Nothing]
			}]&/@songList16[[page]],
			Appearance->"Vertical"
		],"",
		Dynamic@Row[{
			Button[buttonName[["PgPrev"]],page--,ImageSize->200,Enabled->(page>1)],
			Spacer[10],
			Button[buttonName[["PgNext"]],page++,ImageSize->200,Enabled->(page<Length@songList16)]
		}],
		Row[{
			Button[buttonName[["PlaySong"]],DialogReturn[Player[song]],ImageSize->200],
			Spacer[10],
			Button[buttonName[["Manage"]],DialogReturn[Management],ImageSize->200]
		}],
		Row[{
			Button[buttonName[["Settings"]],DialogReturn[settings],ImageSize->200,Enabled->False],
			Spacer[10],
			Button[buttonName[["Exit"]],DialogReturn[],ImageSize->200]
		}],""
	},Center,ItemSize->40],
	WindowTitle->"\:9752\:4e91\:64ad\:653e\:5668"]
];


(* ::Input:: *)
(*QYMP;*)
