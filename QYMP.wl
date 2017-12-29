(* ::Package:: *)

(* ::Text:: *)
(*Qingyun Music Player*)


version=142;
userPath="C:\\Users\\"<>$UserName<>"\\AppData\\Local\\QYMP\\";
If[!DirectoryQ[userPath],CreateDirectory[userPath]];
If[!DirectoryQ[userPath<>"export\\"],CreateDirectory[userPath<>"export\\"]];
If[!DirectoryQ[userPath<>"buffer\\"],CreateDirectory[userPath<>"buffer\\"]];
template={"Version"->version,"Language"->"chs","Developer"->False};
If[!FileExistsQ[userPath<>"Default.json"],Export[userPath<>"Default.json",template]];
userInfo=Association@Import[userPath<>"Default.json"];
If[userInfo[["Version"]]<version,
	Do[
		If[!KeyExistsQ[userInfo,tag],AppendTo[userInfo,tag->template[[tag]]]],
	{tag,Keys@template}];
	userInfo[["Version"]]=version;
	Export[userPath<>"Default.json",userInfo];
];
If[!FileExistsQ[userPath<>"Instrument.json"],Export[userPath<>"Instrument.json",{"Piano","Violin","Guitar","Flute"}]];
If[!FileExistsQ[userPath<>"Buffer.json"],Export[userPath<>"Buffer.json",{}]];
bufferHash=Association@Import[userPath<>"Buffer.json"];
If[!FileExistsQ[userPath<>"Favorite.json"],Export[userPath<>"Favorite.json",{}]];
favorite=Import[userPath<>"Favorite.json"];


path=NotebookDirectory[];
<<(path<>"information.wl")
<<(path<>"developer.wl")
<<(path<>"qysParse.wl")
<<(path<>"qymParse.wl")
instrData=Association@Import[path<>"instr.json"];
langList={"chs"->"\:7b80\:4f53\:4e2d\:6587"(*,"eng"->"\:82f1\:8bed"*)};
langData=Association@Import[path<>"Lang\\"<>userInfo[["Language"]]<>".json"];
tagName=Association@langData[["TagName"]];
instrName=Association@langData[["Instrument"]];
errorDict=Association@langData[["Error"]];
display=Association@langData[["Dialog"]];
textInfoTags={"SongName","Lyricist","Composer","Adapter","Comment","Abstract"};
metaInfoTags={"Format","TrackCount","Duration","Instruments"};
refresh;


settings:=DynamicModule[{deveChoice,langChoice},
	CreateDialog[Column[{"",
		Style[display[["Settings"]],Bold,28],,
		Grid[{
		{display[["Identity"]],": ",RadioButtonBar[Dynamic@deveChoice,
			{False->display[["NormalUser"]],True->display[["Developer"]]},
			Appearance->"Horizonal"
		]},
		{display[["Language"]],": ",RadioButtonBar[Dynamic@langChoice,langList,Appearance->"Horizonal"]}}],,
		Row[{
			Button[display[["Save"]],DialogReturn[
				userInfo[["Language"]]=langChoice;
				userInfo[["Developer"]]=deveChoice;
			],ImageSize->150],
			Spacer[10],
			Button[display[["Return"]],DialogReturn[],ImageSize->150]
		}],""
	},Center,ItemSize->40],
	WindowTitle->display[["Settings"]]]
];


parse[song_]:=Module[{filename,hash,audio},
	filename=path<>"Songs\\"<>song<>"."<>index[[song,"Format"]];
	hash=toBase32@FileHash[filename];
	If[KeyExistsQ[bufferHash,song],
		If[bufferHash[[song]]==hash && FileExistsQ[userPath<>"Buffer\\"<>song<>".buffer"],
			Return[Import[userPath<>"Buffer\\"<>song<>".buffer","MP3"]],
			bufferHash[[song]]=hash;
		],
		AppendTo[bufferHash,song->hash];
	];
	Export[userPath<>"Buffer.json",Normal@bufferHash];
	If[index[[song,"Format"]]=="qys",
		audio=QYSParse[filename],
		audio=QYMParse[filename]
	];
	Export[userPath<>"Buffer\\"<>song<>".buffer",audio,"MP3"];
	Return[audio];
];


PlayerPalette[song_]:={Spacer[{40,40}],
	Row[{Style[index[[song,"SongName"]],Bold,28],
		If[KeyExistsQ[index[[song]],"Comment"],
			Style[" ("<>index[[song,"Comment"]]<>")",Gray,28],
			Nothing
		]
	}],"",
	If[KeyExistsQ[index[[song]],"Composer"],tagName[["Composer"]]<>": "<>index[[song,"Composer"]],Nothing],
	If[KeyExistsQ[index[[song]],"Lyricist"],tagName[["Lyricist"]]<>": "<>index[[song,"Lyricist"]],Nothing],
	If[KeyExistsQ[index[[song]],"Adapter"],tagName[["Adapter"]]<>": "<>index[[song,"Adapter"]],Nothing],"",
	If[KeyExistsQ[index[[song]],"Abstract"],
		Column[StringSplit[index[[song,"Abstract"]],"\n"],Left],
		Nothing
	],"",
	Row[{
		Dynamic[timeDisplay[current["Position"]]],
		Spacer[8],
		ProgressIndicator[Dynamic[current["Position"]/duration],ImageSize->{240,16}],
		Spacer[8],
		timeDisplay[duration]
	}],"",
	Row[{Button[
		Dynamic[Switch[current["State"],
			"Playing",display[["Pause"]],
			"Paused"|"Stopped",display[["Play"]]
		]],
		Switch[current["State"],
			"Playing",current["State"]="Paused",
			"Paused"|"Stopped",current["State"]="Playing"
		],
		ImageSize->80],
		Spacer[20],
		Button[display[["Stop"]],current["State"]="Stopped",ImageSize->80],
		Spacer[20],
		Button[display[["Return"]],AudioStop[];DialogReturn[QYMP[1]],ImageSize->80]			
	}],Spacer[{40,40}]
};


Player[song_]:=(
	AudioStop[];
	audio=parse[song];
	duration=Duration[audio];
	current=AudioPlay[audio];
	CreateDialog[If[KeyExistsQ[index[[song]],"Image"],
		Row[{Spacer[50],
			Column[{Spacer[{40,40}],Image[Import[path<>"Images\\"<>index[[song,"Image"]]],ImageSize->{360,Automatic}],Spacer[{40,40}]}],
			Spacer[50],
			Column[PlayerPalette[song],Alignment->Center,ItemSize->30],
		Spacer[50]},Alignment->Center],
		Column[PlayerPalette[song],Alignment->Center,ItemSize->50]
	],WindowTitle->display[["Playing"]]<>": "<>index[[song,"SongName"]]];
);


QYMP[page_]:=DynamicModule[{song},
	refresh;
	AudioStop[];
	CreateDialog[Column[{"",
		Row[{Style[display[["QYMP"]],Bold,32],Style[" (\:7b2c"<>ToString[page]<>"\:9875)",Gray,32]}],,
		SetterBar[Dynamic@song,
			#->Row[{
				Style[index[[#,"SongName"]],24,FontFamily->"\:5fae\:8f6f\:96c5\:9ed1"],
				Spacer[20],
				If[KeyExistsQ[index[[#]],"Comment"],Style[index[[#,"Comment"]],20,Gray,FontFamily->"\:5fae\:8f6f\:96c5\:9ed1"],Nothing]
			}]&/@songListPaged[[page]],
			Appearance->"Vertical"
		],"",
		Row[{
			Button[display[["PgPrev"]],DialogReturn[QYMP[page-1]],ImageSize->200,Enabled->(page>1)],
			Spacer[10],
			Button[display[["PgNext"]],DialogReturn[QYMP[page+1]],ImageSize->200,Enabled->(page<pageCount)]
		}],
		Row[{
			Button[display[["PlaySong"]],DialogReturn[Player[song]],ImageSize->200],
			Spacer[10],
			Button[display[["Manage"]],DialogReturn[Management[1]],ImageSize->200]
		}],
		Row[{
			Button[display[["Settings"]],DialogReturn[settings],ImageSize->200],
			Spacer[10],
			Button[display[["Exit"]],DialogReturn[],ImageSize->200]
		}],""
	},Center,ItemSize->50],
	WindowTitle->display[["QYMP"]]]
];


(* ::Input::Initialization:: *)
QYMP[1];


(* ::Input:: *)
(*AudioStop[];AudioPlay@QYSParse[path<>"Songs\\Gate_of_Steiner.qys"];*)


(* ::Input:: *)
(*Options[QYSParse[path<>"Songs\\Phantom_Ensemble.qys"]]*)
