(* ::Package:: *)

uiModifySong[song_]:=DynamicModule[{textInfo},
	textInfo=Thulium`SongIndex[[song,textInfoTags]];
	CreateDialog[Column[{Spacer[{20,20}],
		caption[textInfo[["SongName"]],"Title"],
		Spacer[1],
		Grid[{Spacer[40],
			caption[tagName[[#]],"Text"],
			Spacer[2],
			InputField[Dynamic@textInfo[[#]],String],
		Spacer[40]}&/@textInfoTags,Alignment->Right],
		Spacer[1],
		Grid[{
			{Button[text[["Save"]],putTextInfo[song,textInfo],ImageSize->150,Enabled->Dynamic[textInfo[["SongName"]]!=""]],
			Button[text[["Undo"]],textInfo=Thulium`SongIndex[[song,textInfoTags]],ImageSize->150]},
			{Button[text[["DeleteSong"]],DialogReturn[uiDeleteSong[song]],ImageSize->150],
			Button[text[["Return"]],DialogReturn[refresh;uiPlaylist["All"]],ImageSize->150]}
		}],Spacer[{20,20}]
	},Center,ItemSize->Full,Spacings->1],
	Background->styleColor[["Background"]],WindowTitle->text[["ModifySong"]]];
];


(* ::Input:: *)
(*uiModifySong["Anima"];*)


putTextInfo[song_,textInfo_]:=Module[
	{info=Thulium`SongIndex[[song]]},
	Do[
		info[[tag]]=textInfo[[tag]],
	{tag,textInfoTags}];
	Thulium`SongIndex[[song]]=info;
	Export[localPath<>"Meta/"<>song<>".json",Thulium`SongIndex[[song]]];
];


tagTemplate=<|"Image"->"","Uploader"->"","Tags"->{}|>;
addSong[songPath_,textInfo_]:=Module[{song,metaInfo,audio},
	song=StringDelete[songPath,RegularExpression["\\.\\w+$"]];
	AppendTo[Thulium`SongIndex,song->Join[textInfo,tagTemplate]];
	putTextInfo[song,textInfo];
];


(* ::Input:: *)
(*uiAddSong;*)


ignoreList={"test.qys"};
uiAddSong:=DynamicModule[{songPath,textInfo,candidates},
	textInfo=AssociationMap[""&,textInfoTags];
	SetDirectory[localPath];
	candidates=Complement[StringDrop[StringReplace["\\"->"/"]/@FileNames["*.tm","Songs",Infinity],6],
		#<>".tm"&/@Keys@Thulium`SongIndex,
		ignoreList
	];
	CreateDialog[Column[{Spacer[{40,40}],
		caption["_AddSong","Title"],
		Spacer[4],
		Row[{text[["SongPath"]],Spacer[12],PopupMenu[Dynamic@songPath,candidates,ImageSize->200]}],
		Column[Row[{Spacer[40],
			caption[tagName[[#]],"Text"],
			Spacer[16],
			InputField[Dynamic@textInfo[[#]],String],
		Spacer[40]}]&/@textInfoTags],
		Spacer[4],
		Row[{Button[text[["Add"]],addSong[songPath,textInfo];DialogReturn[homepage],
		ImageSize->150,Enabled->Dynamic[textInfo[["SongName"]]!=""]],
		Spacer[20],
		Button[text[["Return"]],DialogReturn[refresh;uiPlaylist["All"]],ImageSize->150]}],
	Spacer[{40,40}]},Center,ItemSize->Full,Spacings->1],
	Background->styleColor[["Background"]],WindowTitle->text[["AddSong"]]]
];


uiDeleteSong[song_]:=CreateDialog[Column[{"",
	text[["SureToRemove"]],"",
	Row[{
		Button[text[["Confirm"]],
			Thulium`SongIndex=Delete[Thulium`SongIndex,song];
			DeleteFile[localPath<>"Meta/"<>song<>".json"];
			DialogReturn[refresh;uiPlaylist["All"]],
		ImageSize->100],
		Spacer[20],
		Button[text[["Return"]],DialogReturn[uiModifySong[song]],ImageSize->100]			
	}],""
},Center,ItemSize->36],
Background->styleColor[["Background"]],WindowTitle->text[["DeleteSong"]]];
