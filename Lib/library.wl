(* ::Package:: *)

(* path and template *)
version=281;
cloudPath="http://qymp.ob-studio.cn/assets/";
dataPathTemplate="C:\\ProgramData\\obstudio\\QYMP\\";
userPath=StringReplace[$HomeDirectory,"\\"->"/"]<>"/AppData/Local/obstudio/QYMP/";
If[!DirectoryQ[dataPathTemplate],CreateDirectory[dataPathTemplate]];
userTemplate=<|
	"Version"->version,
	"Language"->"chs",
	"Developer"->False,
	"Player"->"Old",
	"DataPath"->dataPathTemplate
|>;


(* instruments *)
instDict=Association@Import[localPath<>"src\\Instrument.json"];
percDict=Association@Import[localPath<>"src\\Percussion.json"];
instList=Keys@instDict;
percList=Keys@percDict;


(* tags *)
metaInfoTags={"SectionCount","RealTrackCount","Duration","Instruments"};
textInfoTags={"SongName","Lyricist","Composer","Adapter","Comment","Abstract","Origin"};
otherInfoTags={"Format","Image","Uploader","Tags"};
imageTags={"Title","Painter","PainterID","IllustID","Source","URL"};
aboutTags={"Version","Producer","Website"};
langList={"chs","eng"};


(* some functions *)
textLength[string_]:=2StringLength[string]-StringCount[string,Alternatives@CharacterRange[32,127]];
toBase32[n_]:=StringDelete[ToString@BaseForm[n,32],"\n"~~__];
timeDisplay[t_]:=Module[
	{sec=Floor[QuantityMagnitude[UnitConvert[t,"Seconds"]]]},
	IntegerString[Floor[sec/60],10,2]<>":"<>IntegerString[Mod[sec,60],10,2]
];
completeText[raw_,arg_]:=StringReplace[raw,{
	"&"~~i:int:>ToString[arg[[ToExpression@i]],FormatType->InputForm],
	"$"~~i:int:>"\""<>arg[[ToExpression@i]]<>"\"",
	"#"~~i:int:>StringRiffle[ToString[#,FormatType->InputForm]&/@arg[[ToExpression@i]],", "]
}];
caption[string_String]:=caption[string,"None",{}];
caption[string_String,argument_List]:=caption[string,"None",argument];
caption[string_String,style_String]:=caption[string,style,{}];
caption[string_String,style_String,argument_List]:=Style[completeText[Which[
	StringLength@string>0&&StringPart[string,1]=="_",text[[StringDrop[string,1]]],
	True,string
],argument],styleDict[[style]]];
getArgument[string_,function_]:=Switch[function,
	"Instr",{string},
	"Chord",ToExpression/@StringSplit[string,","],
	_,ToExpression[string]
];


playlistTemplate=<|
	"Type"->"Playlist",
	"Path"->"",
	"Title"->"",
	"Abstract"->"",
	"Comment"->"",
	"SongList"->"",
	"IndexWidth"->0
|>;


(* ::Text:: *)
(*Refresh & Update*)


refreshLanguage:=Module[{langDataPath},
	langDataPath=localPath<>"Lang\\"<>userInfo[["Language"]]<>"\\";
	tagName=Association@Import[langDataPath<>"GeneralTags.json"];
	instrName=Association@Import[langDataPath<>"Instruments.json"];
	text=Association@Import[langDataPath<>"GeneralTexts.json"];
	msgData=Association@Import[langDataPath<>"Messages.json"];
];


refresh:=Block[
	{
		metaTree,songsClassified,
		playlistInfo,songList
	},
	SetDirectory[localPath];
	metaTree=StringDrop[FileNames["*","Meta",Infinity],5];
	songs=StringDrop[Select[metaTree,StringMatchQ[__~~".json"]],-5];
	dirList=Select[metaTree,!StringMatchQ[#,__~~".json"]&];
	Do[
		If[!DirectoryQ[dataPath<>"buffer\\"<>dir],CreateDirectory[dataPath<>"buffer\\"<>dir]],
	{dir,dirList}];
	index=Association/@AssociationMap[Import[localPath<>"Meta\\"<>#<>".json"]&,songs];
	imageDirList=DeleteDuplicates@Flatten[
		StringCases[dir__~~"\\"~~Except["\\"]..:>dir]/@Values@index[[songs,"Image"]]
	];
	Do[
		If[!DirectoryQ[dataPath<>"image\\"<>dir],CreateDirectory[dataPath<>"image\\"<>dir]],
	{dir,imageDirList}];
	playlists=Association/@Import[localPath<>"playlist.json"];
	playlistData=<||>;
	songsClassified={};
	Do[
		playlistInfo=Append[Association@Import[localPath<>"Playlists\\"<>playlist,"JSON"],<|"Type"->"Playlist"|>];
		songList=#Song&/@Association/@playlistInfo[["SongList"]];
		AppendTo[playlistData,<|playlist->playlistInfo|>];
		AppendTo[songsClassified,playlistInfo[["Path"]]<>#&/@songList],
	{playlist,Select[playlists,#Type=="Playlist"&][[All,"Name"]]}];
	Do[
		songList=Keys@Select[Normal@index,MemberQ[Values[#][["Tags"]],tag]&];
		playlistInfo=ReplacePart[playlistTemplate,{
			"Type"->"Tag",
			"Title"->If[KeyExistsQ[tagDict,tag],
				If[KeyExistsQ[tagDict[[tag]],userInfo[["Language"]]],
					tagDict[[tag,userInfo[["Language"]]]],
					tagDict[[tag,tagDict[[tag,"Origin"]]]]
				],
			tag],
			"SongList"->({"Song"->#}&)/@songList
		}];
		AppendTo[playlistData,<|tag->playlistInfo|>];
		AppendTo[songsClassified,playlistInfo[["Path"]]<>#&/@songList],
	{tag,Select[playlists,#Type=="Tag"&][[All,"Name"]]}];
	playlists=Join[{"All","Unclassified"},playlists[[All,"Name"]]];
	PrependTo[playlistData,{
		"All"->ReplacePart[playlistTemplate,{
			"Type"->"Class","Title"->text[["AllSongs"]],"SongList"->({"Song"->#}&/@songs)
		}],
		"Unclassified"->ReplacePart[playlistTemplate,{
			"Type"->"Class","Title"->text[["Unclassified"]],
			"SongList"->({"Song"->#}&/@Complement[songs,Flatten@songsClassified])
		}]
	}];
	pageData=AssociationMap[1&,Prepend[playlists,"Main"]];
];


(* ::Input:: *)
(*refresh;*)


updateImage:=Block[{updates={},image,filename,meta},
	Do[
		If[KeyExistsQ[index[[song]],"Image"]&&!FileExistsQ[dataPath<>"Images\\"<>index[[song,"Image"]]],
			AppendTo[updates,index[[song,"Image"]]]
		],
	{song,songs}];
	If[updates=={},Return[]];
	Monitor[Do[
		filename=updates[[i]];
		image=Import[cloudPath<>"images/"<>StringReplace[filename,"\\"->"/"]];
		Export[dataPath<>"Images\\"<>filename,image];
		meta=Association@Import[cloudPath<>"images/"<>StringReplace[filename,{"\\"->"/","."~~__->".json"}]];
		If[KeyExistsQ[imageData,filename],
			imageData[[filename]]=meta,
			AppendTo[imageData,filename->meta]
		],
	{i,Length@updates}],
	Panel[Column[{Spacer[{4,4}],
		text[["UpdatingImage"]],
		Spacer[1],
		ProgressIndicator[i,{1,Length@updates},ImageSize->{320,16}],
		Spacer[1],
		Row[{
			caption["_Progression","Text",{i,Length@updates}],
			Spacer[4],text[["Loading"]],
			updates[[i]]
		}],
	Spacer[{4,4}]},Alignment->Center],ImageSize->400,Alignment->Center]];
	Export[dataPath<>"Image.json",Normal/@Normal@imageData];
];


updateBuffer:=Block[{updates={},song,filename,hash,audio,bufferList},
	SetDirectory[dataPath];
	bufferList=StringTake[FileNames["*.buffer","buffer",Infinity],{8,-8}];
	DeleteFile[dataPath<>"Buffer\\"<>#<>".buffer"]&/@Complement[bufferList,songs];
	Do[
		filename=localPath<>"Songs\\"<>song<>"."<>index[[song,"Format"]];
		hash=toBase32@FileHash[filename];
		If[KeyExistsQ[bufferHash,song],
			If[bufferHash[[song]]==hash && FileExistsQ[dataPath<>"Buffer\\"<>song<>".buffer"],
				Continue[],
				AppendTo[updates,song];
			],
			AppendTo[bufferHash,song->hash];
			AppendTo[updates,song];
		],
	{song,songs}];
	If[updates=={},Return[]];
	Monitor[Do[
		song=updates[[i]];
		filename=localPath<>"Songs\\"<>song<>"."<>index[[song,"Format"]];
		bufferHash[[song]]=toBase32@FileHash[filename];
		If[index[[song,"Format"]]=="qys",
			audio=QYSParse[filename],
			audio=AudioAdapt[Parse[filename]]
		];
		Export[dataPath<>"Buffer\\"<>song<>".buffer",audio,"MP3"],
	{i,Length@updates}],
	CreateWindow[Column[{Spacer[{4,4}],
		text[["UpdatingBuffer"]],
		Spacer[1],
		ProgressIndicator[i,{1,Length@updates},ImageSize->{320,16}],
		Spacer[1],
		Row[{
			caption["_Progression","Text",{i,Length@updates}],
			Spacer[4],text[["Loading"]],
			index[[updates[[i]],"SongName"]]
		}],
	Spacer[{4,4}]},Alignment->Center],ImageSize->{400,300}]];
	Export[dataPath<>"Buffer.json",Normal@bufferHash[[Intersection[Keys@bufferHash,songs]]]];
];
