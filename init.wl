(* ::Package:: *)

(* ::Subsubsection:: *)
(*Packages Initialization*)


initJS := (
	If[Length@FindExternalEvaluators["NodeJS"]==0,
		CreateDialog[{
			TextCell["Thulium Music Player requires Node.js as external evaluator."],
			TextCell["Please follow the guide to install Node.js and Zeromq first."],
			DefaultButton[]
		}];
		Abort[];
	];
	System`JS = StartExternalSession["NodeJS"];
	ExternalEvaluate[JS, File[localPath<>"library/Parser/Parser.js"]];
	ExternalEvaluate[JS, "const fs = require('fs')"];
	ExternalEvaluate[JS, "
		function Parse(filePath) {
			const data = JSON.parse(fs.readFileSync(filePath, 'utf8'))
			return new Parser(data).parse()
		}
	"];
	DeleteObject[Drop[ExternalSessions[],-1]];
);


(* ::Input:: *)
(*initJS;*)


(* path and template *)
version=281;
cloudPath="http://qymp.ob-studio.cn/assets/";
defaultDataPath=StringReplace[FileNameDrop[$BaseDirectory],"\\"->"/"]<>"/ObStudio/QYMP/";
userPath=StringReplace[FileNameDrop[$UserBaseDirectory],{"\\"->"/","Roaming"~~EndOfString->"Local"}]<>"/ObStudio/QYMP/";
If[!DirectoryQ[defaultDataPath],CreateDirectory[defaultDataPath]];
userTemplate=<|
	"Version"->version,
	"Language"->"chs",
	"Developer"->False,
	"Player"->"New",
	"DataPath"->defaultDataPath
|>;


(* instruments *)
instDict=Association@Import[localPath<>"library/Data/Instrument.json"];
percDict=Association@Import[localPath<>"library/Data/Percussion.json"];
instList=Keys@instDict;
percList=Keys@percDict;


(* tags *)
metaInfoTags={"SectionCount","RealTrackCount","Duration","Instruments"};
textInfoTags={"SongName","Lyricist","Composer","Adapter","Comment","Abstract","Origin"};
otherInfoTags={"Image","Uploader","Tags"};
imageTags={"Title","Painter","PainterID","IllustID","Source","URL"};
aboutTags={"Version","Producer","Website"};
langList={"chs","eng"};


(* some functions *)
textLength[string_]:=2StringLength[string]-StringCount[string,Alternatives@CharacterRange[32,127]];
timeDisplay[time_]:=Block[
	{sec=Floor[QuantityMagnitude[UnitConvert[time,"Seconds"]]]},
	IntegerString[Floor[sec/60],10,2]<>":"<>IntegerString[Mod[sec,60],10,2]
];
completeText[raw_,arg_]:=StringReplace[raw,{
	"&"~~i:DigitCharacter:>ToString[arg[[ToExpression@i]],FormatType->InputForm],
	"$"~~i:DigitCharacter:>"\""<>arg[[ToExpression@i]]<>"\"",
	"#"~~i:DigitCharacter:>StringRiffle[ToString[#,FormatType->InputForm]&/@arg[[ToExpression@i]],", "]
}];
caption[string_String]:=caption[string,"None",{}];
caption[string_String,argument_List]:=caption[string,"None",argument];
caption[string_String,style_String]:=caption[string,style,{}];
caption[string_String,style_String,argument_List]:=Style[completeText[Which[
	StringLength@string>0&&StringPart[string,1]=="_",text[[StringDrop[string,1]]],
	True,string
],argument],styleDict[[style]]];


playlistTemplate=<|
	"Type"->"Playlist",
	"Path"->"",
	"Title"->"",
	"Abstract"->"",
	"Comment"->"",
	"SongList"->"",
	"IndexWidth"->0
|>;


refreshLanguage:=Module[{langDataPath},
	langDataPath=localPath<>"language/"<>userInfo[["Language"]]<>"/";
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
	metaTree=StringReplace["\\"->"/"]/@StringDrop[FileNames["*","Meta",Infinity],5];
	songs=StringDrop[Select[metaTree,StringMatchQ[__~~".json"]],-5];
	dirList=Select[metaTree,!StringMatchQ[#,__~~".json"]&];
	Do[
		If[!DirectoryQ[dataPath<>"buffer/"<>dir],CreateDirectory[dataPath<>"buffer/"<>dir]],
	{dir,dirList}];
	index=Association/@AssociationMap[Import[localPath<>"Meta/"<>#<>".json"]&,songs];
	imageDirList=DeleteDuplicates@Flatten[
		StringCases[dir__~~"/"~~Except["/"]..:>dir]/@Values@index[[songs,"Image"]]
	];
	Do[
		If[!DirectoryQ[dataPath<>"images/"<>dir],CreateDirectory[dataPath<>"images/"<>dir]],
	{dir,imageDirList}];
	playlists=Association/@Import[localPath<>"playlist.json"];
	playlistData=<||>;
	songsClassified={};
	Do[
		playlistInfo=Append[Association@Import[localPath<>"Playlists/"<>playlist,"JSON"],<|"Type"->"Playlist"|>];
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
		If[KeyExistsQ[index[[song]],"Image"]&&!FileExistsQ[dataPath<>"Images/"<>index[[song,"Image"]]],
			AppendTo[updates,index[[song,"Image"]]]
		],
	{song,songs}];
	If[updates=={},Return[]];
	Monitor[Do[
		filename=updates[[i]];
		image=Import[cloudPath<>"images/"<>filename];
		Export[dataPath<>"Images/"<>filename,image];
		meta=Association@Import[cloudPath<>"images/"<>StringReplace[filename,"."~~__->".json"]];
		If[KeyExistsQ[imageData,filename],
			imageData[[filename]]=meta,
			AppendTo[imageData,filename->meta]
		],
	{i,Length@updates}],
	Panel[Column[{Spacer[{4,4}],
		text[["UpdatingImage"]],
		Spacer[1],
		Row[{Graphics@{progressBar[(i-1)/Length@updates,24]}},ImageSize->{400,20},Alignment->Center],
		Spacer[1],
		Row[{
			caption["_Progression","Text",{i,Length@updates}],
			Spacer[4],text[["Loading"]],
			updates[[i]]
		}],
	Spacer[{4,4}]},Alignment->Center],ImageSize->{400,Full},Alignment->Center]];
	Export[dataPath<>"Image.json",Normal/@Normal@imageData];
];


updateBuffer:=Block[{updates={},song,filename,hash,audio,bufferList},
	SetDirectory[dataPath];
	bufferList=StringReplace["\\"->"/"]/@StringTake[FileNames["*.buffer","buffer",Infinity],{8,-8}];
	DeleteFile[dataPath<>"Buffer/"<>#<>".buffer"]&/@Complement[bufferList,songs];
	Do[
		filename=localPath<>"Songs/"<>song<>".tm";
		hash=IntegerString[FileHash[filename],32];
		If[KeyExistsQ[bufferHash,song],
			If[bufferHash[[song]]==hash && FileExistsQ[dataPath<>"Buffer/"<>song<>".buffer"],
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
		filename=localPath<>"Songs/"<>song<>".tm";
		bufferHash[[song]]=IntegerString[FileHash[filename],32];
		audio=AudioAdapt[Parse[filename]];
		Export[dataPath<>"Buffer/"<>song<>".buffer",audio,"MP3"],
	{i,Length@updates}],
	Panel[Column[{Spacer[{4,4}],
		text[["UpdatingBuffer"]],
		Spacer[1],
		Row[{Graphics@{progressBar[(i-1)/Length@updates,24]}},ImageSize->{400,20},Alignment->Center],
		Spacer[1],
		Row[{
			caption["_Progression","Text",{i,Length@updates}],
			Spacer[4],text[["Loading"]],
			index[[updates[[i]],"SongName"]]
		}],
	Spacer[{4,4}]},Alignment->Center],ImageSize->{400,Full},Alignment->Center]];
	Export[dataPath<>"Buffer.json",Normal@bufferHash[[Intersection[Keys@bufferHash,songs]]]];
];


update:=(
	refresh;
	updateImage;
	updateBuffer;
	$Updated=True;
	homepage;
);
$Updated=False;


(* local data *)
colorData=Association@Import[localPath<>"Assets/color.json"];                               (* colors *)
styleColor=RGBColor/@Association@colorData[["StyleColor"]];
buttonColor=RGBColor/@#&/@Association/@Association@colorData[["ButtonColor"]];
pageSelectorColor=RGBColor/@#&/@Association/@Association@colorData[["PageSelectorColor"]];
styleData=Association/@Association@Import[localPath<>"Assets/style.json"];                  (* styles *)
styleDict=Normal@Module[{outcome={}},
	If[KeyExistsQ[#,"Size"],AppendTo[outcome,FontSize->#[["Size"]]]];
	If[KeyExistsQ[#,"Family"],AppendTo[outcome,FontFamily->#[["Family"]]]];
	If[KeyExistsQ[#,"Weight"],AppendTo[outcome,FontWeight->ToExpression@#[["Weight"]]]];
	If[KeyExistsQ[#,"Color"],AppendTo[outcome,FontColor->styleColor[[#[["Color"]]]]]];
outcome]&/@styleData;
langDict=Association@Import[localPath<>"language/Languages.json"];                      (* languages *)
tagDict=Association/@Association@Import[localPath<>"Tags.json"];


(* user data *)
If[!DirectoryQ[userPath],
	CreateDirectory[userPath];
	userInfo=userTemplate;
	refreshLanguage;
	uiSetPath;
	Export[userPath<>"Default.json",userInfo],
	userInfo=Association@Import[userPath<>"Default.json"];
	refreshLanguage;
	If[userInfo[["Version"]]<version,
		Do[
			If[!KeyExistsQ[userInfo,tag],AppendTo[userInfo,tag->userTemplate[[tag]]]],
		{tag,Keys@userTemplate}];
		userInfo[["Version"]]=version;
		Export[userPath<>"Default.json",userInfo];
	];
];
If[!FileExistsQ[userPath<>"Favorite.json"],Export[userPath<>"Favorite.json",{}]];
favorite=Import[userPath<>"Favorite.json"];


(* program data *)
dataPath=userInfo[["DataPath"]];
If[!DirectoryQ[dataPath],CreateDirectory[dataPath]];
If[!DirectoryQ[dataPath<>"buffer/"],CreateDirectory[dataPath<>"buffer/"]];
If[!DirectoryQ[dataPath<>"images/"],CreateDirectory[dataPath<>"images/"]];
If[!FileExistsQ[dataPath<>"Buffer.json"],Export[dataPath<>"Buffer.json",{}]];
bufferHash=Association@Import[dataPath<>"Buffer.json"];
If[!FileExistsQ[dataPath<>"Image.json"],Export[dataPath<>"Image.json",{}]];
imageData=Association/@Association@Import[dataPath<>"image.json"];
