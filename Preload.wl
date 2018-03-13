(* ::Package:: *)

Switch[$VersionNumber,
	11.2, StatusAlias = "State",
	11.3, StatusAlias = "Status",
	_,
		CreateDialog[{
			TextCell["Sorry, but your Mathematica isn't updated enough."],
			TextCell["Try to install Mathematica with version no less than 11.2."],
			DefaultButton[]
		}];
		Abort[];
];


Off[General::shdw];
If[Length @ FindExternalEvaluators["NodeJS"] == 0,
	CreateDialog[{
		TextCell["Thulium Music Player requires Node.js as external evaluator."],
		TextCell["Please follow the guide to install Node.js and Zeromq first."],
		DefaultButton[]
	}];
	Abort[];
];
On[General::shdw];


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
textInfoTags={"SongName","Lyricist","Composer","Adapter","Comment","Abstract","Origin"};
otherInfoTags={"Image","Uploader","Tags"};
imageTags={"Title","Painter","PainterID","IllustID","Source","URL"};
aboutTags={"Version","Producer","Website"};
langList={"chs","eng"};


playlistTemplate=<|
	"Type"->"Playlist",
	"Path"->"",
	"Title"->"",
	"Abstract"->"",
	"Comment"->"",
	"SongList"->"",
	"IndexWidth"->0
|>;


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
