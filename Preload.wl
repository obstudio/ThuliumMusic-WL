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


(* tags *)
textInfoTags = {"SongName", "Lyricist", "Composer", "Adapter", "Comment", "Abstract", "Origin"};
otherInfoTags = {"Image", "Uploader", "Tags"};
imageTags = {"Title", "Painter", "PainterID", "IllustID", "Source", "URL"};
aboutTags = {"Version", "Producer", "Website"};
langList = {"chs", "eng"};

(* instruments *)
instDict = Association @ Import[localPath <> "library/Data/Instrument.json"];
percDict = Association @ Import[localPath <> "library/Data/Percussion.json"];
instList = Keys @ instDict;
percList = Keys @ percDict;

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


version = 509;
cloudPath = "http://qymp.ob-studio.cn/assets/";
With[{defaultDataPath = StringReplace[FileNameDrop[$BaseDirectory], "\\" -> "/"] <> "/ObStudio/QYMP/"},
	If[!DirectoryQ[defaultDataPath], CreateDirectory[defaultDataPath]];
	userInfoTemplate=<|
		"Version" -> version,
		"NodeJS" -> False,
		"Language" -> "chs",
		"Developer" -> False,
		"Player" -> "New",
		"DataPath" -> defaultDataPath
	|>;
];


userPath = StringReplace[FileNameDrop[$UserBaseDirectory], {"\\" -> "/", "Roaming"~~EndOfString -> "Local"}] <> "/ObStudio/QYMP/";
If[!DirectoryQ[userPath],
	(* initial use *)
	CreateDirectory[userPath];
	userInfo = userInfoTemplate;
	refreshLanguage;
	uiSetPath;
	Export[userPath <> "Default.json", userInfo],
	(* general case *)
	userInfo = Association @ Import[userPath <> "Default.json"];
	refreshLanguage;
	If[userInfo[["Version"]] < version,
		Scan[
			If[!KeyExistsQ[userInfo, #], AppendTo[userInfo, # -> userInfoTemplate[[#]]]]&,
			Keys @ userInfoTemplate
		];
		userInfo[["Version"]] = version;
		Export[userPath <> "Default.json", userInfo];
	];
];
If[!FileExistsQ[userPath <> "Favorite.json"], Export[userPath <> "Favorite.json",{}]];
favorite = Import[userPath <> "Favorite.json"];


(* Find Node.js as external evaluator *)
If[!userInfo[["NodeJS"]],
	Off[General::shdw];
	If[Length @ FindExternalEvaluators["NodeJS"] == 0,
		CreateDialog[{
			TextCell["Thulium Music Player requires Node.js as external evaluator."],
			TextCell["Please follow the guide to install Node.js and Zeromq first."],
			DefaultButton[]
		}];
		Abort[],
		userInfo[["NodeJS"]] = True;
		Export[userPath <> "Default.json", userInfo];
	];
	On[General::shdw];
];


(* program data *)
dataPath = userInfo[["DataPath"]];
dirCreate[dataPath];
dirCreate[dataPath <> "buffer/"];
dirCreate[dataPath <> "images/"];
jsonCreate[dataPath <> "Buffer.json"];
jsonCreate[dataPath <> "Image.json"];
If[!FileExistsQ[dataPath <> "Index.mx"],
	index = <||>;
	DumpSave[dataPath <> "Index.mx", index],
	Get[dataPath <> "Index.mx"]
];
imageData = Association /@ Association @ Import[dataPath <> "image.json"];
