(* ::Package:: *)

Thulium`$Version = "2.3";
If[DirectoryQ[localPath <> ".git"], 
  With[{ref = StringCases[Import[localPath <> ".git/HEAD"], RegularExpression["^ref: (.+)$"] :> "$1"]},
    Thulium`$Commit = StringTake[Import[localPath <> ".git/" <> ref], 7];
  ],
  Thulium`$Commit = "";
];


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


dirCreate[path_] := If[!DirectoryQ[path], CreateDirectory[path]];
jsonCreate[path_] := If[!FileExistsQ[path], Export[path, {}]];


refreshLanguage := With[
  {langDataPath=localPath<>"language/"<>userInfo[["Language"]]<>"/"},
  tagName=Association@Import[langDataPath<>"GeneralTags.json"];
  instrName=Association@Import[langDataPath<>"Instruments.json"];
  text=Association@Import[langDataPath<>"GeneralTexts.json"];
  msgData=Association@Import[langDataPath<>"Messages.json"];
];


(* tags *)
textInfoTags = {"SongName", "Lyricist", "Composer", "Adapter", "Comment", "Abstract", "Origin"};
otherInfoTags = {"Image", "Uploader", "Tags"};
imageTags = {"Title", "Painter", "PainterID", "IllustID", "Source", "URL"};
aboutTags = {"Version", "Producer", "Website"};
langList = {"chs", "eng"};

(* instruments *)
instDict = Association @ Import[localPath <> "library/Config/Instrument.json"];
percDict = Association @ Import[localPath <> "library/Config/Percussion.json"];
instList = Keys @ instDict;
percList = Keys @ percDict;

(* local data *)
colorData=Association@Import[localPath<>"library/Assets/color.json"];                               (* colors *)
styleColor=RGBColor/@Association@colorData[["StyleColor"]];
buttonColor=RGBColor/@#&/@Association/@Association@colorData[["ButtonColor"]];
pageSelectorColor=RGBColor/@#&/@Association/@Association@colorData[["PageSelectorColor"]];
styleData=Association/@Association@Import[localPath<>"library/Assets/style.json"];                  (* styles *)
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
defaultDataPath = StringReplace[FileNameDrop[$BaseDirectory], "\\" -> "/"] <> "/ObStudio/QYMP/";
If[!DirectoryQ[defaultDataPath], CreateDirectory[defaultDataPath]];
userInfoTemplate=<|
	"Version" -> version,
	"NodeJS" -> False,
	"Language" -> "chs",
	"Developer" -> False,
	"Player" -> "New",
	"DataPath" -> defaultDataPath
|>;


userPath = StringReplace[FileNameDrop[$UserBaseDirectory], {"\\" -> "/", "Roaming"~~EndOfString -> "Local"}] <> "/ObStudio/Thulium/";
If[!DirectoryQ[userPath] || !FileExistsQ[userPath <> "Default.json"],
	(* initial use *)
	Quiet @ CreateDirectory[userPath];
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
If[!FileExistsQ[userPath <> "Favorite.json"], Export[userPath <> "Favorite.json", {}]];
favorite = Import[userPath <> "Favorite.json"];
If[!FileExistsQ[userPath <> "WorkBench.nb"], Export[userPath <> "WorkBench.nb", WorkBenchTemplate]];


(* program data *)
dataPath = userInfo[["DataPath"]];
dirCreate[dataPath];
dirCreate[dataPath <> "buffer/"];
dirCreate[dataPath <> "images/"];
jsonCreate[dataPath <> "Buffer.json"];
jsonCreate[dataPath <> "Image.json"];
If[!FileExistsQ[dataPath <> "Index.mx"],
  Thulium`SongIndex = <||>;
  Thulium`PlaylistIndex = <||>;
  Thulium`ImageIndex = <||>;
  Thulium`PageIndex = <|"Main" -> 1|>;
  DumpSave[dataPath <> "Index.mx", {
    Thulium`SongIndex, Thulium`PlaylistIndex, Thulium`ImageIndex
  }],
  Get[dataPath <> "Index.mx"];
  Thulium`PageIndex = Prepend[
    AssociationMap[1&, Keys @ Thulium`PlaylistIndex],
    {"Main" -> 1}
  ];
];


Thulium`update`BufferHash = Association @ Import[dataPath <> "Buffer.json"];
