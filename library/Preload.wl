(* ::Package:: *)

(* tags *)
textInfoTags = {"SongName", "Lyricist", "Composer", "Adapter", "Comment", "Abstract", "Origin"};
otherInfoTags = {"Image", "Uploader", "Tags"};
imageTags = {"Title", "Painter", "PainterID", "IllustID", "Source", "URL"};
aboutTags = {"Version", "Producer", "Website"};
langList = {"chs", "eng"};

(* instruments *)
instDict = Association @ Import[$LocalPath <> "library/Config/Instrument.json"];
percDict = Association @ Import[$LocalPath <> "library/Config/Percussion.json"];
instList = Keys @ instDict;
percList = Keys @ percDict;


Begin["Thulium`System`"];

$$Version::usage = "Thulium Version";
$$Commit::usage = "Git commit hash";
$$Build::usage = "Build version";
$LocalPath::usage = "Thulium Music local path";
$UserPath::usage = "Thulium Music user path";
$CloudPath::usage = "Thulium Music cloud path";
$DataPath::usage = "Thulium Music data path";
StatusAlias::usage = "StatusAlias";

Begin["`Private`"];

$$Version = "2.3";
$$Build = 701;
If[DirectoryQ[$LocalPath <> ".git"], 
  With[{ref = StringCases[Import[$LocalPath <> ".git/HEAD"], RegularExpression["^ref: (.+)$"] :> "$1"]},
    $$Commit = StringTake[Import[$LocalPath <> ".git/" <> ref], 7];
  ],
  $$Commit = "";
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

$CloudPath = "http://qymp.ob-studio.cn/assets/";
defaultDataPath = StringReplace[FileNameDrop[$BaseDirectory], "\\" -> "/"] <> "/ObStudio/QYMP/";
If[!DirectoryQ[defaultDataPath], CreateDirectory[defaultDataPath]];
userInfoTemplate=<|
  "Version" -> $$Build,
  "NodeJS" -> False,
  "Language" -> "chs",
  "Developer" -> False,
  "DataPath" -> defaultDataPath
|>;

uiSetPath := Module[{path = defaultDataPath},
  CreateDialog[Row[{
    Spacer[96],
    Column[{
      Spacer[{48,48}],
      Graphics[{logo},ImageSize->{512,Automatic}],
      Spacer[1],
      Caption[TextDict["ChooseBasePath"],"Title"],
      Row[{
        FileNameSetter[Dynamic[path],"Directory",
          Appearance -> Thulium`SmartButton`Private`SmartButtonDisplay["Browse","Default"],
          WindowTitle -> TextDict[["ChooseBasePath"]]
        ],
        Spacer[8],
        InputField[
          Dynamic[path],String,
          BaseStyle->{FontSize->20},
          ImageSize->{384,40},
          ContinuousAction->True
        ],
        Spacer[8],
        SmartButton["Tick",UserInfo[["DataPath"]]=path;DialogReturn[]]
      },ImageSize->{512,48},Alignment->Center,ImageMargins->4],
      Spacer[{48,48}]
    },Alignment->Center],
    Spacer[96]
  }],
  WindowTitle->TextDict[["BasicSettings"]],
  Background->Thulium`Assets`WindowBackground];
];

$UserPath = StringReplace[FileNameDrop[$UserBaseDirectory], {
  "\\" -> "/",
  RegularExpression["Roaming$"] -> "Local"
}] <> "/ObStudio/Thulium/";

If[!DirectoryQ[$UserPath] || !FileExistsQ[$UserPath <> "Default.json"],
  (* initial use *)
  Quiet @ CreateDirectory[$UserPath];
  UserInfo = userInfoTemplate;
  RefreshLanguage;
  uiSetPath;
  Export[$UserPath <> "Default.json", UserInfo],
  (* general case *)
  UserInfo = Association @ Import[$UserPath <> "Default.json"];
  RefreshLanguage;
  If[UserInfo["Version"] < $$Build,
    Scan[
      If[!KeyExistsQ[UserInfo, #], AppendTo[UserInfo, # -> userInfoTemplate[[#]]]]&,
      Keys @ userInfoTemplate
    ];
    UserInfo["Version"] = $$Build;
    Export[$UserPath <> "Default.json", UserInfo];
  ];
];

If[!FileExistsQ[$UserPath <> "Favorite.json"], Export[$UserPath <> "Favorite.json", {}]];
favorite = Import[$UserPath <> "Favorite.json"];

$DataPath = UserInfo["DataPath"];

End[];

End[];

DeclarePackage["Thulium`System`", {
  "$$Version", "$$Commit", "$$Build", "StatusAlias"
}]


dirCreate[path_] := If[!DirectoryQ[path], CreateDirectory[path]];
jsonCreate[path_] := If[!FileExistsQ[path], Export[path, {}]];


(* program data *)
dirCreate[$DataPath];
dirCreate[$DataPath <> "buffer/"];
dirCreate[$DataPath <> "images/"];
jsonCreate[$DataPath <> "Buffer.json"];
jsonCreate[$DataPath <> "Image.json"];
If[!FileExistsQ[$DataPath <> "Index.mx"],
  Thulium`SongIndex = <||>;
  Thulium`PlaylistIndex = <||>;
  Thulium`ImageIndex = <||>;
  Thulium`PageIndex = <|"Main" -> 1|>;
  DumpSave[$DataPath <> "Index.mx", {
    Thulium`SongIndex, Thulium`PlaylistIndex, Thulium`ImageIndex
  }],
  Get[$DataPath <> "Index.mx"];
  If[Head[Thulium`SongIndex] =!= Association, Thulium`SongIndex = <||>];
  If[Head[Thulium`ImageIndex] =!= Association, Thulium`ImageIndex = <||>];
  If[Head[Thulium`PlaylistIndex] =!= Association, Thulium`PlaylistIndex = <||>];
  Thulium`PageIndex = Prepend[
    AssociationMap[1&, Keys @ Thulium`PlaylistIndex],
    {"Main" -> 1}
  ];
];


Thulium`update`BufferHash = Association @ Import[$DataPath <> "Buffer.json"];
