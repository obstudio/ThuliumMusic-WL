(* ::Package:: *)

Begin["Thulium`System`"];

Begin["`Private`"];

$$Version = "2.5";
$$Build = 756;
If[DirectoryQ[$LocalPath <> ".git"], 
  With[{ref = StringCases[Import[$LocalPath <> ".git/HEAD"], RegularExpression["^ref: (.+)$"] :> "$1"][[1]]},
    $$Commit = StringTake[Import[$LocalPath <> ".git/" <> ref], 7];
    $$Branch = StringCases[ref, RegularExpression["/([a-zA-Z0-9-]+)$"] :> "$1"][[1]];
  ],
  $$Commit = "";
  $$Branch = "";
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
  "ListLength" -> 10,
  "Looping" -> False,
  "DataPath" -> defaultDataPath
|>;

uiSetPath := Module[{path = defaultDataPath},
  CreateDialog[Row[{
    Spacer[96],
    Column[{
      Spacer[{48,48}],
      Graphics[{
        RGBColor["#00A0E9"],
        FilledCurve[{BezierCurve[Thulium`Assets`LogoCloud]}],
        RGBColor["#FFFFFF"],
        FilledCurve[{BezierCurve[Thulium`Assets`LogoNote]}]
      },ImageSize->{512,Automatic}],
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

dirCreate[path_] := If[!DirectoryQ[path], CreateDirectory[path]];
jsonCreate[path_] := If[!FileExistsQ[path], Export[path, {}]];
(* program data *)
dirCreate[$DataPath];
dirCreate[$DataPath <> "buffer/"];
dirCreate[$DataPath <> "images/"];
jsonCreate[$DataPath <> "Buffer.json"];
jsonCreate[$DataPath <> "Image.json"];
If[!FileExistsQ[$DataPath <> "Index.mx"],
  SongIndex = <||>;
  PlaylistIndex = <||>;
  ImageIndex = <||>;
  PageIndex = <|"Main" -> 1|>;
  DumpSave[$DataPath <> "Index.mx", {
    SongIndex, PlaylistIndex, ImageIndex
  }],
  Get[$DataPath <> "Index.mx"];
  If[Head[SongIndex] =!= Association, SongIndex = <||>];
  If[Head[ImageIndex] =!= Association, ImageIndex = <||>];
  If[Head[PlaylistIndex] =!= Association, PlaylistIndex = <||>];
  PageIndex = Prepend[
    AssociationMap[1&, Keys @ PlaylistIndex],
    {"Main" -> 1}
  ];
];

End[];

End[];

