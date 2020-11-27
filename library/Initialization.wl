(* ::Package:: *)

BeginPackage["Thulium`System`", {"Thulium`Graphics`"}];

$$Version::usage = "Thulium Music Version";
$$Commit::usage = "Thulium Music Commit Code";
$$Build::usage = "Thulium Music Build version";
$$Branch::usage = "Thulium Music Current Branch";

$LocalPath::usage = "Thulium Music Repository Path";
$UserPath::usage = "Thulium Music User Path";
$CloudPath::usage = "Thulium Music Cloud Path";
$DataPath::usage = "Thulium Music Data Path";

PageIndex::usage = "Thulium Music Page Index";
SongIndex::usage = "Thulium Music Song Index";
ImageIndex::usage = "Thulium Music Image Index";
PlaylistIndex::usage = "Thulium Music Playlist Index";

UserInfo::usage = "Thulium Music User Information";
MenuCell::usage = "Thulium Music Menu Cell";
StatusAlias::usage = "alias for \"Status\" as a property of AudioStream";

InitializePackage::usage = "initialize all packages";
InitializeParser::usage = "initialize Thulium Music parser";

Begin["`Private`"];


MenuCell = Cell[BoxData @ RowBox[{(*
  TemplateBox[{4}, "Spacer1"],
  TemplateBox[{
    "Start Kernel",
    "Click to initialize the Thulium Kernel.",
    Unevaluated @ Thulium`InitializeParser
  }, "TextButtonMonitored"],*)
  TemplateBox[{4}, "Spacer1"],
  TemplateBox[{
    "Check Update",
    "Click to update the songs and playlists.",
    Unevaluated[
      If[!$Parser, InitializeParser];
      Thulium`Update`CheckUpdate
    ]
  }, "TextButtonMonitored"],
  TemplateBox[{4}, "Spacer1"],
  TemplateBox[{
    "Quick Start",
    "Click to start Thulium Music Player.",
    Unevaluated[
      If[!$Init, InitializePackage];
      Thulium`Homepage[];
    ]
  }, "TextButtonMonitored"],
  TemplateBox[{4}, "Spacer1"]
}], "Menu", CellTags -> "$menu"];


InitializePackage := Block[{packages},
  CleanMessages[2];
  SetDirectory[$LocalPath <> "library"];
  Monitor[
    packages = Join[
      Complement[FileNames["*.mx", "Package", Infinity], FileNames[".*.mx", "Package", Infinity]],
      FileNames["*.wl", "Package", Infinity],
      Complement[FileNames["*.wl", "*", Infinity], FileNames["*.wl", "Package", Infinity]]
    ];
    Do[Get[packages[[i]]], {i, Length @ packages}],
  ProgressDisplay[packages, i, "Loading packages from library ......"]];
  Get["Preload.wl"];
  $Init = True;
  ResetDirectory[];
  MessageDisplay[Cell[BoxData @ TemplateBox[{
    RowBox[{
      "Succeed: Initializing Thulium Kernel ",
      TemplateBox[{"(details)",
        GridBox[{
          {"Version: ", RawDisplay[$$Version]},
          If[$$Commit =!= "", {"Commit: ", RawDisplay[$$Commit]}, Nothing],
          If[$$Branch =!= "", {"Branch: ", RawDisplay[$$Branch]}, Nothing]
        }, ColumnAlignments -> {Center, Left}, ColumnSpacings -> 0],
      0.1}, "<Tooltip>"]
    }]
  }, "SuccessMessage"], "MessageCell", CellTags -> "$msg"]];
];


InitializeParser := Block[{result, succeed, msgCells},
  If[!$Init, InitializePackage];
  CleanMessages[2];
  Monitor[
    Off[General::shdw];
    
    If[!UserInfo[["NodeJS"]],
      If[Length @ FindExternalEvaluators["NodeJS"] == 0,
        (* FIXME: to be optimized *)
        CreateDialog[{
          TextCell["Thulium Music Player requires Node.js as external evaluator."],
          TextCell["Please follow the guide to install Node.js and Zeromq first."],
          DefaultButton[]
        }];
        Abort[],
        UserInfo[["NodeJS"]] = True;
        Export[$UserPath <> "Default.json", UserInfo];
      ];
    ];
    
    succeed = Check[
      System`JS = StartExternalSession["NodeJS"];
      result = ExternalEvaluate[System`JS, "const Thulium = require(\"" <> $LocalPath <> "library/Thulium\")"];
      DeleteObject[Drop[ExternalSessions[], -1]];
      True,
      $MessageList
    ];
    
    On[General::shdw];
    Get[$LocalPath <> "library/Adapter.wl"];
    $Parser = True,
  MonitorDisplay["Initializing Node.js as external evaluator ......"]];
  
  If[CurrentValue[{StyleDefinitions, "<Tooltip>"}] == {}, Return[]];
  MessageDisplay[If[succeed === True,
    Cell[BoxData @ TemplateBox[{
      RowBox[{
        "Succeed: Start External Session ",
        TemplateBox[{"(details)",
          GridBox[{
            {"System: ", RawDisplay[System`JS["System"] <> " " <> System`JS["Version"]]},
            {"Path: ", RawDisplay[StringReplace[System`JS["Executable"], "\\" -> "\\\\"]]},
            {"UUID: ", RawDisplay[Level[System`JS, 1][[1]]]}
          }, ColumnAlignments -> {Center, Left}],
        0.1}, "<Tooltip>"]
      }]
    }, "SuccessMessage"], "MessageCell", CellTags -> "$msg"],
    Cell[BoxData @ TemplateBox[{
      "Failure: Fail to start external session"
    }, "FailureMessage"], "MessageCell", CellTags -> "$msg"]
  ]];
];


End[];

EndPackage[];

DeclarePackage["Thulium`System`", {
  "$UserPath", "$CloudPath", "$DataPath", "UserInfo",
  "PageIndex", "SongIndex", "ImageIndex", "PlaylistIndex"
}];
