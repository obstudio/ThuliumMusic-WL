(* ::Package:: *)

MonitorDisplay[content_] := Style[
  Framed[
    Pane[content,
      Scrollbars -> False,
      Alignment -> {Center, Center},
      ImageMargins -> {{4, 4}, {4, 4}},
      ImageSize -> {Dynamic @ CurrentValue[EvaluationNotebook[], WindowSize][[1]] / 2 - 200, Automatic}
    ],
    Background -> RGBColor[0.96, 0.98, 1],
    RoundingRadius -> {8, 8},
    ContentPadding -> True,
    FrameStyle -> None
  ],
  FontFamily -> "Calibri",
  FontSize -> 16
];

ProgressDisplay[items_, index_, title_] := MonitorDisplay[
  Column[{
    title,
    Graphics[progressBar[(index - 1) / Length[items], 24], ImageSize -> {400, 20}],
    Row[{
      "Loading: ", Spacer[2],
      items[[index]], Spacer[6],
      "(", index, "/", Length[items], ")"
    }]
  }, Alignment -> Center]
];

MessageDisplay[cells_] := Block[{msgCells},
  SelectionMove[First @ Cells[CellTags -> "$monitor"], After, Cell, AutoScroll -> False];
  NotebookWrite[EvaluationNotebook[], cells];
  NotebookLocate["$title"];
];

CleanMessages[maxCount_] := With[{msgCells = Cells[CellTags -> "$msg"]},
  If[Length @ msgCells > maxCount, NotebookDelete[Drop[msgCells, maxCount]]];
];

RawDisplay[text_] := FormBox[StyleBox["\"" <> text <> "\"", FontFamily -> "Calibri"], "InputForm"];


Thulium`MenuCell = Cell[BoxData @ RowBox[{(*
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
    Unevaluated @ Thulium`CheckUpdate
  }, "TextButtonMonitored"],
  TemplateBox[{4}, "Spacer1"],
  TemplateBox[{
    "Quick Start",
    "Click to start Thulium Music Player.",
    Hold[
      If[!Thulium`$Init, Thulium`InitializePackage];
      homepage;
    ]
  }, "TextButton"],
  TemplateBox[{4}, "Spacer1"]
}], "Menu", CellTags -> "$menu"];


Thulium`InitializePackage := Block[{packages},
  CleanMessages[2];
  DeclarePackage["Thulium`System`", {"UserInfo", "$UserPath", "$CloudPath", "$DataPath"}];
  SetDirectory[localPath <> "library"];
  Monitor[
    packages = Join[
      Complement[FileNames["*.mx", "Paclet", Infinity], FileNames[".*.mx", "Paclet", Infinity]],
      FileNames["*.wl", "Paclet", Infinity],
      Complement[FileNames["*.wl", "*", Infinity], FileNames["*.wl", "Paclet", Infinity]]
    ];
    Do[Get[packages[[i]]], {i, Length @ packages}],
  ProgressDisplay[packages, i, "Loading packages from library ......"]];
  Get["Preload.wl"];
  Thulium`$Init = True;
  MessageDisplay[Cell[BoxData @ TemplateBox[{
    RowBox[{
      "Succeed: Initializing Thulium Kernel ",
      TemplateBox[{"(details)",
        GridBox[{
          {"Version: ", RawDisplay[Thulium`System`$$Version]},
          If[Thulium`$Commit =!= "", {"Commit: ", RawDisplay[Thulium`System`$$Commit]}, Nothing]
        }, ColumnAlignments -> {Center, Left}, ColumnSpacings -> 0],
      0.1}, "<Tooltip>"]
    }]
  }, "SuccessMessage"], "MessageCell", CellTags -> "$msg"]];
  ResetDirectory[];
];


Thulium`InitializeParser := Block[{result, succeed, msgCells},
  If[!Thulium`$Init, Thulium`InitializePackage];
  CleanMessages[2];
  Monitor[
    Off[General::shdw];
    
    If[!Thulium`System`UserInfo[["NodeJS"]],
      If[Length @ FindExternalEvaluators["NodeJS"] == 0,
        (* FIXME: to be optimized *)
        CreateDialog[{
          TextCell["Thulium Music Player requires Node.js as external evaluator."],
          TextCell["Please follow the guide to install Node.js and Zeromq first."],
          DefaultButton[]
        }];
        Abort[],
        Thulium`System`UserInfo[["NodeJS"]] = True;
        Export[Thulium`System`$UserPath <> "Default.json", Thulium`System`UserInfo];
      ];
    ];
    
    succeed = Check[
      System`JS = StartExternalSession["NodeJS"];
      result = ExternalEvaluate[System`JS, File[localPath <> "library/Thulium.js"]];
      DeleteObject[Drop[ExternalSessions[], -1]];
      True,
      $MessageList
    ];
    
    On[General::shdw];
    Get[localPath <> "library/Adapter.wl"];
    Thulium`$Parser = True,
  MonitorDisplay["Initializing Node.js as external evaluator ......"]];
  
  MessageDisplay[If[succeed === True,
    Cell[BoxData @ TemplateBox[{
      RowBox[{
        "Secceed: Start External Session ",
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


Thulium`CheckUpdate := With[
  {
    UpdateDisplay = Function[RawDisplay[StringJoin[
      ToString[Length[#1]], " (Add: ",
      ToString[Length[#2]], ", Delete: ",
      ToString[Length[#3]], ")"
    ]]]
  },
  If[!Thulium`$Parser, Thulium`InitializeParser];
  CleanMessages[2];
  Thulium`UpdateIndex;
  Thulium`UpdateImage;
  Thulium`UpdateBuffer;
  DumpSave[$DataPath <> "Index.mx", {
    Thulium`SongIndex,
    Thulium`ImageIndex,
    Thulium`PlaylistIndex
  }];
  MessageDisplay[Cell[BoxData @ TemplateBox[{
    RowBox[{
      "Succeed: Update Music Library ",
      TemplateBox[{"(details)",
        GridBox[{
          {"Songs: ", UpdateDisplay[Thulium`SongIndex, Thulium`update`NewSongs, Thulium`update`DelSongs]},
          {"Images: ", UpdateDisplay[Thulium`ImageIndex, Thulium`update`NewImages, Thulium`update`DelImages]},
          {"Playlists: ", UpdateDisplay[Thulium`PlaylistIndex, Thulium`update`NewPlaylists, Thulium`update`DelPlaylists]}
        }, ColumnAlignments -> {Center, Left}, ColumnSpacings -> 0],
      0.1}, "<Tooltip>"]
    }]
  }, "SuccessMessage"], "MessageCell", CellTags -> "$msg"]];
];
