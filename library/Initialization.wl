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
  SetDirectory[localPath <> "library"];
  Monitor[
    packages = Join[
      Complement[FileNames["*.wl", "Paclet", Infinity], FileNames[".*.wl", "Paclet", Infinity]],
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
          {"Version: ", RawDisplay[Thulium`$Version]},
          If[Thulium`$Commit =!= "", {"Commit: ", RawDisplay[Thulium`$Commit]}, Nothing]
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
    
    If[!userInfo[["NodeJS"]],
      If[Length @ FindExternalEvaluators["NodeJS"] == 0,
        (* FIXME: to be optimized *)
        CreateDialog[{
          TextCell["Thulium Music Player requires Node.js as external evaluator."],
          TextCell["Please follow the guide to install Node.js and Zeromq first."],
          DefaultButton[]
        }];
        Abort[],
        userInfo[["NodeJS"]] = True;
        Export[userPath <> "Default.json", userInfo];
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


playlistTemplate = <|
  "Type" -> "", "Path" -> "", "Title" -> "", "Abstract" -> "",
  "Comment" -> "", "SongList" -> {}, "IndexWidth" -> 0
|>;

Thulium`UpdateIndex := Block[
  {
    oldSongs = Keys @ Thulium`SongIndex,
    oldImages = Keys @ Thulium`ImageIndex,
    oldPlaylists = Keys @ Thulium`PlaylistIndex,
    metaTree, songList, dirList, 
    imageList, imageDirList,
    bufferList, fileName, fileHash,
    playlists = Association /@ Import[localPath <> "playlist.json"],
    playlistData = <||>, songsClassified = {}, playlistInfo
  },
  
  Monitor[
    SetDirectory[localPath <> "Meta"];
    metaTree = StringReplace["\\" -> "/"] /@ FileNames["*", "", Infinity];
    ResetDirectory[];
    songList = StringDrop[Select[metaTree, StringEndsQ[".json"]], -5];
    dirList = Select[metaTree, !StringEndsQ[#, ".json"]&];
    Scan[If[!DirectoryQ[#], CreateDirectory[#]]&[dataPath <> "buffer/" <> #]&, dirList];
    Thulium`SongIndex = AssociationMap[Association @ Import[localPath <> "Meta/" <> # <> ".json"]&, songList];
    
    SetDirectory[dataPath];
    bufferList = StringReplace["\\" -> "/"] /@ StringTake[FileNames["*.buffer", "Buffer", Infinity], {8, -8}];
    Scan[DeleteFile[# <> ".buffer"]&, Complement[ToLowerCase /@ bufferList, ToLowerCase /@ songList]];
    imageList = DeleteCases[Values @ Thulium`SongIndex[[All, "Image"]], ""];
    imageDirList = DeleteDuplicates[DirectoryName /@ imageList];
    Scan[If[!DirectoryQ[#], CreateDirectory[#]]&["images/" <> #]&, imageDirList];
    (* FIXME: delete unused images *)
    ResetDirectory[];
    
    Thulium`update`NewImages = {};
    Thulium`update`DelImages = Complement[oldImages, imageList];
    Do[
      If[
        Or[
          !FileExistsQ[dataPath <> "Images/" <> image],
          !MemberQ[oldImages, image]
        ],
        AppendTo[Thulium`update`NewImages, image]
      ],
    {image, imageList}];
    KeyDropFrom[Thulium`ImageIndex, Thulium`update`DelImages];
    
    Thulium`update`NewSongs = {};
    Thulium`update`DelSongs = Complement[oldSongs, songList];
    Do[
      fileName = localPath <> "Songs/" <> song <> ".tm";
      fileHash = IntegerString[FileHash[fileName], 32];
      If[
        Or[
          !KeyExistsQ[Thulium`update`BufferHash, song],
          Thulium`update`BufferHash[[song]] != fileHash,
          !FileExistsQ[dataPath <> "Buffer/" <> song <> ".buffer"]
        ],
        If[!MemberQ[oldSongs, song], AppendTo[Thulium`update`DelSongs, song]];
        AppendTo[Thulium`update`NewSongs, song];
        AssociateTo[Thulium`update`BufferHash, song -> fileHash];
      ],
    {song, songList}];
    KeyDropFrom[Thulium`update`BufferHash, Thulium`update`DelSongs];
      
    Do[
      playlistInfo = Append[<|"Type" -> "Playlist"|>, Import[localPath <> "Playlists/" <> playlist, "JSON"]];
      songList = playlistInfo[["Path"]] <> #Song &/@ Association /@ playlistInfo[["SongList"]];
      AppendTo[playlistData, {playlist -> playlistInfo}];
      AppendTo[songsClassified, songList],
    {playlist, Select[playlists, #Type == "Playlist"&][[All, "Name"]]}];
    
    Do[
      songList = Select[Keys @ Thulium`SongIndex, MemberQ[Thulium`SongIndex[#, "Tags"], "Touhou"]&];
      playlistInfo = ReplacePart[playlistTemplate, {
        "Type" -> "Tag",
        "Title" -> If[KeyExistsQ[tagDict, tag],
          If[KeyExistsQ[tagDict[[tag]], userInfo[["Language"]]],
            tagDict[[tag, userInfo[["Language"]]]],
            tagDict[[tag, tagDict[[tag, "Origin"]]]]
          ],
          tag
        ],
        "SongList" -> ({"Song" -> #}&) /@ songList
      }];
      AppendTo[playlistData, {tag -> playlistInfo}];
      AppendTo[songsClassified, songList],
    {tag, Select[playlists, #Type == "Tag"&][[All, "Name"]]}];
    
    PrependTo[playlistData, {
      "All" -> ReplacePart[playlistTemplate, {
        "Type" -> "Class",
        "Title" -> text[["AllSongs"]],
        "SongList" -> ({"Song" -> #}&) /@ Keys @ Thulium`SongIndex
      }],
      "Unclassified"->ReplacePart[playlistTemplate,{
        "Type" -> "Class",
        "Title" -> text[["Unclassified"]],
        "SongList" -> ({"Song" -> #}&) /@ Complement[Keys @ Thulium`SongIndex, Flatten @ songsClassified]
      }]
    }];
    
    Thulium`update`NewPlaylists = Complement[Keys @ playlistData, oldPlaylists];
    Thulium`update`DelPlaylists = Complement[oldPlaylists, Keys @ playlistData];
    Thulium`PlaylistIndex = playlistData;
    Thulium`PageIndex = Prepend[AssociationMap[1&, Keys @ playlistData], {"Main" -> 1}],
  MonitorDisplay["Constructing music index ......"]];
];


Thulium`UpdateImage := With[{updates = Thulium`update`NewSongs}, Block[{filename, metaFileName},
  If[Length[updates] === 0, Return[]];
  Monitor[
    Do[
      filename = updates[[i]];
      metaFileName = StringReplace[filename, RegularExpression["\\.[^\\.]+$"] -> ".json"];
      CopyFile[cloudPath <> "images/" <> filename, dataPath <> "Images/" <> filename];
      AssociateTo[Thulium`ImageIndex, filename -> Association @ Import[cloudPath <> "images/" <> metaFileName]],
    {i, Length @ updates}],
  ProgressDisplay[updates, i, "Downloading images from the internet ......"]];
]];


Thulium`UpdateBuffer := With[{updates = Thulium`update`NewSongs}, Block[{song, audio},
  If[Length[updates] === 0, Return[]];
  Monitor[
    Do[
      song = updates[[i]];
      Check[
        audio = AudioAdapt[Parse[localPath <> "Songs/" <> song <> ".tm"]];
        Export[dataPath <> "Buffer/" <> song <> ".buffer", audio, "MP3"],
        KeyDropFrom[Thulium`update`BufferHash, song];
      ],
    {i, Length @ updates}];
    Export[dataPath <> "Buffer.json", Thulium`update`BufferHash[[
      Intersection[Keys @ Thulium`update`BufferHash, Keys @ Thulium`SongIndex]
    ]]],
  ProgressDisplay[updates, i, "Generating music buffer ......"]];
]];


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
  DumpSave[dataPath <> "Index.mx", {
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
