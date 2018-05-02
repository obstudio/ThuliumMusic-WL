(* ::Package:: *)

dirCreate[path_] := If[!DirectoryQ[path], CreateDirectory[path]];
jsonCreate[path_] := If[!FileExistsQ[path], Export[path, {}]];


refreshLanguage := With[
  {langDataPath=localPath<>"language/"<>userInfo[["Language"]]<>"/"},
  tagName=Association@Import[langDataPath<>"GeneralTags.json"];
  instrName=Association@Import[langDataPath<>"Instruments.json"];
  text=Association@Import[langDataPath<>"GeneralTexts.json"];
  msgData=Association@Import[langDataPath<>"Messages.json"];
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


updateImage := Block[
  {
    updates={}, image, filename, meta,
    imageData = Association /@ Association @ Import[dataPath <> "image.json"]
  },
  Do[
    If[KeyExistsQ[Thulium`SongIndex[[song]],"Image"]&&!FileExistsQ[dataPath<>"Images/"<>Thulium`SongIndex[[song,"Image"]]],
      AppendTo[updates,Thulium`SongIndex[[song,"Image"]]]
    ],
  {song,Keys@Thulium`SongIndex}];
  If[updates=={},Return[]];
  Monitor[
    Do[
      filename=updates[[i]];
      image=Import[cloudPath<>"images/"<>filename];
      Export[dataPath<>"Images/"<>filename,image];
      meta=Association@Import[cloudPath<>"images/"<>StringReplace[filename,"."~~__->".json"]];
      If[KeyExistsQ[imageData,filename],
        imageData[[filename]]=meta,
        AppendTo[imageData,filename->meta]
      ],
    {i,Length@updates}];
    Export[dataPath <>"Image.json", imageData],
    (* monitor *)
    MonitorDisplay[Column[{
      "Downloading images from the internet ......",
      Graphics[progressBar[(i - 1) / Length[updates], 24], ImageSize -> {400, 20}],
      Row[{
        "Loading: ", Spacer[2],
        updates[[i]], Spacer[6],
        "(", i, "/", Length[updates], ")"
      }]
    }, Alignment -> Center]]
  ];
];


updateBuffer := Block[
  {
    updates={}, song, filename, hash, audio, bufferList,
    bufferHash = Association @ Import[dataPath <> "Buffer.json"]
  },
  SetDirectory[dataPath];
  bufferList=StringReplace["\\"->"/"]/@StringTake[FileNames["*.buffer","buffer",Infinity],{8,-8}];
  DeleteFile[dataPath<>"Buffer/"<>#<>".buffer"]&/@Complement[bufferList,Keys@Thulium`SongIndex];
  Do[
    filename=localPath<>"Songs/"<>song<>".tm";
    hash=IntegerString[FileHash[filename],32];
    If[KeyExistsQ[bufferHash,song],
      If[bufferHash[[song]]==hash && FileExistsQ[dataPath<>"Buffer/"<>song<>".buffer"],
        Continue[],
        bufferHash[[song]]=hash;
        AppendTo[updates,song];
      ],
      AppendTo[bufferHash,song->hash];
      AppendTo[updates,song];
    ],
  {song,Keys@Thulium`SongIndex}];
  If[updates=={},Return[]];
  Monitor[
    Do[
      song=updates[[i]];
      filename=localPath<>"Songs/"<>song<>".tm";
      Check[
        audio=AudioAdapt[Parse[filename]];
        Export[dataPath<>"Buffer/"<>song<>".buffer",audio,"MP3"],
        Delete[bufferHash, song];
      ],
    {i,Length@updates}];
    Export[dataPath<>"Buffer.json",bufferHash[[Intersection[Keys@bufferHash,Keys@Thulium`SongIndex]]]];
    ResetDirectory[],
    (* monitor *)
    MonitorDisplay[Column[{
      "Generating music buffer ......",
      Graphics[progressBar[(i - 1) / Length[updates], 24], ImageSize -> {400, 20}],
      Row[{
        "Loading: ", Spacer[2],
        updates[[i]], Spacer[6],
        "(", i, "/", Length[updates], ")"
      }]
    }, Alignment -> Center]]
  ];
];


update := (
  If[!Thulium`$Parser, Thulium`InitializeParser];
updateIndex; updateImage; updateBuffer);


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

MessageDisplay[cells_] := Block[{msgCells},
  SelectionMove[First @ Cells[CellTags -> "$monitor"], After, Cell, AutoScroll -> False];
  NotebookWrite[EvaluationNotebook[], cells];
  msgCells = Cells[CellTags -> "$msg"];
  If[Length @ msgCells > 3, NotebookDelete[Drop[msgCells, 3]]];
  NotebookLocate["$title"];
];


Thulium`MenuCell = Cell[BoxData @ RowBox[{
  TemplateBox[{4}, "Spacer1"],
  TemplateBox[{
    "Start Kernel",
    "Click to initialize the Thulium Kernel.",
    Unevaluated @ Thulium`InitializeParser
  }, "TextButtonMonitored"],
  TemplateBox[{4}, "Spacer1"],
  TemplateBox[{
    "Check Update",
    "Click to update the songs and playlists.",
    Unevaluated @ update
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


Thulium`InitializePackage := Block[{paclets, packages, length},
  SetDirectory[localPath <> "library"];
  Monitor[
    paclets = FileNames["*.wl", "Paclet", Infinity];
    packages = FileNames["*.wl", "*", Infinity];
    length = Length @ packages;
    packages = Complement[packages, paclets];
    Do[Get[packages[[i]]], {i, Length @ packages}];
    Thulium`$Init = True,
    MonitorDisplay[Column[{
      "Loading packages from library ......",
      Graphics[progressBar[(i - 1 + Length @ paclets) / length, 24], ImageSize -> {400, 20}],
      Row[{
        "Loading: ", Spacer[2],
        packages[[i]], Spacer[6],
        "(", i + Length @ paclets, "/", length, ")"
      }]
    }, Alignment -> Center]]
  ];
  Get["Preload.wl"];
  MessageDisplay[Cell[BoxData @ TemplateBox[{
    "Succeed: Initializing Thulium Kernel"
  }, "SuccessMessage"], "MessageCell", CellTags -> "$msg"]];
  ResetDirectory[];
];


Thulium`InitializeParser := Block[{result, succeed, msgCells},
  If[!Thulium`$Init, Thulium`InitializePackage];
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
  Monitor[
    Get[localPath <> "library/Adapter.wl"];
    succeed = Check[
      System`JS = StartExternalSession["NodeJS"];
      result = ExternalEvaluate[System`JS, File[localPath <> "library/Thulium.js"]];
      DeleteObject[Drop[ExternalSessions[], -1]];
      True,
      $MessageList
    ];
    Thulium`$Parser = True,
    MonitorDisplay["Initializing Node.js as external evaluator ......"]
  ];
  On[General::shdw];
  MessageDisplay[If[succeed === True,
    Cell[BoxData @ TemplateBox[{
      RowBox[{
        "Secceed: Start External Session ",
        TemplateBox[{"(details)",
          GridBox[{
            {"System: ", FormBox[StyleBox[
              "\"" <> System`JS["System"] <> " " <> System`JS["Version"] <> "\"",
            FontFamily -> "Calibri"], "InputForm"]},
            {"Path: ", FormBox[StyleBox[
              "\"" <> StringReplace[System`JS["Executable"], "\\" -> "\\\\"] <> "\"",
            FontFamily -> "Calibri"], "InputForm"]},
            {"UUID: ", FormBox[StyleBox[
              "\"" <> Level[System`JS, 1][[1]] <> "\"",
            FontFamily -> "Calibri"], "InputForm"]}
          }, ColumnAlignments -> {Center, Center}],
        0.1}, "TooltipTemplate"]
      }]
    }, "SuccessMessage"], "MessageCell", CellTags -> "$msg"],
    Cell[BoxData @ TemplateBox[{
      "Failure: Fail to start external session"
    }, "FailureMessage"], "MessageCell", CellTags -> "$msg"]
  ]];
];


updateIndex := Block[
  {
    metaTree, songList, dirList, imageDirList,
    playlists = Association /@ Import[localPath <> "playlist.json"],
    playlistData = <||>, songsClassified = {}, playlistInfo
  },
  Monitor[
    SetDirectory[localPath <> "Meta"];
    metaTree = StringReplace["\\" -> "/"] /@ FileNames["*", "", Infinity];
    ResetDirectory[];
    songList = StringDrop[Select[metaTree, StringEndsQ[".json"]], -5];
    dirList = Select[metaTree, !StringEndsQ[#, ".json"]&];
    Scan[If[!DirectoryQ[dataPath <> "buffer/" <> #], CreateDirectory[dataPath <> "buffer/" <> #]]&, dirList];
    Thulium`SongIndex = AssociationMap[Association @ Import[localPath <> "Meta/" <> # <> ".json"]&, songList];
    imageDirList = DeleteDuplicates[DeleteCases[DirectoryName /@ Values @ Thulium`SongIndex[[All, "Image"]], ""]];
    Scan[If[!DirectoryQ[dataPath <> "images/" <> #], CreateDirectory[dataPath <> "images/" <> #]]&, imageDirList];
    
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
    
    Thulium`PlaylistIndex = playlistData;
    Thulium`PageIndex = Prepend[AssociationMap[1&, Keys @ playlistData], {"Main" -> 1}];
    DumpSave[dataPath <> "Index.mx", {Thulium`SongIndex, Thulium`PlaylistIndex}],
    (* monitor *)
    MonitorDisplay["Constructing music index ......"]
  ]
];


Thulium`$Version = "2.3";
