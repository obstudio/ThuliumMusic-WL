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


updateIndex := Block[
  {
    metaTree, songsClassified,
    playlistInfo, songList
  },
  Monitor[
    SetDirectory[localPath];
    metaTree = StringReplace["\\" -> "/"] /@ StringDrop[FileNames["*", "Meta", Infinity], 5];
    songs = StringDrop[Select[metaTree, StringMatchQ[__~~".json"]], -5];
    dirList = Select[metaTree, !StringMatchQ[#, __~~".json"]&];
    Scan[dirCreate[dataPath <> "buffer/" <> #]&, dirList];
    index = Association /@ AssociationMap[Import[localPath <> "Meta/" <> # <> ".json"]&, songs];
    DumpSave[dataPath <> "Index.mx", index];
    imageDirList = DeleteDuplicates @ Flatten[
      StringCases[dir__~~"/"~~Except["/"].. :> dir] /@ Values @ index[[All, "Image"]]
    ];
    Scan[dirCreate[dataPath <> "images/" <> #]&, imageDirList];
    playlists = Association /@ Import[localPath <> "playlist.json"];
    playlistData = <||>;
    songsClassified = {};
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
    pageData=AssociationMap[1&,Prepend[playlists,"Main"]],
    (* monitor *)
    MonitorDisplay["Constructing music index ......"]
  ]
];


updateImage := Block[
  {
    updates={}, image, filename, meta,
    imageData = Association /@ Association @ Import[dataPath <> "image.json"]
  },
  Do[
    If[KeyExistsQ[index[[song]],"Image"]&&!FileExistsQ[dataPath<>"Images/"<>index[[song,"Image"]]],
      AppendTo[updates,index[[song,"Image"]]]
    ],
  {song,songs}];
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
  DeleteFile[dataPath<>"Buffer/"<>#<>".buffer"]&/@Complement[bufferList,songs];
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
  {song,songs}];
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
    Export[dataPath<>"Buffer.json",bufferHash[[Intersection[Keys@bufferHash,songs]]]];
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


Thulium`MenuCell = Cell[BoxData @ RowBox[{
  TemplateBox[{4}, "Spacer1"],
  TemplateBox[{
    "Initialize",
    "Click to initialize the parser.",
    Unevaluated @ Thulium`InitializeParser
  }, "TextButtonMonitored"],
  TemplateBox[{4}, "Spacer1"],
  TemplateBox[{
    "Check Update",
    "Click to initialize the songs.",
    Unevaluated @ update
  }, "TextButtonMonitored"],
  TemplateBox[{4}, "Spacer1"],
  TemplateBox[{
    "Quick Start",
    "Click to start Thulium Music Player.",
    Hold @ homepage
  }, "TextButton"],
  TemplateBox[{4}, "Spacer1"]
}], "Menu", CellTags -> "$menu"];


Thulium`InitializePackage := Block[
  {paclets, packages, length},
  SetDirectory[localPath <> "library"];
  Monitor[
    paclets = FileNames["*.wl", "Paclet", Infinity];
    packages = FileNames["*.wl", "*", Infinity];
    length = Length @ packages;
    packages = Complement[packages, paclets];
    Do[Get[packages[[i]]], {i, Length @ packages}],
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
  ResetDirectory[];
];


Thulium`InitializeParser := (
  Monitor[
    If[!Thulium`$Init, Thulium`InitializePackage];
    System`JS = StartExternalSession["NodeJS"];
    ExternalEvaluate[System`JS, File[localPath <> "library/Thulium.js"]];
    DeleteObject[Drop[ExternalSessions[], -1]];
    Thulium`$Parser = True,
    MonitorDisplay["Initializing Node.js as external evaluator ......"]
  ];
  SelectionMove[First @ Cells[CellTags -> "$monitor"], After, Cell, AutoScroll -> False];
  NotebookWrite[EvaluationNotebook[], {
    Cell[BoxData @ TemplateBox[{
      RowBox[{
        "Secceed: Start External Session ",
        TemplateBox[{"(details)",
          GridBox[{
            {"System: ", FormBox[StyleBox[
              "\"" <> System`JS["System"] <> " " <> System`JS["Version"] <> "\"",
            FontFamily -> "Calibri", FontSize -> 24], "InputForm"]},
            {"Path: ", FormBox[StyleBox[
              "\"" <> StringReplace[System`JS["Executable"], "\\" -> "\\\\"] <> "\"",
            FontFamily -> "Calibri", FontSize -> 24], "InputForm"]},
            {"UUID: ", FormBox[StyleBox[
              "\"" <> Level[System`JS, 1][[1]] <> "\"",
            FontFamily -> "Calibri", FontSize -> 24], "InputForm"]}
          }, ColumnAlignments -> {Center, Center}]
        }, "TooltipTemplate"]
      }]
    }, "Tip"], "Tip", CellTags -> "Status"]
  }];
  NotebookLocate["$title"];
);
