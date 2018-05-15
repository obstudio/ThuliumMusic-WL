(* ::Package:: *)

BeginPackage["Thulium`Update`", {
  "Thulium`System`",
  "Thulium`Assets`",
  "Thulium`Graphics`"
}];

CheckUpdate::usage = "CheckUpdate";

Begin["`Private`"];

playlistTemplate = <|
  "Type" -> "", "Path" -> "", "Title" -> "", "Abstract" -> "",
  "Comment" -> "", "SongList" -> {}, "IndexWidth" -> 0
|>;

updateDisplay[all_, add_, del_] := RawDisplay[StringJoin[
  ToString[Length[all]], " (Add: ",
  ToString[Length[add]], ", Delete: ",
  ToString[Length[del]], ")"
]];

CheckUpdate := Block[
  {
    oldSongs = Keys @ SongIndex, newSongs, delSongs,
    oldImages = Keys @ ImageIndex, newImages, delImages,
    oldPlaylists = Keys @ PlaylistIndex, newPlaylists, delPlaylists,
    metaTree, songList, dirList, 
    imageList, imageDirList,
    bufferHash, bufferList, fileName, fileHash,
    playlists = Association /@ Import[$LocalPath <> "playlist.json"],
    playlistData = <||>, songsClassified = {}, playlistInfo
  },
  
  CleanMessages[2];
  Monitor[
    SetDirectory[$LocalPath <> "Meta"];
    metaTree = StringReplace["\\" -> "/"] /@ FileNames["*", "", Infinity];
    ResetDirectory[];
    songList = StringDrop[Select[metaTree, StringEndsQ[".json"]], -5];
    dirList = Select[metaTree, !StringEndsQ[#, ".json"]&];
    Scan[If[!DirectoryQ[#], CreateDirectory[#]]&[$DataPath <> "buffer/" <> #]&, dirList];
    SongIndex = AssociationMap[Association @ Import[$LocalPath <> "Meta/" <> # <> ".json"]&, songList];
    
    SetDirectory[$DataPath];
    bufferHash = Association @ Import[$DataPath <> "Buffer.json"];
    bufferList = StringReplace["\\" -> "/"] /@ StringTake[FileNames["*.buffer", "Buffer", Infinity], {8, -8}];
    Scan[DeleteFile[# <> ".buffer"]&, Complement[ToLowerCase /@ bufferList, ToLowerCase /@ songList]];
    imageList = DeleteCases[Values @ SongIndex[[All, "Image"]], ""];
    imageDirList = DeleteDuplicates[DirectoryName /@ imageList];
    Scan[If[!DirectoryQ[#], CreateDirectory[#]]&["images/" <> #]&, imageDirList];
    (* FIXME: delete unused images *)
    ResetDirectory[];
    
    newImages = {};
    delImages = Complement[oldImages, imageList];
    Do[
      If[
        Or[
          !FileExistsQ[$DataPath <> "Images/" <> image],
          !MemberQ[oldImages, image]
        ],
        AppendTo[newImages, image]
      ],
    {image, imageList}];
    KeyDropFrom[ImageIndex, delImages];
    
    newSongs = {};
    delSongs = Complement[oldSongs, songList];
    Do[
      fileName = $LocalPath <> "Songs/" <> song <> ".tm";
      fileHash = IntegerString[FileHash[fileName], 32];
      If[
        Or[
          !KeyExistsQ[bufferHash, song],
          bufferHash[[song]] != fileHash,
          !FileExistsQ[$DataPath <> "Buffer/" <> song <> ".buffer"]
        ],
        If[!MemberQ[oldSongs, song], AppendTo[delSongs, song]];
        AppendTo[newSongs, song];
        AssociateTo[bufferHash, song -> fileHash];
      ],
    {song, songList}];
    KeyDropFrom[bufferHash, delSongs];
      
    Do[
      playlistInfo = Append[<|"Type" -> "Playlist"|>, Import[$LocalPath <> "Playlists/" <> playlist, "JSON"]];
      songList = playlistInfo[["Path"]] <> #Song &/@ Association /@ playlistInfo[["SongList"]];
      AppendTo[playlistData, {playlist -> playlistInfo}];
      AppendTo[songsClassified, songList],
    {playlist, Select[playlists, #Type == "Playlist"&][[All, "Name"]]}];
    
    Do[
      songList = Select[Keys @ SongIndex, MemberQ[SongIndex[#, "Tags"], "Touhou"]&];
      playlistInfo = ReplacePart[playlistTemplate, {
        "Type" -> "Tag",
        "Title" -> If[KeyExistsQ[TagDict, tag],
          If[KeyExistsQ[TagDict[[tag]], UserInfo[["Language"]]],
            TagDict[[tag, UserInfo[["Language"]]]],
            TagDict[[tag, TagDict[[tag, "Origin"]]]]
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
        "Title" -> TextDict[["AllSongs"]],
        "SongList" -> ({"Song" -> #}&) /@ Keys @ SongIndex
      }],
      "Unclassified"->ReplacePart[playlistTemplate,{
        "Type" -> "Class",
        "Title" -> TextDict[["Unclassified"]],
        "SongList" -> ({"Song" -> #}&) /@ Complement[Keys @ SongIndex, Flatten @ songsClassified]
      }]
    }];
    
    newPlaylists = Complement[Keys @ playlistData, oldPlaylists];
    delPlaylists = Complement[oldPlaylists, Keys @ playlistData];
    PlaylistIndex = playlistData;
    PageIndex = Prepend[AssociationMap[1&, Keys @ playlistData], {"Main" -> 1}],
  MonitorDisplay["Constructing music index ......"]];
  
  If[Length[newImages] =!= 0, Monitor[
    Do[Block[{filename, metaFileName, image},
      filename = newImages[[i]];
      metaFileName = StringReplace[filename, RegularExpression["\\.[^\\.]+$"] -> ".json"];
      image = Import[$CloudPath <> "images/" <> filename];
      Export[$DataPath <> "Images/" <> filename, image];
      AssociateTo[ImageIndex, filename -> Association @ Import[$CloudPath <> "images/" <> metaFileName]]
    ], {i, Length @ newImages}],
  ProgressDisplay[newImages, i, "Downloading images from the internet ......"]]];

  If[Length[newSongs] =!= 0, Monitor[
    Do[Block[{song, audio},
      song = newSongs[[i]];
      Check[
        audio = Thulium`AudioAdapt[Thulium`Parse[$LocalPath <> "Songs/" <> song <> ".tm"]];
        Export[$DataPath <> "Buffer/" <> song <> ".buffer", audio, "MP3"],
        KeyDropFrom[bufferHash, song];
      ]
    ], {i, Length @ newSongs}];
    Export[$DataPath <> "Buffer.json", bufferHash[[
      Intersection[Keys @ bufferHash, Keys @ SongIndex]
    ]]],
  ProgressDisplay[newSongs, i, "Generating music buffer ......"]]];
  
  DumpSave[$DataPath <> "Index.mx", {SongIndex, ImageIndex, PlaylistIndex}];
  
  MessageDisplay[Cell[BoxData @ TemplateBox[{
    RowBox[{
      "Succeed: Update Music Library ",
      TemplateBox[{"(details)",
        GridBox[{
          {"Songs: ", updateDisplay[SongIndex, newSongs, delSongs]},
          {"Images: ", updateDisplay[ImageIndex, newImages, delImages]},
          {"Playlists: ", updateDisplay[PlaylistIndex, newPlaylists, delPlaylists]}
        }, ColumnAlignments -> {Center, Left}, ColumnSpacings -> 0],
      0.1}, "<Tooltip>"]
    }]
  }, "SuccessMessage"], "MessageCell", CellTags -> "$msg"]];
];

End[];

EndPackage[];
