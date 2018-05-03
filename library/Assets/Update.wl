(* ::Package:: *)

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
          If[KeyExistsQ[tagDict[[tag]], UserInfo[["Language"]]],
            tagDict[[tag, UserInfo[["Language"]]]],
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
        "Title" -> TextDict[["AllSongs"]],
        "SongList" -> ({"Song" -> #}&) /@ Keys @ Thulium`SongIndex
      }],
      "Unclassified"->ReplacePart[playlistTemplate,{
        "Type" -> "Class",
        "Title" -> TextDict[["Unclassified"]],
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
