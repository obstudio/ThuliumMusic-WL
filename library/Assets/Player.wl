(* ::Package:: *)

BeginPackage["Thulium`Interface`Player`", {
  "Thulium`System`",
  "Thulium`Assets`",
  "Thulium`Graphics`",
  "Thulium`SmartButton`"
}];

Player::usage = "Thulium Music Player Interface";

Begin["`Private`"];

PlayerControls[audio_] := Module[
  {
    stream = AudioPlay[audio], time,
    duration = QuantityMagnitude[Duration[audio], "Seconds"]
  },
  time := QuantityMagnitude[stream["Position"], "Seconds"];
  With[{StatusAlias = StatusAlias}, Column[{
  Row[{
    Column[{Style[Dynamic[TimeDisplay[time]], 20], Spacer[1]}],
    Spacer[7],
    Magnify[EventHandler[
      Dynamic[Graphics[{progressSlider[time / duration, 16]}]],
      {"MouseDragged" :> (stream["Position"] = duration * progressLocate[16])}
    ], 3.6],
    Spacer[8],
    Column[{Style[TimeDisplay[time], 20], Spacer[1]}]
  }, ImageSize -> Full, Alignment -> Center],
  Row[{
    Module[{style = "Default"},
      Dynamic @ Switch[stream[StatusAlias],
        "Playing",EventHandler[SmartButton["Pause", style], {
          "MouseDown" :> (style = "Clicked"),
          "MouseUp" :> (style = "Default"; stream[StatusAlias] = "Paused")
        }],
        "Paused"|"Stopped",EventHandler[SmartButton["Play", style], {
          "MouseDown" :> (style = "Clicked"),
          "MouseUp" :> (style = "Default"; stream[StatusAlias] = "Playing")
        }]
      ]
    ],
    Spacer[20],
    Module[{style = "Default"},
      EventHandler[Dynamic@SmartButton["Stop",style],{
        "MouseDown" :> (style = "Clicked"),
        "MouseUp" :> (style = "Default"; stream[StatusAlias] = "Stopped"; stream["Position"] = 0)
      }]
    ],
    Spacer[20],
    Module[{style = "Default"},
      EventHandler[Dynamic@SmartButton["ArrowL", style], {
        "MouseDown" :> (style = "Clicked"),
        "MouseUp" :> (style = "Default"; AudioStop[]; DialogReturn[uiPlaylist[currentPlaylist]])
      }]
    ]   
  }, ImageSize -> {300, 60}, Alignment -> Center]
  }, Alignment -> Center]
]];

Player[song_]:=Block[
  {
    image, audio, imageExist=False, aspectRatio
  },
  Quiet @ Check[
    audio = Import[$DataPath <> "Buffer/" <> song <> ".buffer", "MP3"],
    Return[uiPlaylist[currentPlaylist]],
  Import::nffil];
  AudioStop[];
  If[SongIndex[song, "Image"] != "",
    imageExist = True;
    image = Import[$DataPath <> "Images/" <> SongIndex[song, "Image"]];
    aspectRatio = ImageAspectRatio[image];
  ];
  CreateDialog[Row[{
    If[imageExist,Row[{Spacer[48],Column[{Spacer[{40,40}],
      Tooltip[ImageEffect[Image[image,ImageSize->Piecewise[{
          {{Automatic,600},aspectRatio>2},
          {{480,Automatic},aspectRatio<1/2},
          {{Automatic,400},aspectRatio<=1&&aspectRatio>1/2},
          {{360,Automatic},aspectRatio>1&&aspectRatio<2}
        }]],{"FadedFrame"}],
        If[ImageIndex[[SongIndex[[song,"Image"]]]]!=<||>,
          Column[If[KeyExistsQ[ImageIndex[[SongIndex[[song,"Image"]]]],#],
            TagName[[#]]<>": "<>ImageIndex[[SongIndex[[song,"Image"]],#]],
            Nothing
          ]&/@imageTags],
          TextDict[["NoImageInfo"]]
        ]
      ],
    Spacer[{40,40}]}]}],Nothing],Spacer[48],
    Column[Join[{Spacer[{60,60}],
      If[SongIndex[[song,"Comment"]]!="",
        If[TextLength@SongIndex[[song,"Comment"]]>16,
          Column,Row
        ][{
          Caption[SongIndex[[song,"SongName"]],"Title"],
          Caption[" ("<>SongIndex[[song,"Comment"]]<>")","TitleCmt"]
        },Alignment->Center],
        Caption[SongIndex[[song,"SongName"]],"Title"]
      ],
      Spacer[1],
      Column[If[SongIndex[[song,#]]!="",
        Caption[TagName[[#]]<>": "<>SongIndex[[song,#]],"Text"],
        Nothing
      ]&/@{"Origin","Composer","Lyricist","Adapter"},Alignment->Center],
      Spacer[1],
      If[SongIndex[[song,"Abstract"]]!="",
        Column[Caption[#,"Text"]&/@StringSplit[SongIndex[[song,"Abstract"]],"\n"],Center],
        Nothing
      ],
      Spacer[1]},
      {PlayerControls[audio]},
      {Spacer[{60,60}]}
    ],Alignment->Center,ItemSize->Full],
  Spacer[48]},Alignment->Center,ImageSize->Full],
  Background -> WindowBackground,
  WindowTitle -> TextDict["Playing"]<>": "<>SongIndex[song,"SongName"]];
];

End[];

EndPackage[];

Thulium`Player = Thulium`Interface`Player`Player;


(* ::Input:: *)
(*AudioStop[];*)


(* ::Input:: *)
(*Thulium`Player["Touhou/Oriental_Blood"]*)
