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
      Column[{Style[TimeDisplay[duration], 20], Spacer[1]}]
    }, ImageSize -> Full, Alignment -> Center],
    Row[{
      Dynamic @ SwitchButton[stream[StatusAlias],
        {"Playing", "Pause", Hold[stream[StatusAlias] = "Paused"]},
        {"Paused", "Play", Hold[stream[StatusAlias] = "Playing"]},
        {"Stopped", "Play", Hold[stream[StatusAlias] = "Playing"]}
      ],
      Spacer[20],
      SmartButton["Stop", stream[StatusAlias] = "Stopped"; stream["Position"] = 0],
      Spacer[20],
      SmartButton["Return", AudioStop[]; DialogReturn[Thulium`Playlist[Thulium`CurrentPlaylist]]]
    }, ImageSize -> {300, 60}, Alignment -> Center]
  }, Alignment -> Center]
]];

ImageDisplay[image_] := Block[
  {source, aspectRatio},
  source = Import[$DataPath <> "Images/" <> image];
  aspectRatio = ImageAspectRatio[source];
  Row[{
    Spacer[48],
    Column[{
      Spacer[{40, 40}],
      TooltipDisplay[
        ImageEffect[Image[source, ImageSize -> Piecewise[{
          {{Automatic, 600}, aspectRatio > 2},
          {{480, Automatic}, aspectRatio < 1/2},
          {{Automatic, 400}, aspectRatio <= 1 && aspectRatio > 1/2},
          {{360, Automatic}, aspectRatio > 1 && aspectRatio < 2}
        }]], {"FadedFrame"}],
        If[ImageIndex[[image]] != <||>,
          Column[If[KeyExistsQ[ImageIndex[image], #],
            TagName[[#]] <> ": " <> ImageIndex[image, #],
            Nothing
          ]& /@ imageTags],
          TextDict["NoImageInfo"],
          TextDict["NoImageInfo"]
        ]
      ],
      Spacer[{40, 40}]
    }]
  }]
];

Player[song_] := Block[{image, audio, imageExist = False, aspectRatio},
  Quiet @ Check[
    audio = Import[$DataPath <> "Buffer/" <> song <> ".buffer", "MP3"],
    Return[Thulium`Playlist[Thulium`CurrentPlaylist]],
  Import::nffil];
  AudioStop[];
  CreateDialog[Row[{
    If[SongIndex[song, "Image"] != "",
      ImageDisplay[SongIndex[song, "Image"]],
      Nothing
    ],
    Spacer[48],
    Column[{
      Spacer[{60, 60}],
      If[SongIndex[song, "Comment"] != "",
        If[TextLength @ SongIndex[song, "Comment"] > 16, Column, Row][{
          Caption[SongIndex[song, "SongName"], "Title"],
          Caption[" (" <> SongIndex[song, "Comment"] <> ")", "TitleCmt"]
        }, Alignment -> Center],
        Caption[SongIndex[song, "SongName"], "Title"]
      ],
      Spacer[1],
      Column[If[SongIndex[song, #] != "",
        Caption[TagName[[#]] <> ": " <> SongIndex[[song, #]], "Text"],
        Nothing
      ]& /@ {"Origin", "Composer", "Lyricist", "Adapter"}, Alignment -> Center],
      Spacer[1],
      If[SongIndex[song, "Abstract"] != "",
        Column[Caption[#, "Text"]& /@ StringSplit[SongIndex[song, "Abstract"], "\n"], Center],
        Nothing
      ],
      Spacer[1],
      PlayerControls[audio],
      Spacer[{60, 60}]
    }, Alignment -> Center, ItemSize -> Full],
    Spacer[48]
  }, Alignment -> Center,ImageSize -> Full],
  Background -> WindowBackground,
  WindowTitle -> TextDict["Playing"] <> ": " <> SongIndex[song, "SongName"]];
];

End[];

EndPackage[];

Thulium`Player = Thulium`Interface`Player`Player;


(* ::Input:: *)
(*AudioStop[];*)


(* ::Input:: *)
(*Thulium`Player["Touhou/Dream_Battle"]*)
