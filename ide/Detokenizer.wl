(* ::Package:: *)


Detoken[input_]:=
Block[{Whitespace,FUNCTION,Comments,Undefined,Library,Settings,Sections},
Whitespace="Content"/.#&;
FUNCTION[function_]:=
Block[{KeyOct,BarBeat,Spd,Dur,Oct,Vol,FadeIn,FadeOut},
KeyOct="(1="<>#<>")"&;
BarBeat="("<>ToString[#1]<>"/"<>ToString[#2]<>")"&;
Spd="("<>ToString[#]<>")"&;
Dur="(Dur:"<>ToString[#]<>")"&;
Oct="(Oct:"<>ToString[#]<>")"&;
Vol="("<>ToString[#]<>"%)"&;
FadeIn="(FadeIn:"<>ToString[#]<>")"&;
FadeOut="(FadeOut:"<>ToString[#]<>")"&;
If["Simplified"/.function,
ToExpression["Name"/.function]@@("Content"/.#&/@("Argument"/.function)),
("Name"/.function)<>"("<>("Content"/.#&/@("Argument"/.function))<>")"(**)
]
];

Comments[comments_]:=comments;
(*Undefined[undefined_]:=undefined;*)
(*Library[library_]:=library;*)
(*Settings[settings_]:=settings;*)

Sections[sections_List]:=
Block[{EachSection},
EachSection[eachsection_]:=
Block[{ID,Comments,Settings,Tracks},
(*ID[id_]:=id;*)

Comments[comments_]:=comments<>"\n";

Settings[settings_List]:=
Block[{EachSetting},
EachSetting[setting_]:=
Block[{},
ToExpression["Type"/.setting][setting]];
If[settings==={},"\n",{EachSetting/@settings,"\n\n"}]];

Tracks[tracks_List]:=
Block[{EachTrack},
EachTrack[track_]:=
Block[{Id,Instruments,Content},
Id=If[#=="","<","<"<>#<>":"]&;

Instruments[instruments_]:=("Instrument"/.instruments)<>">";

Content[contents_List]:=
Block[{EachContent},
EachContent[content_]:=
Block[{Note,PedalPress,PedalRelease,Tie,BarLine,Subtrack,Clef,Undefined},

Note[note_]:=
Block[{Arpeggio,Pitches,PitchOperators,DurationOperators,VolumeOperators,Staccato},
Arpeggio=If[#,"$",""]&;

Pitches[pitches_List]:=
Block[{eachpitch},
eachpitch[pitch_]:=
Block[{ScaleDegree,ChordNotations,ChordOperators},
ScaleDegree=#&;
ChordNotations=#&;
ChordOperators=#&;
ToExpression[#1][#1/.pitch]&/@
{
"ScaleDegree",
"PitchOperators",
"ChordNotations",
"ChordOperators"
}];
If[Length[pitches]==1,
eachpitch/@pitches,
"["<>(eachpitch/@pitches)<>"]"
]];

PitchOperators=#&;
DurationOperators=#&;
VolumeOperators=#&;
Staccato[staccato_]:=If[staccato==0,"",StringRepeat["`",staccato]];
ToExpression[#1][#1/.note]&/@
{
"Arpeggio",
"Pitches",
"PitchOperators",
"DurationOperators",
"VolumeOperators",
"Staccato"
}];

PedalPress="&"&;
PedalRelease="*"&;
Tie="^"&;
BarLine="|"&;(**)
Subtrack[subtrack_]:="{"<>EachContent/@("Content"/.subtrack)<>"}";

Undefined="Content"/.#&;
ToExpression["Type"/.content][content]];
EachContent/@contents];

ToExpression[#1][#1/.track]&/@
{
"Id",
"Instruments",
"Content"
}];
Riffle[EachTrack/@tracks,"\n\n"]];

ToExpression[#1][#1/.eachsection]&/@
{
(*"ID",*)
"Comments",
"Settings",
"Tracks"
}];
Riffle[EachSection/@sections,"\n\n"]];

Riffle[ToExpression[#1][#1/.input]&/@
{
"Comments",
(*"Undefined",
"Library",
"Settings",*)
"Sections"
},"\n\n"]]


(* ::Input:: *)
(*input=Import[NotebookDirectory[]<>"test.json"];*)


(* ::Input:: *)
(*StringJoin@Flatten@Detoken@input*)
