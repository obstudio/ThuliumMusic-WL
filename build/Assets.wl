(* ::Package:: *)

ClearAll["Thulium`Assets`*"];
ClearAll["Thulium`Assets`*`*"];


BeginPackage["Thulium`Assets`", {
  "Thulium`System`",
  "Thulium`Graphics`"
}];

WindowBackground::usage = "Thulium front end window background";
ChsFont::usage = "Thulium Music chinese Font";
EngFont::usage = "Thulium Music english Font";
StyleDict::usage = "Thulium Style Dict";
Caption::usage = "Thulium front end captions.";
Container::usage = "Dialog container framework.";
TextLength::usage = "TextLength";
TimeDisplay::usage = "TimeDisplay";
ListSize::usage = "ListSize";

LogoCloud::usage = "LogoCloud";
LogoNote::usage = "LogoNote";

RefreshLanguage::usage = "RefreshLanguage";
TagName::usage = "TagName";
InstName::usage = "InstName";
TextDict::usage = "TextDict";
MsgDict::usage = "MsgDict";
LangDict::usage = "LangDict";
TagDict::usage = "TagDict";

(* tags *)
textInfoTags = {"SongName", "Lyricist", "Composer", "Adapter", "Comment", "Abstract", "Origin"};
otherInfoTags = {"Image", "Uploader", "Tags"};
imageTags = {"Title", "Painter", "PainterID", "IllustID", "Source", "URL"};
aboutTags = {"Version", "Producer", "Website"};
langList = {"chs", "eng"};

TooltipDisplay::usage = "TooltipDisplay";

Begin["`Private`"];

LogoCloud = SVGPath["M836.15,454.53c90.25,18.32,158.21,98.11,158.21,193.791c0,109.22-88.54,197.729-197.75,197.729h-568.5
c-122.87,0-222.46-99.6-222.46-222.46c0-96.97,62.07-179.44,148.62-209.9c-0.22-4.15-0.32-8.34-0.32-12.57
c0-136.5,110.67-247.17,247.18-247.17c91.59,0,171.54,49.82,214.24,123.84c23.55-15.77,51.88-24.97,82.37-24.97
c81.91,0,148.29,66.41,148.29,148.3c0,18.84-3.511,36.84-9.9,53.41"][[1]];
LogoNote = SVGPath["M569.07,330.54c0,0-3.4-3.95-12.53-10.1
c-28.78-19.4-64.45-29.11-98.33-31.92c-10.33-0.86-23.36-1.46-33.73-1.62c-25.17-0.38-53.61,3.05-76.72,14.4L492.81,615.11
c-22.67-12.301-52.2-18.33-83.75-15.301c-65.69,6.311-115.29,49.521-110.78,96.53c4.51,47,61.43,79.99,127.13,73.69
c65.69-6.311,115.29-49.53,110.78-96.53c-0.53-5.48-1.771-10.77-3.65-15.83h-0.01L424.34,315.88c6.1-2.72,35.51-4.39,52.38-4.1
c22.98,0.39,59.32,7.82,69.48,10.78C555.89,325.38,569.07,330.54,569.07,330.54z"][[1]];

ListSize = 16;
WindowBackground = RGBColor[1, 1, 1];

LangDict = Association @ Import[$LocalPath <> "language/Languages.json"];
TagDict = Association /@ Association @ Import[$LocalPath <> "Tags.json"];

RefreshLanguage := With[
  {langDataPath = $LocalPath <> "language/" <> UserInfo["Language"] <> "/"},
  TagName = Association @ Import[langDataPath <> "GeneralTags.json"];
  InstName = Association @ Import[langDataPath <> "Instruments.json"];
  TextDict = Association @ Import[langDataPath <> "GeneralTexts.json"];
  MsgDict = Association @ Import[langDataPath <> "Messages.json"];
];

ChsFont := Once @ If[$OperatingSystem === "MacOSX", "\:82f9\:65b9", "\:5fae\:8f6f\:96c5\:9ed1"];
EngFont := Once @ If[$OperatingSystem === "MacOSX", "Calibri", "Calibri"];

StyleDict := Once @ With[{font = If[$OperatingSystem === "MacOSX", "\:82f9\:65b9", "\:5fae\:8f6f\:96c5\:9ed1"]}, <|
  "None" -> {},
  "Text" -> {FontSize -> 20}, 
  "Title" -> {FontSize -> 32, FontFamily -> font, FontWeight -> Bold},
  "TitleCmt" -> {FontSize -> 28, FontFamily -> font, FontWeight -> Bold, FontColor -> GrayLevel[0.4]},
  "Subtitle" -> {FontSize -> 24, FontFamily -> font, FontWeight -> Bold},
  "BigTitle" -> {FontSize -> 40, FontFamily -> font, FontWeight -> Bold},
  "BigTitleCmt" -> {FontSize -> 40, FontFamily -> font, FontWeight -> Bold, FontColor -> GrayLevel[0.4]},
  "FormHint" -> {FontSize -> 20, FontFamily -> font, FontSlant -> Plain, FontColor -> GrayLevel[0.4]}, 
  "SongName" -> {FontSize -> 24, FontFamily -> font}, 
  "SongIndex" -> {FontSize -> 22, FontFamily -> font, FontColor -> GrayLevel[0.2]},
  "SongComment" -> {FontSize -> 22, FontFamily -> font, FontColor -> GrayLevel[0.4]}
|>];

RenderTemplate[template_, arguments_] := StringReplace[template, {
  "&" ~~ id: DigitCharacter :> ToString[arguments[[ToExpression @ id]], FormatType -> InputForm],
  "$" ~~ id: DigitCharacter :> "\"" <> arguments[[ToExpression @ id]] <> "\"",
  "#" ~~ id: DigitCharacter :> StringRiffle[
    ToString[#, FormatType -> InputForm]& /@ arguments[[ToExpression @ id]],
  ", "]
}];

Caption[string_String, style_String: "None", arguments_List: {}] := Style[
  RenderTemplate[string, arguments],
  StyleDict[style]
];

Container[content_, lr_, bt_] := Container[content, {lr, lr}, {bt, bt}];
Container[content_, {l_, r_}, {b_, t_}] := Column[{
  Spacer[{1, b}],
  Row[{Spacer[l], content, Spacer[r]}],
  Spacer[{1, t}]
}];

(* Using CJK Unified Ideographs from Unicode 5.0 *)
(* Full-width characters range: \u2E80 - \uFE4F *)
SetAttributes[TextLength, Listable];
TextLength[str_String] := With[
  {charCode = ToCharacterCode[str, "Unicode"]},
  Length[charCode] + Length @ Select[charCode, 11904 <= # <= 65103&]
];

TimeDisplay[seconds_, levelspec_: 2] := StringRiffle[{
  IntegerString[Floor[seconds / (60 ^ (levelspec - 1))], 10, 2],
  Sequence @@ Table[
    IntegerString[Floor[Mod[seconds / (60 ^ (level - 1)), 60]], 10, 2],
  {level, levelspec - 1, 1, -1}]
}, ":"];

TooltipDisplay[content_, tooltip_] := Tooltip[
  content,
  Framed[
    Pane[
      tooltip,
      ImageSize -> All,
      ImageMargins -> {{4, 4}, {4, 4}}
    ],
    Background -> RGBColor[1, 1, 0.9, 0.8],
    FrameStyle -> {1, RGBColor[0.8, 0.8, 0.7, 0.2]},
    RoundingRadius -> {8, 8},
    ContentPadding -> True
  ],
  TooltipDelay -> 0.1,
  TooltipStyle -> {
    CellFrame -> {{0, 0}, {0, 0}},
    Background -> RGBColor[0, 0, 0, 0]
  }
];

End[];

EndPackage[];

DeclarePackage["Thulium`Assets`", {
  "RefreshLanguage", "LangDict", "TagDict",
  "TagName", "InstName", "TextDict", "MsgDict",
  "WindowBackground", "Container", "Caption", "ListSize",
  "TextLength", "TimeDisplay",
  "textInfoTags", "otherInfoTags", "imageTags", "aboutTags", "langList"
}];


DumpSave[$LocalPath <> "library/Package/Assets.mx", "Thulium`Assets`"];
