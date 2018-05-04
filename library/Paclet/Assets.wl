(* ::Package:: *)

BeginPackage["Thulium`Language`", {"Thulium`System`"}];

RefreshLanguage::usage = "RefreshLanguage";
TagName::usage = "TagName";
InstName::usage = "InstName";
TextDict::usage = "TextDict";
MsgDict::usage = "MsgDict";
LangDict::usage = "LangDict";
TagDict::usage = "TagDict";

Begin["`Private`"];

LangDict = Association @ Import[$LocalPath <> "language/Languages.json"];
TagDict = Association /@ Association @ Import[$LocalPath <> "Tags.json"];

RefreshLanguage := With[
  {langDataPath = $LocalPath <> "language/" <> UserInfo["Language"] <> "/"},
  TagName = Association @ Import[langDataPath <> "GeneralTags.json"];
  InstName = Association @ Import[langDataPath <> "Instruments.json"];
  TextDict = Association @ Import[langDataPath <> "GeneralTexts.json"];
  MsgDict = Association @ Import[langDataPath <> "Messages.json"];
];

End[];

EndPackage[];

DeclarePackage["Thulium`Language`", {
  "RefreshLanguage", "LangDict", "TagDict",
  "TagName", "InstName", "TextDict", "MsgDict"
}];


BeginPackage["Thulium`Assets`", {"Thulium`System`"}];

WindowBackground::usage = "Thulium front end window background";
Caption::usage = "Thulium front end captions.";
Container::usage = "Dialog container framework.";
$ListSize::usage = "ListSize";

Begin["`Private`"];

$ListSize = 16;
WindowBackground = RGBColor[1, 1, 1];

StyleFont = If[$OperatingSystem === "MacOSX", "\:82f9\:65b9", "\:5fae\:8f6f\:96c5\:9ed1"];

StyleDict = <|
  "None" -> {},
  "Text" -> {FontSize -> 20}, 
  "Title" -> {FontSize -> 32, FontFamily -> StyleFont, FontWeight -> Bold},
  "TitleCmt" -> {FontSize -> 28, FontFamily -> StyleFont, FontWeight -> Bold, FontColor -> GrayLevel[0.4]},
  "Subtitle" -> {FontSize -> 24, FontFamily -> StyleFont, FontWeight -> Bold},
  "BigTitle" -> {FontSize -> 40, FontFamily -> StyleFont, FontWeight -> Bold},
  "BigTitleCmt" -> {FontSize -> 40, FontFamily -> StyleFont, FontWeight -> Bold, FontColor -> GrayLevel[0.4]},
  "SongName" -> {FontSize -> 24, FontFamily -> StyleFont}, 
  "SongIndex" -> {FontSize -> 22, FontFamily -> StyleFont, FontColor -> GrayLevel[0.2]},
  "SongComment" -> {FontSize -> 22, FontFamily -> StyleFont, FontColor -> GrayLevel[0.4]}
|>;

RenderTemplate[template_, arguments_] := StringReplace[template, {
  "&" ~~ id: DigitCharacter :> ToString[arguments[[ToExpression @ id]], FormatType -> InputForm],
  "$" ~~ id: DigitCharacter :> "\"" <> arguments[[ToExpression @ id]] <> "\"",
  "#" ~~ id: DigitCharacter :> StringRiffle[
    ToString[#, FormatType -> InputForm]& /@ arguments[[ToExpression @ id]],
  ", "]
}];

Caption[string_String, style_String: "None", arguments_List: {}] := Style[
  RenderTemplate[string, arguments],
StyleDict[style]];

Container[content_, lr_, bt_] := Container[content, {lr, lr}, {bt, bt}];
Container[content_, {l_, r_}, {b_, t_}] := Column[{
  Spacer[{1, b}],
  Row[{Spacer[l], content, Spacer[r]}],
  Spacer[{1, t}]
}];

End[];

EndPackage[];

DeclarePackage["Thulium`Assets`", {
  "WindowBackground", "Container", "Caption", "$ListSize"
}];
