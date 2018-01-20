(* ::Package:: *)

(* user data *)
version=201;
userPath=$HomeDirectory<>"\\AppData\\Local\\QYMP\\";
cloudPath="http://qymp.ob-studio.cn/assets/";
If[!DirectoryQ[userPath],CreateDirectory[userPath]];
If[!DirectoryQ[userPath<>"export\\"],CreateDirectory[userPath<>"export\\"]];
If[!DirectoryQ[userPath<>"buffer\\"],CreateDirectory[userPath<>"buffer\\"]];
If[!DirectoryQ[userPath<>"images\\"],CreateDirectory[userPath<>"images\\"]];
template=<|"Version"->version,"Language"->"chs","Developer"->False,"Player"->"Old"|>;
If[!FileExistsQ[userPath<>"Default.json"],Export[userPath<>"Default.json",template]];
userInfo=Association@Import[userPath<>"Default.json"];
If[userInfo[["Version"]]<version,
	Do[
		If[!KeyExistsQ[userInfo,tag],AppendTo[userInfo,tag->template[[tag]]]],
	{tag,Keys@template}];
	userInfo[["Version"]]=version;
	Export[userPath<>"Default.json",userInfo];
];
If[!FileExistsQ[userPath<>"Buffer.json"],Export[userPath<>"Buffer.json",{}]];
bufferHash=Association@Import[userPath<>"Buffer.json"];
If[!FileExistsQ[userPath<>"ErrorLog.json"],Export[userPath<>"ErrorLog.json",{}]];
errorLog=Association@Import[userPath<>"ErrorLog.json"];
If[!FileExistsQ[userPath<>"Favorite.json"],Export[userPath<>"Favorite.json",{}]];
favorite=Import[userPath<>"Favorite.json"];
If[!FileExistsQ[userPath<>"Image.json"],Export[userPath<>"Image.json",{}]];
imageData=Association/@Association@Import[userPath<>"image.json"];


(* local data *)
path=NotebookDirectory[];
instrData=Association@Import[path<>"instr.json"];                               (* instruments *)
colorData=Association@Import[path<>"color.json"];                               (* colors *)
styleColor=RGBColor/@Association@colorData[["StyleColor"]];
buttonColor=RGBColor/@#&/@Association/@Association@colorData[["ButtonColor"]];
pageSelectorColor=RGBColor/@#&/@Association/@Association@colorData[["PageSelectorColor"]];
styleData=Association/@Association@Import[path<>"style.json"];                  (* styles *)
styleDict=Normal@Module[{outcome={}},
	If[KeyExistsQ[#,"FontSize"],AppendTo[outcome,FontSize->#[["FontSize"]]]];
	If[KeyExistsQ[#,"FontFamily"],AppendTo[outcome,FontFamily->#[["FontFamily"]]]];
	If[KeyExistsQ[#,"FontWeight"],AppendTo[outcome,FontWeight->ToExpression@#[["FontWeight"]]]];
	If[KeyExistsQ[#,"FontColor"],AppendTo[outcome,FontColor->styleColor[[#[["FontColor"]]]]]];
outcome]&/@styleData;
langList={"chs"(*,"eng"*)};                                                     (* languages *)
langDict=#->caption[Association[Import[path<>"Lang\\"<>#<>".json"]][["LanguageName"]],"Text"]&/@langList;
langData=Association@Import[path<>"Lang\\"<>userInfo[["Language"]]<>".json"];
tagName=Association@langData[["TagName"]];
instrName=Association@langData[["Instrument"]];
text=Association@langData[["Caption"]];
aboutInfo=Association@text[["AboutQYMP"]];
metaInfoTags={"Format","TrackCount","Duration","Instruments"};                  (* tags *)
textInfoTags={"SongName","Lyricist","Composer","Adapter","Comment","Abstract"};
otherInfoTags={"Image","Uploader"};
imageTags={"Title","Painter","PainterID","IllustID","URL"};
aboutTags={"Version","Producer","Website"};


(* some functions *)
textLength[string_]:=2StringLength[string]-StringCount[string,Alternatives@CharacterRange[32,127]];
toBase32[n_]:=StringDelete[ToString@BaseForm[n,32],"\n"~~__];
timeDisplay[t_]:=Module[
	{sec=Floor[QuantityMagnitude[UnitConvert[t,"Seconds"]]]},
	IntegerString[Floor[sec/60],10,2]<>":"<>IntegerString[Mod[sec,60],10,2]
];
completeText[raw_,arg_]:=StringReplace[raw,{
	"&"~~i:int:>ToString[arg[[ToExpression@i]],FormatType->InputForm],
	"$"~~i:int:>"\""<>arg[[ToExpression@i]]<>"\"",
	"#"~~i:int:>StringRiffle[ToString[#,FormatType->InputForm]&/@arg[[ToExpression@i]],", "]
}];
caption[string_String]:=caption[string,"None",{}];
caption[string_String,argument_List]:=caption[string,"None",argument];
caption[string_String,style_String]:=caption[string,style,{}];
caption[string_String,style_String,argument_List]:=Style[completeText[
	If[StringLength@string>0&&StringPart[string,1]=="_",text[[StringDrop[string,1]]],string],
argument],styleDict[[style]]];


(* tokenizer related *)
rep=#~~(","~~#)...&;
int=DigitCharacter..;
expr=Except["("|")"|"<"|">"]..;
name=LetterCharacter~~WordCharacter...;
real=DigitCharacter...~~"."~~DigitCharacter...;
tonalityDict=<|
	"C"->0,"G"->7,"D"->2,"A"->9,"E"->4,
	"B"->-1,"#F"->6,"#C"->1,"F"->5,"bB"->-2,
	"bE"->3,"bA"->8,"bD"->1,"bG"->6,"bC"->-1,
	"F#"->6,"C#"->1,"Bb"->-2,"Gb"->6,
	"Eb"->3,"Ab"->8,"Db"->1,"Cb"->-1
|>;
key=Alternatives@Keys@tonalityDict;
getArgument[string_,function_]:=Switch[function,
	"Instr",{string},
	"Volume"|"Chord",ToExpression/@StringSplit[string,","],
	_,ToExpression[string]
];


writeInfo[song_,info_]:=Export[
	path<>"Meta\\"<>song<>".meta",
	StringRiffle[KeyValueMap[#1<>": "<>#2<>";"&,info],"\n"],
"Text"];
readInfo[song_]:=Module[
	{data,info={},match,i},
	data=StringSplit[Import[path<>"Meta\\"<>song<>".meta","Text"],{";\n",";"}];
	Do[
		match=StringPosition[data[[i]],": "][[1,1]];
		AppendTo[info,StringTake[data[[i]],match-1]->StringDrop[data[[i]],match+1]],
	{i,Length[data]}];
	Return[Association@info];
];
getTextInfo[song_]:=(
	refresh;
	AssociationMap[If[KeyExistsQ[index[[song]],#],index[[song,#]],""]&,textInfoTags]
);
putTextInfo[song_,textInfo_]:=Module[
	{info=index[[song]]},
	Do[
		If[KeyExistsQ[info,tag],
			If[textInfo[[tag]]!="",info[[tag]]=textInfo[[tag]],info=Delete[info,tag]],
			If[textInfo[[tag]]!="",AppendTo[info,tag->textInfo[[tag]]]]
		],
	{tag,textInfoTags}];
	index[[song]]=info;
	writeInfo[song,index[[song]]];
];
