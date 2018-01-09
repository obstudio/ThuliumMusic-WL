(* ::Package:: *)

version=142;
userPath="C:\\Users\\"<>$UserName<>"\\AppData\\Local\\QYMP\\";
cloudPath="http://www.qymp.tk/assets/";
If[!DirectoryQ[userPath],CreateDirectory[userPath]];
If[!DirectoryQ[userPath<>"export\\"],CreateDirectory[userPath<>"export\\"]];
If[!DirectoryQ[userPath<>"buffer\\"],CreateDirectory[userPath<>"buffer\\"]];
If[!DirectoryQ[userPath<>"images\\"],CreateDirectory[userPath<>"images\\"]];
template={"Version"->version,"Language"->"chs","Developer"->False};
If[!FileExistsQ[userPath<>"Default.json"],Export[userPath<>"Default.json",template]];
userInfo=Association@Import[userPath<>"Default.json"];
If[userInfo[["Version"]]<version,
	Do[
		If[!KeyExistsQ[userInfo,tag],AppendTo[userInfo,tag->template[[tag]]]],
	{tag,Keys@template}];
	userInfo[["Version"]]=version;
	Export[userPath<>"Default.json",userInfo];
];
If[!FileExistsQ[userPath<>"Instrument.json"],Export[userPath<>"Instrument.json",{"Piano","Violin","Guitar","Flute"}]];
If[!FileExistsQ[userPath<>"Buffer.json"],Export[userPath<>"Buffer.json",{}]];
bufferHash=Association@Import[userPath<>"Buffer.json"];
If[!FileExistsQ[userPath<>"ErrorLog.json"],Export[userPath<>"ErrorLog.json",{}]];
errorLog=Association@Import[userPath<>"ErrorLog.json"];
If[!FileExistsQ[userPath<>"Favorite.json"],Export[userPath<>"Favorite.json",{}]];
favorite=Import[userPath<>"Favorite.json"];
If[!FileExistsQ[userPath<>"Image.json"],Export[userPath<>"Image.json",{}]];
imageData=Association/@Association@Import[userPath<>"image.json"];


SetDirectory[path];
instrData=Association@Import[path<>"instr.json"];                            (* instruments *)
styleData=ToExpression/@#&/@#&/@Association@Import[path<>"style.json"];     (* styles *)
colorData=Association@Import[path<>"color.json"];                            (* colors *)
styleColor=RGBColor/@Association@colorData[["StyleColor"]];
buttonColor=RGBColor/@#&/@Association/@Association@colorData[["ButtonColor"]];
pageSelectorColor=RGBColor/@#&/@Association/@Association@colorData[["PageSelectorColor"]];
langList={"chs"->"\:7b80\:4f53\:4e2d\:6587","eng"->"English"};                               (* languages *)
langData=Association@Import[path<>"Lang\\"<>userInfo[["Language"]]<>".json"];
tagName=Association@langData[["TagName"]];
instrName=Association@langData[["Instrument"]];
errorDict=Association@langData[["Error"]];
text=Association@langData[["Caption"]];


textInfoTags={"SongName","Lyricist","Composer","Adapter","Comment","Abstract"};
metaInfoTags={"Format","TrackCount","Duration","Instruments"};
imageTags={"Title","Painter","PainterID","IllustID","URL"};


caption[string_,style_]:=caption[string,style,{}];
caption[string_,style_,argument_]:=Style[
	completeText[
		If[StringPart[string,1]=="_",text[[StringDrop[string,1]]],string],
	argument],
styleData[[style]]];


matchDict=<|"["->"]","("->")","{"->"}","<"->">"|>;
tonalityDict=<|
	"C"->0,"G"->7,"D"->2,"A"->-3,"E"->4,
	"B"->-1,"#F"->6,"#C"->1,"F"->5,"bB"->-2,
	"bE"->3,"bA"->-4,"bD"->1,"bG"->6,"bC"->-1,
	"F#"->6,"C#"->1,"Bb"->-2,"Gb"->6,
	"Eb"->3,"Ab"->-4,"Db"->1,"Cb"->-1
|>;
pitchDict=<|"1"->0,"2"->2,"3"->4,"4"->5,"5"->7,"6"->9,"7"->11|>;
pitchOpDict=<|
	"#"->1,"b"->-1,"'"->12,","->-12,"M"->{0,4,7},"m"->{0,3,7},
	"a"->{0,4,8},"d"->{0,3,6},"p"->{0,7,12},"o"->{0,12}
|>;
pitchOpList=Append[Keys[pitchOpDict],"$"];
defaultParameter=<|
	"Volume"->1,"Speed"->90,"Key"->0,"Beat"->4,"Bar"->4,"Instr"->"Piano",
	"Dur"->0,"FadeIn"->0,"FadeOut"->0,"Stac"->1/2,"Appo"->1/4,"Oct"->0,
	"Port"->6,"Spac"->0,"Chord"->{0,12}
|>;
funcList=Keys@defaultParameter;


findMatch[score_,pos_]:=Module[
	{i=pos+1,left,right,stack=1},
	left=StringPart[score,pos];
	right=matchDict[[left]];
	While[i<=StringLength[score],
		Switch[StringPart[score,i],
			left,stack++,
			right,stack--;If[stack==0,Break[]];
		];
		i++
	];
	Return[i];
];
toArgument[str_]:=If[StringContainsQ[str,","],ToExpression/@StringSplit[str,","],ToExpression@str];
toBase32[n_]:=StringDelete[ToString@BaseForm[n,32],"\n"~~__];
timeDisplay[t_]:=Module[
	{sec=Floor[QuantityMagnitude[UnitConvert[t,"Seconds"]]]},
	IntegerString[Floor[sec/60],10,2]<>":"<>IntegerString[Mod[sec,60],10,2]
];
completeText[raw_,arg_]:=StringReplace[raw,Flatten@Array[{
	"&"<>ToString[#]->ToString[arg[[#]],FormatType->InputForm],
	"$"<>ToString[#]->arg[[#]],
	"#"<>ToString[#]->StringRiffle[ToString[#,FormatType->InputForm]&/@arg[[#]],", "]
}&,Length@arg]];
generateMessage[tag_,arg_]:=completeText[errorDict[[tag]],arg];


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
	{info=Normal@index[[song,metaInfoTags]]},
	Do[
		AppendTo[info,If[textInfo[[tag]]!="",tag->textInfo[[tag]],Nothing]],
	{tag,textInfoTags}];
	index[[song]]=Association@info;
	writeInfo[song,index[[song]]];
];
