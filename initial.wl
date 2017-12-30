(* ::Package:: *)

version=142;
userPath="C:\\Users\\"<>$UserName<>"\\AppData\\Local\\QYMP\\";
If[!DirectoryQ[userPath],CreateDirectory[userPath]];
If[!DirectoryQ[userPath<>"export\\"],CreateDirectory[userPath<>"export\\"]];
If[!DirectoryQ[userPath<>"buffer\\"],CreateDirectory[userPath<>"buffer\\"]];
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


SetDirectory[path];
instrData=Association@Import[path<>"instr.json"];
langList={"chs"->"\:7b80\:4f53\:4e2d\:6587"(*,"eng"->"\:82f1\:8bed"*)};
langData=Association@Import[path<>"Lang\\"<>userInfo[["Language"]]<>".json"];
tagName=Association@langData[["TagName"]];
instrName=Association@langData[["Instrument"]];
errorDict=Association@langData[["Error"]];
display=Association@langData[["Dialog"]];
textInfoTags={"SongName","Lyricist","Composer","Adapter","Comment","Abstract"};
metaInfoTags={"Format","TrackCount","Duration","Instruments"};
imageData=Association/@Association@Import[path<>"image.json"];
imageTags={"Title","Painter","PainterID","IllustID","URL"};


refresh:=(
	metaTree=StringDrop[FileNames["*","Meta",Infinity],5];
	songList=StringDrop[Select[metaTree,StringMatchQ[__~~".meta"]],-5];
	dirList=Select[metaTree,!StringMatchQ[#,__~~".meta"]&];
	Do[
		If[!DirectoryQ[userPath<>"buffer\\"<>dir],CreateDirectory[userPath<>"buffer\\"<>dir]],
	{dir,dirList}];
	index=AssociationMap[readInfo,songList];
	pageCount=Ceiling[Length@songList/16];
	songListPaged=Partition[songList,UpTo@Ceiling[Length@songList/pageCount]];
);
refresh;


tonalityDict=<|
	"C"->0,"G"->7,"D"->2,"A"->-3,"E"->4,
	"B"->-1,"#F"->6,"#C"->1,"F"->5,"bB"->-2,
	"bE"->3,"bA"->-4,"bD"->1,"bG"->6,"bC"->-1,
	"F#"->6,"C#"->1,"Bb"->-2,"Gb"->6,
	"Eb"->3,"Ab"->-4,"Db"->1,"Cb"->-1
|>;
pitchDict=<|"1"->0,"2"->2,"3"->4,"4"->5,"5"->7,"6"->9,"7"->11|>;
defaultParameter=<|
	"Volume"->1,"Speed"->90,"Key"->0,"Beat"->4,"Bar"->4,"Instr"->"Piano",
	"Dur"->0,"FadeIn"->0,"FadeOut"->0,"Stac"->1/2,"Appo"->1/4,"Oct"->0,
	"Port"->6,"Spac"->0
|>;
funcList=Keys@defaultParameter;


toBase32[n_]:=StringDelete[ToString@BaseForm[n,32],"\n"~~__];
timeDisplay[t_]:=Module[
	{sec=Floor[QuantityMagnitude[UnitConvert[t,"Seconds"]]]},
	IntegerString[Floor[sec/60],10,2]<>":"<>IntegerString[Mod[sec,60],10,2]
];
generateMessage[tag_,arg_]:=Module[{argRule},
	argRule=Flatten@Array[{
		"&"<>ToString[#]->ToString[arg[[#]],FormatType->InputForm],
		"$"<>ToString[#]->arg[[#]],
		"#"<>ToString[#]->StringRiffle[ToString[#,FormatType->InputForm]&/@arg[[#]],", "]
	}&,Length@arg];
	Return@StringReplace[errorDict[[tag]],argRule];
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
	{info=Normal@index[[song,metaInfoTags]]},
	Do[
		AppendTo[info,If[textInfo[[tag]]!="",tag->textInfo[[tag]],Nothing]],
	{tag,textInfoTags}];
	index[[song]]=Association@info;
	writeInfo[song,index[[song]]];
];


update:=Module[{updates={},song,filename,hash,audio,messages},
	refresh;
	Do[
		filename=path<>"Songs\\"<>song<>"."<>index[[song,"Format"]];
		hash=toBase32@FileHash[filename];
		If[KeyExistsQ[bufferHash,song],
			If[bufferHash[[song]]==hash && FileExistsQ[userPath<>"Buffer\\"<>song<>".buffer"],
				Continue[],
				AppendTo[updates,song];
			],
			AppendTo[bufferHash,song->hash];
			AppendTo[updates,song];
		],
	{song,songList}];
	If[updates=={},Return[]];
	Monitor[Do[
		song=updates[[i]];
		filename=path<>"Songs\\"<>song<>"."<>index[[song,"Format"]];
		bufferHash[[song]]=toBase32@FileHash[filename];
		filename=path<>"Songs\\"<>song<>"."<>index[[song,"Format"]];
		hash=toBase32@FileHash[filename];
		If[KeyExistsQ[bufferHash,song],
			If[bufferHash[[song]]==hash && FileExistsQ[userPath<>"Buffer\\"<>song<>".buffer"],
				Continue[],
				bufferHash[[song]]=hash;
			],
			AppendTo[bufferHash,song->hash];
		];
		If[index[[song,"Format"]]=="qys",
			audio=QYSParse[filename],
			audio=QYMParse[filename]
		];
		messages=Values[Options[audio,MetaInformation]][[1]][["Messages"]];
		If[KeyExistsQ[errorLog,song],
			errorLog[[song]]=messages,
			AppendTo[errorLog,song->messages]
		];
		Export[userPath<>"Buffer\\"<>song<>".buffer",audio,"MP3"],
	{i,Length@updates}],
	Panel[Column[{Spacer[{4,4}],
		"\:6b63\:5728\:66f4\:65b0\:672c\:5730\:6b4c\:66f2\:5e93\[Ellipsis]\[Ellipsis]",
		Spacer[1],
		ProgressIndicator[i,{1,Length@updates},ImageSize->{320,16}],
		Spacer[1],
		"(\:7b2c"<>ToString@i<>"\:9996, \:5171"<>ToString@Length@updates<>"\:9996) \:6b63\:5728\:8f7d\:5165: "<>index[[updates[[i]],"SongName"]],
	Spacer[{4,4}]},Alignment->Center],ImageSize->400,Alignment->Center]];
	Export[userPath<>"Buffer.json",Normal@bufferHash[[Intersection[Keys@bufferHash,songList]]]];
	Export[userPath<>"ErrorLog.json",Normal@errorLog];
];


page=1;
