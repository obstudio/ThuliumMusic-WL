(* ::Package:: *)

tonalityDict=<|
	"C"->0,"G"->7,"D"->2,"A"->-3,"E"->4,
	"B"->-1,"#F"->6,"#C"->1,"F"->5,"bB"->-2,
	"bE"->3,"bA"->-4,"bD"->1,"bG"->6,"bC"->-1,
	"F#"->6,"C#"->1,"Bb"->-2,"Gb"->6,
	"Eb"->3,"Ab"->-4,"Db"->1,"Cb"->-1
|>;
pitchDict=<|"1"->0,"2"->2,"3"->4,"4"->5,"5"->7,"6"->9,"7"->11|>;


toBase32[n_]:=StringDelete[ToString@BaseForm[n,32],"\n"~~__];


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
	writeInfo[song,Association@info];
];


refresh:=(
	SetDirectory[path<>"Meta\\"];
	songList=StringDrop[FileNames[],-5];
	index=AssociationMap[readInfo,songList];
	songList16=Partition[songList,UpTo@16];
);
