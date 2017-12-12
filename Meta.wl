(* ::Package:: *)

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
	AssociationMap[If[KeyExistsQ[Index[[song]],#],Index[[song,#]],""]&,TextInfoTags]
);
putTextInfo[song_,textInfo_]:=Module[
	{info=Normal@Index[[song,MetaInfoTags]]},
	Do[
		AppendTo[info,If[textInfo[[tag]]!="",tag->textInfo[[tag]],Nothing]],
	{tag,TextInfoTags}];
	writeInfo[song,Association@info];
];
refresh:=(
	SetDirectory[path<>"Meta\\"];
	SongList=StringDrop[FileNames[],-5];
	Index=AssociationMap[readInfo,SongList];
);
