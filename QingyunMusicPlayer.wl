(* ::Package:: *)

(* ::Text:: *)
(*Qingyun Music Player*)
(*Version 1.0.0*)


$CharacterEncoding="UTF-8";
$Language="ChineseSimplified";
$local=NotebookDirectory[];
<<($local<>"qymPlay.wl")
index=Import[$local<>"Index.xml","CDATA"];
$songCount=Length@index/4;
$songTitle=Take[index,{1,Length@index,4}];
$songPath=Take[index,{4,Length@index,4}];
$songLyricist=Take[index,{3,Length@index,4}];
$songComposer=Take[index,{2,Length@index,4}];
$songDict=Association[#->$songTitle[[#]]&/@Range[$songCount]];
$songInfo=Column[{
	$songTitle[[#]],
	If[$songComposer[[#]]=="N",Nothing,"\:66f2\:ff1a"<>$songComposer[[#]]],
	If[$songLyricist[[#]]=="N",Nothing,"\:8bcd\:ff1a"<>$songLyricist[[#]]]
}]&/@Range@$songCount;
QingyunPlay[song_,default_]:=Module[{filename},
	filename=$local<>"Songs\\"<>song;
	If[FileExistsQ[filename],
		qymPlay[filename,default],
		MessageDialog[{
			TextCell["File not found!"],DefaultButton[]},
		WindowTitle->"Error"];
		Return[];
	]
];
QingyunPlay[]:=CreateDialog[Column[{,
	Style["\:9752\:4e91\:64ad\:653e\:5668",Bold,32],,
	Style["\:9009\:62e9\:66f2\:76ee",Bold,24],
	SetterBar[Dynamic[choice],$songDict,Appearance->"Vertical"->{Automatic,2}],,
	Style["\:9009\:62e9\:9ed8\:8ba4\:4e50\:5668",Bold,20],
	RadioButtonBar[Dynamic[instrument],{"Sine","Piano","Violin"}],,
	Button["\:64ad\:653e",
		MessageDialog[$songInfo[[choice]],WindowTitle->"\:6b63\:5728\:64ad\:653e..."];
		QingyunPlay[$songPath[[choice]],Dynamic[instrument]],
	ImageSize->150],
},Center],WindowTitle->"\:9752\:4e91\:64ad\:653e\:5668"];


(* ::Input:: *)
(*QingyunPlay["Sumizome_Sakura.qym",{"Sine"}]*)


(* ::Input:: *)
(*QingyunPlay[];*)
