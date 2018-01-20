(* ::Package:: *)

(* ::Input:: *)
(*While[$Context!="Global`",End[]];*)


(* Here is the beginning of context QYS` *)
Begin["QYS`"];

subtrack=Nest[(("{"~~#~~"}")|Except["}"])...&,Except["}"]...,8];
pitOp=Alternatives[Characters@"abdMmop#$,'"]...;
durOp=Alternatives[Characters@"-_.`"]...;
pitch="%"|"x"|DigitCharacter~~pitOp;
order=""|rep[int~~""|(".."~~int)];

getOrder[ord_]:=Union@@StringCases[ord,{
	n:int~~".."~~m:int:>Range[ToExpression@n,ToExpression@m],
	n:int:>{ToExpression@n}
}];
getPitch[pitches_]:=StringCases[pitches,
	pitSd:("%"|"x"|DigitCharacter)~~pitOp:pitOp:>
	{
		"ScaleDegree"->Switch[pitSd,"%",-1,"x",10,_,ToExpression@pitSd],
		"SemitonesCount"->StringCount[pitOp,"#"]-StringCount[pitOp,"b"],
		"OctavesCount"->StringCount[pitOp,"'"]-StringCount[pitOp,","],
		"ChordSymbol"->StringDelete[pitOp,"#"|"b"|"'"|","]
	}
];

(* track tokenizer *)
getTrack[score_]:=StringCases[score,{
	"{"~~n:int~~"*"~~sub:subtrack~~"}":>
		{"Type"->"Track","Contents"->getTrack[sub],"Repeat"->-ToExpression@n},
	"{"~~sub:subtrack~~"}":>
		{"Type"->"Track","Contents"->getTrack[sub],"Repeat"->Max[0,
			StringCases[sub,"\\"|"/"~~i:order~~":":>getOrder[i]]
		]},
	bl:"\\"|"/"~~i:order~~":":>
		{"Type"->"BarLine","Newline"->(bl=="\\"),"Skip"->False,"Order"->getOrder[i]},
	bl:"\\"|"|"|"/":>
		{"Type"->"BarLine","Newline"->(bl=="\\"),"Skip"->(bl=="/"),"Order"->{0}},
	
	(* functions *)
	"<"~~func:name~~":"~~arg:Except[">"]..~~">":>
		{"Type"->"FunctionToken","Simplified"->False,"Argument"->{func->getArgument[arg,func]}},
	"<"~~vol:rep[real]~~">":>
		{"Type"->"FunctionToken","Simplified"->True,"Argument"->{"Volume"->ToExpression/@StringSplit[vol,","]}},
	"<"~~speed:int~~">":>
		{"Type"->"FunctionToken","Simplified"->True,"Argument"->{"Speed"->ToExpression@speed}},
	"<"~~bar:int~~"/"~~beat:int~~">":>
		{"Type"->"FunctionToken","Simplified"->True,"Argument"->{"Bar"->ToExpression@bar,"Beat"->ToExpression@beat}},
	"<1="~~k:key~~o:("'"|",")...~~">":>
		{"Type"->"FunctionToken","Simplified"->True,"Argument"->{"Key"->tonalityDict[[k]],"Oct"->StringCount[o,"'"]-StringCount[o,","]}},
	"<"~~cont:rep[name~~""|("("~~real~~")")]~~">":>
		{"Type"->"FunctionToken","Simplified"->True,"Argument"->Module[{ivList=StringSplit[cont,","]},{
			"Instr"->StringDelete["("~~__~~")"]/@ivList,
			If[Or@@StringContainsQ["("]/@ivList,
				"Volume"->(If[StringContainsQ[#,"("],
					StringCases[#,"("~~volume__~~")":>ToExpression@volume][[1]],
				1.0]&)/@ivList,
			Nothing]
		}]},
	
	(* temporary operators *)
	"("~~n:int~~"~)":>
		{"Type"->"Tuplet","NotesCount"->ToExpression[n]},
	"("~~n:expr~~"-)":>
		{"Type"->"Tremolo1","StrokesCount"->ToExpression[n]},
	"("~~n:expr~~"=)":>
		{"Type"->"Tremolo2","StrokesCount"->ToExpression[n]},
	"("~~pitches:pitch..~~"^)":>
		{"Type"->"Appoggiatura","Pitches"->getPitch[pitches]},
	
	(* others *)
	"~":>{"Type"->"Portamento"},
	"^":>{"Type"->"Tie"},
	pitches:pitch|("["~~(pitch|"^")..~~"]")~~pitOp:pitOp~~durOp:durOp:>{
		"Type"->"Note",
		"Pitches"->getPitch[StringDelete[pitches,"^"|"["|"]"]],
		"SemitonesCount"->StringCount[pitOp,"#"]-StringCount[pitOp,"b"],
		"OctavesCount"->StringCount[pitOp,"'"]-StringCount[pitOp,","],
		"Staccato"->StringContainsQ[durOp,"`"],
		"Arpeggio"->StringContainsQ[pitches,"^"],
		"DurationOperators"->StringDelete[durOp,"`"]
	},
	space:Whitespace:>{"Type"->"Whitespace","Content"->space},
	undef__:>{"Type"->"Undefined","Content"->undef}
}];

(* tokenizer *)
Tokenize[filename_]:=Module[
	{
		i,data,score="",
		songComments={},sections={},
		sectionMeta={},comments={},
		tracks={},trackToken
	},
	data=Import[filename,"Lines"];
	Do[
		Which[
			line=="",
				If[sectionMeta=={}&&songComments=={},
					songComments=comments;
					comments={}
				],
			StringLength@line>2&&StringTake[line,2]=="//",
				AppendTo[comments,StringDrop[line,2]],
			True,
				score=score<>line;
				If[StringPart[line,-1]=="\\",Continue[]];
				trackToken=getTrack[score];
				If[ContainsOnly[Association[#][["Type"]]&/@trackToken,{"FunctionToken"}],
					If[sectionMeta!={},                                             (* empty track *)
						AppendTo[sections,Append[sectionMeta,"Tracks"->tracks]];
						tracks={};
					];
					sectionMeta={"Comments"->comments,"GlobalSettings"->trackToken};
					comments={},
					AppendTo[tracks,<|                                              (* real track *)
						"Type"->"Track",
						"Contents"->trackToken,
						"Repeat"->0
					|>]
				];
				score=""
		],
	{line,data}];
	If[sectionMeta!={},AppendTo[sections,Append[sectionMeta,"Tracks"->tracks]]];
	Return[{
		"Comments"->songComments,
		"Sections"->sections
	}];
];
End[];
(* Here is the end of context QYS` *)


(* ::Input:: *)
(*QYS`getTrack["<1=bB,,>\t"]*)


(* ::Input:: *)
(*Export["E:\\test-Tokenizer.json",QYS`Tokenize[path<>"Songs\\test.qys"]];*)


(* ::Input:: *)
(*ExportString[QYS`Tokenize[path<>"Songs\\test.qys"],"JSON"]*)


(* ::Input:: *)
(*QYS`Tokenize[path<>"Songs\\test.qys"]*)
