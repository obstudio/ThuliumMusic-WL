(* ::Package:: *)

(* ::Input:: *)
(*While[$Context!="Global`",End[]];*)


(* Here is the beginning of context QYS` *)
Begin["QYS`"];
Subtrack=Nest[(("{"~~#~~"}")|Except["}"])...&,Except["}"],8];
InstrVolume=(LetterCharacter~~WordCharacter...~~(""|("("~~NumberString~~")")));
PitOperator=Alternatives[Characters@"abdMmop#$,'"]...;
DurOperator=Alternatives[Characters@"-_.`"]...;
Pitch="%"|"x"|DigitCharacter~~PitOperator;
getInsVolToken[ivList_]:={
	"Instr"->StringDelete["("~~__~~")"]/@ivList,
	If[Or@@StringContainsQ["("]/@ivList,
		"Volume"->If[StringContainsQ[#,"("],
			StringCases[#,"("~~vol__~~")":>ToExpression@vol][[1]],
		1.0]&/@ivList,
	Nothing]
};
getPitchToken[pitch_]:=StringCases[pitch,
	pitSd:("%"|"x"|DigitCharacter)~~pitOp:PitOperator:>
	{
		"ScaleDegree"->Switch[pitSd,"%",-1,"x",10,_,pitSd],
		"SemitonesCount"->StringCount[pitOp,"#"]-StringCount[pitOp,"b"],
		"OctavesCount"->StringCount[pitOp,"'"]-StringCount[pitOp,","],
		"ChordSymbol"->StringDelete[pitOp,"#"|"b"|"'"|","]
	}
];

(* track tokenizer *)
getTrackToken[score_]:=StringCases[score,{
	"{"~~sub:Subtrack~~"}":>{"Type"->"Subtrack","Contents"->getTrackToken[sub]},
	bl:"\\"|"|"|"/":>{"Type"->"BarLine","Newline"->(bl=="\\"),"Volta"->(bl=="/")},
	"~":>{"Type"->"Portamento"},
	"^":>{"Type"->"Tie"},
	
	(* functions *)
	"<"~~func:LetterCharacter..~~":"~~arg:Except[">"]..~~">":>
		{"Type"->"FunctionToken","Simplified"->False,"Argument"->{func->getArgument[arg,func]}},
	"<"~~vol:(DigitCharacter...~~"."~~DigitCharacter...)~~">":>
		{"Type"->"FunctionToken","Simplified"->True,"Argument"->{"Volume"->{vol}}},
	"<"~~speed:DigitCharacter..~~">":>
		{"Type"->"FunctionToken","Simplified"->True,"Argument"->{"Speed"->speed}},
	"<"~~bar:NumberString~~"/"~~beat:NumberString~~">":>
		{"Type"->"FunctionToken","Simplified"->True,"Argument"->{"Bar"->bar,"Beat"->beat}},
	"<1="~~cont:(LetterCharacter|","|"'")..~~">":>
		{"Type"->"FunctionToken","Simplified"->True,"Argument"->{
			"Key"->tonalityDict[[StringDelete[cont,","|"'"]]],
			"Oct"->StringCount[cont,"'"]-StringCount[cont,","]
		}},
	"<"~~cont:(InstrVolume..~~(","~~InstrVolume)...)~~">":>
		{"Type"->"FunctionToken","Simplified"->True,"Argument"->getInsVolToken[StringSplit[cont,","]]},
	
	(* temporary operators *)
	"("~~ns:NumberString~~"~)":>
		{"Type"->"Tuplet","NoteCount"->ToExpression[ns]},
	"("~~ns:NumberString~~"-)":>
		{"Type"->"Tremolo1","StrokeCount"->ToExpression[ns]},
	"("~~ns:NumberString~~"=)":>
		{"Type"->"Tremolo2","StrokeCount"->ToExpression[ns]},
	"("~~pitch:Pitch..~~"^)":>
		{"Type"->"Appoggiatura","Pitches"->getPitchToken[pitch]},
	
	pitch:Pitch|("["~~(Pitch|"^")..~~"]")~~pitOp:PitOperator~~durOp:DurOperator:>{
		"Type"->"Note",
		"Pitches"->getPitchToken[StringDelete[pitch,"^"|"["|"]"]],
		"SemitonesCount"->StringCount[pitOp,"#"]-StringCount[pitOp,"b"],
		"OctavesCount"->StringCount[pitOp,"'"]-StringCount[pitOp,","],
		"Staccato"->StringContainsQ[durOp,"`"],
		"Arpeggio"->StringContainsQ[pitch,"^"],
		"DurationOperators"->StringDelete[durOp,"`"]
	}
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
			StringTake[line,2]=="//",
				AppendTo[comments,StringDrop[line,2]],
			True,
				score=score<>line;
				If[StringPart[line,-1]=="\\",Continue[]];
				trackToken=getTrackToken[StringDelete[score,Whitespace]];
				If[MemberQ[Association[#][["Type"]]&/@trackToken,"Note"],     (* empty track *)
					AppendTo[tracks,trackToken],
					If[sectionMeta!={},
						AppendTo[sections,Append[sectionMeta,"Tracks"->tracks]];
						tracks={};
					];
					sectionMeta={"Comments"->comments,"GlobalSettings"->trackToken};
					comments={};
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
(*QYS`getTrackToken["(6,^)1M-"]//Column*)


(* ::Input:: *)
(*Export["E:\\test-Tokenizer.json",QYS`Tokenize[path<>"Songs\\test.qys"]];*)


(* ::Input:: *)
(*ExportString[QYS`Tokenize[NotebookDirectory[]<>"Songs\\test.qys"],"JSON"]*)
