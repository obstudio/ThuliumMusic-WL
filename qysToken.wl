(* ::Package:: *)

(* ::Input:: *)
(*While[$Context!="Global`",End[]];*)


(* Here is the beginning of context QYS` *)
Begin["QYS`"];

subtrack=Nest[(("{"~~#~~"}")|Except["}"])...&,Except["}"]...,8];
pitOp=Alternatives[Characters@"abdMmop#$,'"]...;
durOp=Alternatives[Characters@"-_.`"]...;
pitch="%"|"x"|DigitCharacter~~pitOp;
index=rep[int~~""|(".."~~int)];

getIndex[index_]:=Union@@StringCases[index,{
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
		{"Type"->"subtrack","Contents"->getTrack[sub],"Repeat"->-n},
	"{"~~sub:subtrack~~"}":>
		{"Type"->"subtrack","Contents"->getTrack[sub],"Repeat"->Max[0,
			StringCases[sub,"\\"|"/"~~i:index~~":":>getIndex[i]]
		]},
	bl:"\\"|"/"~~i:index~~":":>
		{"Type"->"Volta","Newline"->(bl=="\\"),"Index"->getIndex[i]},
	bl:"\\"|"|"|"/":>
		{"Type"->"BarLine","Newline"->(bl=="\\"),"Coda"->(bl=="/")},
	
	(* functions *)
	"<"~~func:name~~":"~~arg:Except[">"]..~~">":>
		{"Type"->"FunctionToken","Simplified"->False,"Argument"->{func->getArgument[arg,func]}},
	"<"~~vol:real~~">":>
		{"Type"->"FunctionToken","Simplified"->True,"Argument"->{"Volume"->{ToExpression@vol}}},
	"<"~~speed:int~~">":>
		{"Type"->"FunctionToken","Simplified"->True,"Argument"->{"Speed"->ToExpression@speed}},
	"<"~~bar:int~~"/"~~beat:int~~">":>
		{"Type"->"FunctionToken","Simplified"->True,"Argument"->{"Bar"->ToExpression@bar,"Beat"->ToExpression@beat}},
	"<1="~~cont:(LetterCharacter|","|"'"|"#")..~~">":>
		{"Type"->"FunctionToken","Simplified"->True,"Argument"->{
			"Key"->tonalityDict[[StringDelete[cont,","|"'"]]],
			"Oct"->StringCount[cont,"'"]-StringCount[cont,","]
		}},
	"<"~~cont:rep[name~~""|("("~~real~~")")]~~">":>
		{"Type"->"FunctionToken","Simplified"->True,"Argument"->Module[{ivList=StringSplit[cont,","]},{
			"Instr"->StringDelete["("~~__~~")"]/@ivList,
			If[Or@@StringContainsQ["("]/@ivList,
				"Volume"->(If[StringContainsQ[#,"("],
					StringCases[#,"("~~vol__~~")":>ToExpression@vol][[1]],
				1.0]&)/@ivList,
			Nothing]
		}]},
	
	(* temporary operators *)
	"("~~n:int~~"~)":>
		{"Type"->"Tuplet","NotesCount"->ToExpression[n]},
	"("~~n:int~~"-)":>
		{"Type"->"Tremolo1","StrokesCount"->ToExpression[n]},
	"("~~n:int~~"=)":>
		{"Type"->"Tremolo2","StrokesCount"->ToExpression[n]},
	"("~~pitches:pitch..~~"^)":>
		{"Type"->"Appoggiatura","Pitches"->getPitch[pitches]},
	
	(* note related *)
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
			StringLength@line>2&&StringTake[line,2]=="//",
				AppendTo[comments,StringDrop[line,2]],
			True,
				score=score<>line;
				If[StringPart[line,-1]=="\\",Continue[]];
				trackToken=getTrack[StringDelete[score,Whitespace]];
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
(*QYS`getTrack["{2*1{/2..5,8:}}"]*)


(* ::Input:: *)
(*Export["E:\\test-Tokenizer.json",QYS`Tokenize[path<>"Songs\\test.qys"]];*)


(* ::Input:: *)
(*ExportString[QYS`Tokenize[NotebookDirectory[]<>"Songs\\test.qys"],"JSON"]*)
