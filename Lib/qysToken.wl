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
expr=Except["("|")"|"<"|">"]..;

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
					AppendTo[tracks,{                                              (* real track *)
						"Type"->"Track",
						"Contents"->trackToken,
						"Repeat"->0
					}]
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
(*Export["E:\\test-Tokenizer.json",QYS`Tokenize[localPath<>"Songs\\test.qys"]];*)


(* ::Input:: *)
(*ExportString[QYS`Tokenize[localPath<>"Songs\\test.qys"],"JSON"]*)


(* ::Input:: *)
(*QYS`Tokenize[localPath<>"Songs\\test.qys"]*)


keyDictR=<|-2->"bB",-1->"B",0->"C",1->"#C",2->"D",3->"bE",4->"E",5->"F",6->"#F",7->"G",8->"#G",9->"A"|>;
schemes=RGBColor/@Association@#&/@Association@Import[localPath<>"Lib\\Color.json"];


detoken[tokenizer_,scheme_]:=Module[
	{
		token=Association@tokenizer,
		argument,
		detok={}
	},
	Switch[token[["Type"]],
		"FunctionToken",
			AppendTo[detok,Style["<",scheme[["Function"]]]];
			argument=Association@token[["Argument"]];
			If[token[["Simplified"]]==True,
				Switch[Keys@argument,
					{"Key","Oct"},
						AppendTo[detok,Style["1=",scheme[["FuncName"]]]];
						AppendTo[detok,Style[Key[argument[["Key"]]][keyDictR],scheme[["FuncArg"]]]];
						AppendTo[detok,Style[Switch[argument[["Oct"]],
							_?Positive,StringRepeat["'",argument[["Oct"]]],
							_?Negative,StringRepeat[",",-argument[["Oct"]]],
							0,""
						],scheme[["FuncArg"]]]],
					{"Bar","Beat"},
						AppendTo[detok,Style[argument[["Bar"]],scheme[["FuncArg"]]]];
						AppendTo[detok,Style["/",scheme[["FuncName"]]]];
						AppendTo[detok,Style[argument[["Beat"]],scheme[["FuncArg"]]]],
					{"Instr","Volume"},
						detok=detok~Join~Flatten@Riffle[Table[{
							Style[argument[["Instr",i]],scheme[["FuncArg"]]],
							If[argument[["Volume",i]]!=1,{
								Style["(",scheme[["FuncName"]]],
								Style[argument[["Volume",i]],scheme[["FuncArg"]]],
								Style[")",scheme[["FuncName"]]]
							},{Nothing}]},{i,Length@argument[["Instr"]]}],
						Style[",",scheme[["Function"]]]],
					{"Volume"},
						detok=detok~Join~Riffle[Style[
							If[StringPart[#,-1]==".",#<>"0",#]&@ToString[#],scheme[["FuncArg"]]
						]&/@argument[["Volume"]],
						Style[",",scheme[["Function"]]]],
					{"Instr"},
						detok=detok~Join~Riffle[
							Style[#,scheme[["FuncArg"]]]&/@argument[["Instr"]],
						Style[",",scheme[["Function"]]]],
					{"Speed"},
						AppendTo[detok,Style[argument[["Speed"]],scheme[["FuncArg"]]]];
				],
				Do[
					AppendTo[detok,Style[function,scheme[["FuncName"]]]];
					AppendTo[detok,Style[":",scheme[["Function"]]]];
					Switch[function,
						"Chord"|"Instr",
							detok=detok~Join~Riffle[
								Style[#,scheme[["FuncArg"]]]&/@argument[[function]],
							Style[",",scheme[["Function"]]]],
						"Volume",
							detok=detok~Join~Riffle[Style[
								If[StringPart[#,-1]==".",#<>"0",#]&@ToString[#],scheme[["FuncArg"]]
							]&/@argument[["Volume"]],
							Style[",",scheme[["Function"]]]],
						_,
							AppendTo[detok,Style[argument[[function]],scheme[["FuncArg"]]]];
					],
				{function,Keys@argument}];
			];
			AppendTo[detok,Style[">",scheme[["Function"]]]];
	];
	Return[Row[detok,Background->scheme[["Background"]]]];
];


(* ::Input:: *)
(*Row[detoken[#,schemes[["Default"]]]&/@QYS`getTrack["<1=bD'><Chord:0,12><1.0>"]]*)
