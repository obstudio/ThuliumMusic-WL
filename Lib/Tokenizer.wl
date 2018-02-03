(* ::Package:: *)

BeginPackage["SMML`Tokenizer`"];

keyDict=<|
	"C"->0,"G"->7,"D"->2,"A"->9,"E"->4,
	"B"->-1,"#F"->6,"#C"->1,"F"->5,"bB"->-2,
	"bE"->3,"bA"->8,"bD"->1,"bG"->6,"bC"->-1,
	"F#"->6,"C#"->1,"Bb"->-2,"Gb"->6,
	"Eb"->3,"Ab"->8,"Db"->1,"Cb"->-1
|>;
key=Alternatives@@Keys@keyDict;
word=WordCharacter..;
jsCodePatt=Nest[(("{"~~#~~"}")|Except["{"|"}"])...&,Except["{"|"}"]...,8];
function=Alternatives@@Flatten[StringCases[StringExpression[
	name:word,
	Shortest["("~~__~~")"],
	Whitespace,
	Shortest["{"~~jsCodePatt~~"}"]
]:>name]/@StringCases[
	"module.exports = {"~~cont:Shortest[jsCodePatt]~~"}":>cont
]@Import[
	NotebookDirectory[]<>"Standard\\Function.js","String"
]];

rep[pat_]:=rep[pat,","];
rep[pat_,sep_]:=pat~~(sep~~pat)...;
unsigned=DigitCharacter..;
integer=""|"-"|"+"~~DigitCharacter..;
number=(integer~~""|("/"|"."~~unsigned))|("log2("~~unsigned~~")");
subtrack=Nest[(("{"~~#~~"}")|Except["{"|"}"])...&,Except["{"|"}"]...,8];
argument=rep[number|("{"~~subtrack~~"}")];
orderList=""|rep[integer~~""|("~"~~integer)];
orderTok[ord_]:=Union@@StringCases[ord,{
	n:integer~~"~"~~m:integer:>Range[ToExpression@n,ToExpression@m],
	n:integer:>{ToExpression@n}
}];

preOperator="$"|"";
postOperator="``"|"`"|"";
volOperator=(">"|":")...;
pitOperator=("#"|"b"|"'"|",")...;
durOperator=("."|"-"|"_"|"=")...;
scaleDegree="0"|"1"|"2"|"3"|"4"|"5"|"6"|"7"|"%"|"x";
chordNotation=""|Alternatives@@{"M","m","a","d","p"};
chordOperator=Alternatives@@{"o","u","i","j"}...;
pitch=scaleDegree~~pitOperator~~chordNotation~~chordOperator;
pitches="["~~pitch..~~"]"~~pitOperator;
note=preOperator~~pitch|pitches~~volOperator~~durOperator~~postOperator;
pitchTok[str_]:=StringCases[str,
	StringExpression[
		sd:scaleDegree,
		po:pitOperator,
		cn:chordNotation,
		co:chordOperator
	]:>{
		"ScaleDegree"->sd,
		"PitchOperators"->po,
		"ChordNotations"->cn,
		"ChordOperators"->co
	}
];

trackTok[str_]:=StringCases[str,{
	
	(* functions *)
	"("~~var:function~~":"~~arg:rep[number]~~")":>{
		"Type"->"FUNCTION",
		"Name"->var,
		"Simplified"->True,
		"Argument"->({"Type"->"Expression","Content"->#}&)/@StringSplit[arg,","]
	},
	"("~~vol:number~~"%)":>{
		"Type"->"FUNCTION",
		"Name"->"Vol",
		"Simplified"->True,
		"Argument"->{{"Type"->"Expression","Content"->vol}}
	},
	"("~~bar:unsigned~~"/"~~beat:unsigned~~")":>{
		"Type"->"FUNCTION",
		"Name"->"BarBeat",
		"Simplified"->True,
		"Argument"->{{"Type"->"Expression","Content"->bar},{"Type"->"Expression","Content"->beat}}
	},
	"("~~spd:NumberString~~")":>{
		"Type"->"FUNCTION",
		"Name"->"Spd",
		"Simplified"->True,
		"Argument"->{{"Type"->"Expression","Content"->spd}}
	},
	"(1="~~key:key~~oct:("'"|",")...~~")":>{
		"Type"->"FUNCTION",
		"Name"->"KeyOct",
		"Simplified"->True,
		"Argument"->{{"Type"->"String","Content"->key},{"Type"->"String","Content"->oct}}
	},
	
	(* subtracks and barlines *)
	"{"~~n:integer~~"*"~~sub:subtrack~~"}":>
		{"Type"->"Track","Contents"->trackTok[sub],"Repeat"->-ToExpression@n},
	"{"~~sub:subtrack~~"}":>
		{"Type"->"Track","Contents"->trackTok[sub],"Repeat"->Max[-1,
			StringCases[sub,"/"~~i:orderList~~":":>orderTok[i]]
		]},
	bl:"/"~~i:orderList~~":":>
		{"Type"->"BarLine","Newline"->(bl=="\\"),"Skip"->False,"Order"->orderTok[i]},
	bl:"|"|"/":>
		{"Type"->"BarLine","Newline"->(bl=="\\"),"Skip"->(bl=="/"),"Order"->{0}},
	
	(* notes *)
	StringExpression[
		pre:preOperator,
		""|"["~~pts:pitch..~~"]"|"",
		pit:pitOperator,
		vol:volOperator,
		dur:durOperator,
		pst:postOperator
	]:>{
		"Type"->"Note",
		"Pitches"->pitchTok[pts],
		"PitchOperators"->pit,
		"DurationOperators"->dur,
		"VolumeOperators"->vol,
		"Staccato"->StringContainsQ[pre,"$"],
		"Arpeggio"->StringCount[pst,"`"]
	},
	
	(* other notations *)
	space:Whitespace:>{"Type"->"Whitespace","Content"->space},
	undef_:>{"Type"->"Undefined","Content"->undef}
}];

include[SMLPath_]:=Module[
	{
		data={},context,command,
		messages={},
		tokenData=<|"ChordNotation"-><||>,"ChordInversion"-><||>|>
	},
	
	If[FileExistsQ[SMLPath],
		data=Import[SMLPath,"List"],
		AppendTo[messages,<|"Type"->"FileNotFound","Arguments"->SMLPath|>]
	];
	Do[
		Switch[line,
			_?(StringStartsQ["!"]),
				command=Cases[StringSplit[StringDrop[line,1]," "..],Except[""]];
				Switch[command[[1]],
					"SMML",
						Switch[command[[2]],
							"Version",,
							_,AppendTo[messages,<|"Type"->"InvalidCommand","Arguments"->line|>]
						],
					"Chord",
						Switch[command[[2]],
							"Notation",context="ChordNotation",
							"Inversion",context="ChordInversion"
						],
					_,
						AppendTo[messages,<|"Type"->"InvalidCommand","Arguments"->line|>]
				],
			_?(StringStartsQ[Except["#"]]),
				AppendTo[tokenData[[context]],Switch[context,
					"ChordNotation",
						#[[1]]->ToExpression/@StringSplit[#[[3]],","~~" "...]&,
					"ChordInversion",
						#[[1]]->ToExpression/@StringSplit[#[[3]],","~~" "...]&
				][StringSplit[line,"\t"..]]]
		],
	{line,data}];
	
	Return[<|
		"Data"->data,
		"Messages"->messages,
		"TokenData"->tokenData
	|>];
];

EndPackage[];


(* ::Input:: *)
(*trackTok["(1=bD')(3/4){[13],.`/3,5:$5#}"]*)


(* ::Input:: *)
(*Contexts["SMML`*"]*)


(* ::Input:: *)
(*Export[NotebookDirectory[]<>"test.json",trackTok["(1=bD')(3/4){[13],.`/3,5:$5#}"]];*)
