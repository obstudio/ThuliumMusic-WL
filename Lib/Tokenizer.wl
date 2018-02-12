(* ::Package:: *)

BeginPackage["SMML`Tokenizer`"];
Get[NotebookDirectory[]<>"TrackTok.wl"];

rep[pat_]:=rep[pat,","~~" "...];
rep[pat_,sep_]:=pat~~(sep~~pat)...;
unsigned=DigitCharacter..;
integer=""|"-"|"+"~~unsigned;
chordCodeTok[str_]:=StringCases[str,
	StringExpression[
		ntt:LetterCharacter~~"\t"..,
		cmt:Shortest[__]~~"\t"..,
		pts:rep[integer]
	]:><|
		"Notation"->ntt,
		"Comment"->cmt,
		"Pitches"->StringSplit[pts,","~~" "...]
	|>
][[1]];

intPsb=""|integer;
chordOpCode=rep["["~~intPsb|(intPsb~~";"~~intPsb)~~"]"~~intPsb];
chordOpCodeTok[str_]:=StringCases[line,
	StringExpression[
		ntt:LetterCharacter~~"\t"..,
		cmt:Shortest[__]~~"\t"..,
		pts:chordOpCode
	]:><|
		"Notation"->ntt,
		"Comment"->cmt,
		"Pitches"->StringCases[pts,{
			"["~~pos:intPsb~~"]"~~sft:intPsb:>{
				If[pos=="",1,ToExpression[pos]],
				If[pos=="",-1,ToExpression[pos]],
				If[sft=="",0,ToExpression[sft]]
			},
			"["~~pos1:intPsb~~";"~~pos2:intPsb~~"]"~~sft:intPsb:>{
				If[pos1=="",1,ToExpression[pos1]],
				If[pos2=="",-1,ToExpression[pos2]],
				If[sft=="",0,ToExpression[sft]]
			}
		}]
	|>
][[1]];

rif[list_]:=rif[list,WhitespaceCharacter...];
rif[list_,sep_]:=StringExpression@@Riffle[list,sep,{1,-1,2}];
word=LetterCharacter~~WordCharacter...;
jsCode=Nest[(("{"~~#~~"}")|Except["{"|"}"])...&,Except["{"|"}"]...,8];
functionCode=rif[{word,"("~~Except["{"|"}"]...~~")","{"~~jsCode~~"}"}];
functionCodeTok[str_]:=StringCases[str,
	StringExpression[
		name:word,
		WhitespaceCharacter...,
		"("~~Except["{"|"}"]...~~")",
		WhitespaceCharacter...,
		"{"~~js:jsCode~~"}"
	]:><|
		"Name"->name,
		"Code"->str,
		"Syntax"->StringCases[js,"/**** "~~stx:Shortest[__]~~" ****/":>stx]
	|>
][[1]];

preOperator="$"|"";
postOperator="``"|"`"|"";
volOperator=(">"|":")...;
pitOperator=("#"|"b"|"'"|",")...;
durOperator=("."|"-"|"_"|"=")...;
scaleDegree="0"|"1"|"2"|"3"|"4"|"5"|"6"|"7"|"%"|"x";

number=integer~~""|("."~~unsigned);
expression=(integer~~""|("/"|"."~~unsigned))|("log2("~~unsigned~~")");
string=Except[WhitespaceCharacter|"("|")"|"{"|"}"|"["|"]"|"<"|">"]..;
subtrack=Nest[(("{"~~#~~"}")|Except["{"|"}"])...&,Except["{"|"}"]...,8];
argument=rep[expression|number|string|("{"~~subtrack~~"}")];

(* notation *)
orderList=""|rep[unsigned~~""|("~"~~unsigned)];
orderTok=Union@@StringCases[#,{
	n:integer~~"~"~~m:integer:>Range[ToExpression@n,ToExpression@m],
	n:integer:>{ToExpression@n}
}]&;
notationPatt=("/"~~orderList~~":")|"|"|"/"|"^"|"&"|"*"|Whitespace;
notationTok=StringCases[{
	bl:"/"~~ol:orderList~~":":>
		{"Type"->"BarLine","Newline"->(bl=="\\"),"Skip"->False,"Order"->orderTok[ol]},
	bl:"|"|"/":>
		{"Type"->"BarLine","Newline"->(bl=="\\"),"Skip"->(bl=="/"),"Order"->{0}},
	"^":>{"Type"->"Tie"},
	"&":>{"Type"->"PedalPress"},
	"*":>{"Type"->"PedalRelease"},
	space:Whitespace:>{"Type"->"Whitespace","Content"->space}
}];

contextList={"End","ChordNotation","ChordOperator","Function"};
settingList={"RecursionLimit","StaffDisplay","ForcedSpace"};
syntaxTemplate=<|
	"ChordNotation"->{},
	"ChordOperator"->{},
	"FunctionList"->{},
	"FunctionSimp"-><||>,
	"Typesetting"-><||>
|>;
syntaxList=Keys@syntaxTemplate;
blankLineQ[str_]:=Or[
	StringMatchQ[str,WhitespaceCharacter...],
	StringStartsQ[str,"//"]
];

Tokenizer[filepath_]:=Block[
	{
		rawData={},line,
		lineCount=1,blankCount=1,
		
		tokenizer,
		library={},undefined={},
		settings={},sections={},
		
		messages={},
		syntax=syntaxTemplate,
		sectionData={},(*trackData,*)
		
		source,
		comments={},
		
		context="End",
		contextData={},
		command,value
	},
	
	If[FileExistsQ[filepath],
		rawData=Import[filepath,"List"],
		AppendTo[messages,<|"Type"->"FileNotFound","Arguments"->filepath|>]
	];
	
	While[StringStartsQ[rawData[[lineCount]],"//"],
		AppendTo[comments,rawData[[lineCount]]];
		lineCount++;
	];
	tokenizer=<|"Comments"->comments|>;
	
	While[lineCount<=Length[rawData],
		line=rawData[[lineCount]];
		If[!StringMatchQ[line,WhitespaceCharacter...],
			If[StringStartsQ[line,"#"],
			
				(* command *)
				{command,value}=StringCases[cmd:WordCharacter..~~WhitespaceCharacter...~~arg___:>Sequence[cmd,arg]][line];
				If[contextData!={},
					Switch[context,
						"ChordNotation",
							syntax[["ChordNotation"]]=Union[syntax[["ChordNotation"]],contextData[[All,"Notation"]]],
						"ChordOperator",
							syntax[["ChordOperator"]]=Union[syntax[["ChordOperator"]],contextData[[All,"Notation"]]],
						"Function",
							syntax[["FunctionList"]]=Union[syntax[["FunctionList"]],contextData[[All,"Name"]]];
							syntax[["FunctionSimp"]]=Union[syntax[["FunctionSimp"]],
								Association[If[#Syntax=={},Nothing,#Name->#Syntax[[1]]]&/@contextData]
							]
					];
					AppendTo[library,<|"Type"->context,"Storage"->"Internal","Data"->contextData|>];
					contextData={};
				];
				Switch[command,
					_?(MemberQ[contextList,#]&),
						context=command,
					_?(MemberQ[settingList,#]&),
						AppendTo[settings,command->Switch[command,
							"StaffDisplay",Identity,
							_,ToExpression
						][value]],
					"Include",
						source=Tokenizer[If[StringStartsQ[value,"\"./"],
							DirectoryName[filepath]<>StringTake[value,{4,-2}],
							StringTake[value,{2,-2}]
						]];
						Do[
							syntax[[item]]=Union[syntax[[item]],source[["Syntax",item]]],
						{item,syntaxList}];
						AppendTo[library,<|"Type"->"Package","Storage"->"External","Content"->source[["Tokenizer","Library"]]|>],
					_,
						AppendTo[undefined,<|"Type"->"FalseCmd","Arguments"->command|>]
				],
				
				(* code *)
				Switch[context,
					"ChordNotation",
						AppendTo[contextData,chordCodeTok[line]],
					"ChordOperator",
						AppendTo[contextData,chordOpCodeTok[line]],
					"Function",
						lineCount++;
						While[lineCount<=Length@rawData&&!StringMatchQ[line,functionCode],
							line=line<>"\n"<>rawData[[lineCount]];lineCount++;
						];
						AppendTo[contextData,functionCodeTok[line]],
					"End",
						Break[];
				]
			];
		];
		lineCount++;
	];
	tracktok=TrackTokenize[syntax];
	
	comments={};
	While[lineCount<=Length[rawData],
		line=rawData[[lineCount]];
		If[blankLineQ[line],
		
			(* new section *)
			If[sectionData!={},
				AppendTo[sections,<|
					"Comments"->comments,
					"Tracks"->sectionData
				|>];
				comments={};
				sectionData={};
			];
			If[StringStartsQ[line,"//"],AppendTo[comments,line]];
			blankCount++,
			
			(* music score *)
			lineCount++;
			blankCount=0;
			While[lineCount<=Length@rawData&&!blankLineQ[rawData[[lineCount]]],
				line=line<>"\n"<>rawData[[lineCount]];lineCount++;
			];
			AppendTo[sectionData,tracktok@line];
		];
		lineCount++;
	];
	If[sectionData!={},AppendTo[sections,<|
		"Comments"->comments,
		"Tracks"->sectionData
	|>]];
	
	AppendTo[tokenizer,{
		"Library"->library,
		"Settings"->settings,
		"Sections"->sections,
		"Undefined"->undefined
	}];
	
	Return[<|
		"Syntax"->syntax,
		"Tokenizer"->tokenizer,
		"Messages"->messages
	|>];
	
];

EndPackage[];


(* ::Input:: *)
(*Contexts["SMML`*"]*)


(* ::Input:: *)
(*Export[NotebookDirectory[]<>"test.json",%];*)


(* ::Input:: *)
(*Tokenizer[NotebookDirectory[]<>"test.sml"][["Tokenizer","Sections"]]*)
