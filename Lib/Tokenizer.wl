(* ::Package:: *)

Hold[a,b]


BeginPackage["SMML`Tokenizer`"];
Get[NotebookDirectory[]<>"TrackTok.wl"];

rep[pat_]:=rep[pat,","~~" "...];
rep[pat_,sep_]:=pat~~(sep~~pat)...;
unsigned=DigitCharacter..;
integer=""|"-"|"+"~~unsigned;
intPsb=""|integer;
chordOpCode=rep["["~~intPsb|(intPsb~~";"~~intPsb)~~"]"~~intPsb];
chordOpCodeTok[code_]:=StringCases[code,{
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
}];

rif[list_]:=rif[list,WhitespaceCharacter...];
rif[list_,sep_]:=StringExpression@@Riffle[list,sep,{1,-1,2}];
word=LetterCharacter~~WordCharacter...;
jsPlain=Except["{"|"}"]...;
jsCode=Nest[(("{"~~#~~"}")|jsPlain)...&,jsPlain,8];
functionCode=rif[{word,"("~~jsPlain~~")","{"~~jsCode~~"}"}];
functionCodeTok[code_]:=StringCases[code,StringExpression[
	name:word,
	WhitespaceCharacter...,
	"("~~jsPlain~~")",
	WhitespaceCharacter...,
	"{"~~js:jsCode~~"}"
]:>{
	"Name"->name,
	"Code"->code,
	"Syntax"->StringCases[js,"/** "~~stx:Shortest[__]~~" **/":>stx]
}][[1]];

contextList={"End","ChordNotation","ChordOperator","Declaration"};
syntaxTemplate=<|
	"ChordNotation"-><||>,
	"ChordOperator"-><||>,
	"FunctionList"->{},
	"FunctionSimp"-><||>,
	"RecursionLimit"->8
|>;

Tokenizer[filepath_]:=Block[
	{
		rawData={},line,i=1,
		messages={},tokenizer,
		context="End",
		contextData={},
		command,argument,
		library={},
		syntax=syntaxTemplate
	},
	
	
	If[FileExistsQ[filepath],
		rawData=Import[filepath,"List"],
		AppendTo[messages,<|"Type"->"FileNotFound","Arguments"->filepath|>]
	];
	
	While[i<=Length[rawData],
		line=rawData[[i]];
		Switch[line,
			_?(StringStartsQ["#"]),
				{command,argument}=StringCases[cmd:WordCharacter..~~arg___:>Sequence[cmd,arg]][line];
				If[contextData!={},
					AppendTo[library,<|
						"Type"->context,
						"Storage"->"Internal",
						"Data"->contextData
					|>];
					contextData={};
				];
				Switch[command,
					_?(MemberQ[contextList,#]&),context=command
				],
			_?(StringStartsQ["//"]),
				Null,
			_?(!StringMatchQ[#,WhitespaceCharacter...]&),
				Switch[context,
					"ChordNotation",
						AppendTo[contextData,StringCases[line,
							StringExpression[
								ntt:LetterCharacter~~"\t"..,
								cmt:Shortest[__]~~"\t"..,
								pts:rep[integer]
							]:>Sequence[
								"Notation"->ntt,
								"Comment"->cmt,
								"Pitches"->StringSplit[pts,","~~" "...]
							]
						]],
					"ChordOperator",
						AppendTo[contextData,StringCases[line,
							StringExpression[
								ntt:LetterCharacter~~"\t"..,
								cmt:Shortest[__]~~"\t"..,
								pts:chordOpCode
							]:>Sequence[
								"Notation"->ntt,
								"Comment"->cmt,
								"Pitches"->chordOpCodeTok[pts]
							]
						]],
					"Declaration",
						While[!StringMatchQ[line,functionCode]&&i<=Length@rawData,
							i++;line=line<>"\n"<>rawData[[i]];
						];
						AppendTo[contextData,StringCases[line,
							StringExpression[
								name:word,
								WhitespaceCharacter...,
								"("~~jsPlain~~")",
								WhitespaceCharacter...,
								"{"~~js:jsCode~~"}"
							]:>Sequence[
								"Name"->name,
								"Code"->line,
								"Syntax"->StringCases[js,"/** "~~stx:Shortest[__]~~" **/":>stx]
							]
						]]
				]
		];
		i++;
	];
	
	tokenizer=<|
		"Comments"->{},
		"Library"->library,
		"Settings"->{},
		"Sections"->{},
		"Undefined"->{}
	|>;
	
	Return[<|
		"Messages"->messages,
		"Tokenizer"->tokenizer
	|>];
	
];

EndPackage[];


(* ::Input:: *)
(*Contexts["SMML`*"]*)


(* ::Input:: *)
(*Export[NotebookDirectory[]<>"test.json",%];*)


(* ::Input:: *)
(*Tokenizer[NotebookDirectory[]<>"test.sml"]*)


(* ::Input:: *)
(*SMML`Tokenizer`Track`TrackTokenizer[1]["(100%)"]*)
