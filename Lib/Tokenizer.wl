(* ::Package:: *)

BeginPackage["SMML`Tokenizer`"];

rep[pat_]:=rep[pat,","];
rep[pat_,sep_]:=pat~~(sep~~pat)...;
unsigned=DigitCharacter..;
integer=""|"-"|"+"~~unsigned;

Get[NotebookDirectory[]<>"TrackTok.wl"];

contextList={"Default","ChordNotation","ChordOperator","FunctionPackage"};
contextAbbr=AssociationMap[#&,contextList]~Join~<|
	"End"->"Default",
	"Chord"->"ChordNotation",
	"ChordOp"->"ChordOperator",
	"Function"->"FunctionPackage"
|>;

syntaxTemplate=<|
	"ChordNotation"-><||>,
	"ChordOperator"-><||>,
	"FunctionList"->{},
	"RecursionLimit"->8
|>;

Tokenizer[filepath_]:=Module[
	{
		data={},lineCount=0,
		messages={},tokenizer={},
		context="Default",
		command,argument,
		library={},
		syntax=syntaxTemplate
	},
	
	If[FileExistsQ[filepath],
		data=Import[filepath,"List"],
		AppendTo[messages,<|"Type"->"FileNotFound","Arguments"->filepath|>]
	];
	
	Do[
		lineCount++;
		Switch[line,
			_?(StringStartsQ["#"]),
				{command,argument}=StringCases[cmd:WordCharacter..~~arg___:>Sequence[cmd,arg]][line];
				Switch[command,
					_?(MemberQ[Keys@contextAbbr,#]&),context=contextAbbr[[command]]
				],
			_?(StringStartsQ["//"]),
				Null,
			_?(!StringMatchQ[#,WhitespaceCharacter...]&),
				Switch[context,
					"ChordNotation",
						StringCases[line,
							StringExpression[
								ntt:LetterCharacter~~"\t"..,
								cmt:Shortest[__]~~"\t"..,
								pts:rep[integer,","~~" "...]
							]:>Sequence[
								"Notation"->ntt,
								"Comment"->cmt,
								"Pitches"->StringSplit[pts,","~~" "...]
							]
						]//Print
				]
		],
	{line,data}];
	
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
