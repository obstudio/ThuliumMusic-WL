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
	]:>Sequence[
		"Notation"->ntt,
		"Comment"->cmt,
		"Pitches"->StringSplit[pts,","~~" "...]
	]
];

intPsb=""|integer;
chordOpCode=rep["["~~intPsb|(intPsb~~";"~~intPsb)~~"]"~~intPsb];
chordOpCodeTok[str_]:=StringCases[line,
	StringExpression[
		ntt:LetterCharacter~~"\t"..,
		cmt:Shortest[__]~~"\t"..,
		pts:chordOpCode
	]:>Sequence[
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
	]
];

rif[list_]:=rif[list,WhitespaceCharacter...];
rif[list_,sep_]:=StringExpression@@Riffle[list,sep,{1,-1,2}];
word=LetterCharacter~~WordCharacter...;
jsPlain=Except["{"|"}"]...;
jsCode=Nest[(("{"~~#~~"}")|jsPlain)...&,jsPlain,8];
functionCode=rif[{word,"("~~jsPlain~~")","{"~~jsCode~~"}"}];
functionCodeTok[str_]:=StringCases[str,
	StringExpression[
		name:word,
		WhitespaceCharacter...,
		"("~~jsPlain~~")",
		WhitespaceCharacter...,
		"{"~~js:jsCode~~"}"
	]:>Sequence[
		"Name"->name,
		"Code"->str,
		"Syntax"->StringCases[js,"/** "~~stx:Shortest[__]~~" **/":>stx]
	]
];

contextList={"End","ChordNotation","ChordOperator","Declaration"};
settingList={"RecursionLimit","StaffDisplay","ForcedSpace"};
syntaxTemplate=<|
	"ChordNotation"-><||>,
	"ChordOperator"-><||>,
	"FunctionList"->{},
	"FunctionSimp"-><||>,
	"Typesetting"-><||>
|>;

Tokenizer[filepath_]:=Block[
	{
		rawData={},line,i=1,
		messages={},tokenizer,
		context="End",
		contextData={},
		scoreData={},
		command,argument,
		library={},
		settings={},
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
				{command,argument}=StringCases[cmd:WordCharacter..~~WhitespaceCharacter...~~arg___:>Sequence[cmd,arg]][line];
				If[contextData!={},
					AppendTo[library,<|
						"Type"->context,
						"Storage"->"Internal",
						"Data"->contextData
					|>];
					contextData={};
				];
				Switch[command,
					_?(MemberQ[contextList,#]&),
						context=command,
					_?(MemberQ[settingList,#]&),
						If[KeyExistsQ[settings,command],
							AppendTo[messages,<|"Type"->"RepDecl","Arguments"->command|>]
						];
						AppendTo[settings,command->Switch[command,
							"StaffDisplay",Identity,
							_,ToExpression
						][argument]],
					_,
						AppendTo[messages,<|"Type"->"FalseCmd","Arguments"->command|>]
				],
			_?(StringStartsQ[#,"//"]||StringMatchQ[#,WhitespaceCharacter...]&),
				Null,
			_,
				Switch[context,
					"ChordNotation",
						AppendTo[contextData,chordCodeTok[line]],
					"ChordOperator",
						AppendTo[contextData,chordOpCodeTok[line]],
					"Declaration",
						i++;
						While[i<=Length@rawData&&!StringMatchQ[line,functionCode],
							line=line<>"\n"<>rawData[[i]];i++;
						];
						AppendTo[contextData,functionCodeTok[line]],
					"End",
						i++;
						While[i<=Length@rawData&&!StringMatchQ[rawData[[i]],WhitespaceCharacter...],
							line=line<>"\n"<>rawData[[i]];i++;
						];
						AppendTo[scoreData,line]
				]
		];
		i++;
	];
	
	tokenizer=<|
		"Comments"->{},
		"Library"->library,
		"Settings"->settings,
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


(* ::Input:: *)
(*StringMatchQ["Tremolo1 (expr, subtrack) { /** (%1-)#2 **/\nTremolo1 (expr, subtrack) { /** (%1-)#2 **/\n}",WhitespaceCharacter...~~LetterCharacter~~WordCharacter...~~WhitespaceCharacter...~~"("~~Except["{"|"}"]...~~")"~~WhitespaceCharacter...~~"{"~~(("{"~~(("{"~~(("{"~~(("{"~~(("{"~~(("{"~~(("{"~~(("{"~~Except["{"|"}"]...~~"}")|(Except["{"|"}"]...))...~~"}")|(Except["{"|"}"]...))...~~"}")|(Except["{"|"}"]...))...~~"}")|(Except["{"|"}"]...))...~~"}")|(Except["{"|"}"]...))...~~"}")|(Except["{"|"}"]...))...~~"}")|(Except["{"|"}"]...))...~~"}")|(Except["{"|"}"]...))...~~"}"~~WhitespaceCharacter...]*)



