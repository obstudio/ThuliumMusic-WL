(* ::Package:: *)

<<(NotebookDirectory[]<>"Syntax.wl");
<<(NotebookDirectory[]<>"Standard.wl");

tokenize::nfound = "Cannot find file `1`.";

typeHead="$"|"%"|"&"|"!"|"@";
contextList={"End","Chord","Function","Track"};
settingList={
	"MaxRecursion","StaffDisplay","ForcedSpace",
	"FullName","InlineBars","ColorScheme"
};
syntaxTemplate=<|"Chord"->{},"Function"->{},"Macro"->{}|>;
syntaxTags=<|
	"Chord"->"Notation",
	"Function"->{"Name","Syntax"}
|>;
blankLineQ[str_]:=Or[
	StringMatchQ[str,WhitespaceCharacter...],
	StringStartsQ[str,"//"]
];

tokenize[filepath_]:=Block[
	{
		rawData={},line,
		lineCount=1,blankCount=1,
		tokenizer,return,
		library={},undefined={},
		settings={},sections={},
		sectionData={},comments={},
		trackData,trackMeta,
		blankTrack={},messages={},
		context="End",contextData={},
		source,depth,command,value,
		macroData={},
		
		syntax=syntaxTemplate,
		chordPatt,macroPatt,
		pitch,note,pitchTok,
		functionPatt,functionPattList,
		functionTok,functionTokList,
		argRule,argType,argData,
		typePatt,typeTok,
		funcName,argumentTok,
		object,trackTok
	},
	
	If[FileExistsQ[filepath],
		rawData=Import[filepath,"List"],
		Message[tokenize::nfound,filepath];
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
				{command,value}=StringCases[
					cmd:WordCharacter..~~WhitespaceCharacter...~~arg___:>Sequence[cmd,arg]
				][line];
				If[contextData!={},
					syntax[[context]]=Union[syntax[[context]],contextData[[All,syntaxTags[[context]]]]];
					AppendTo[library,<|"Type"->context,"Data"->contextData|>];
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
						source=tokenize[If[StringContainsQ[value,":"],
							StringTake[value,{2,-2}],
							depth=StringCount[value,"../"];
							FileNameDrop[DirectoryName[filepath],-depth]<>"/"<>StringTake[value,{2+3*depth,-2}]
						]];
						Do[
							syntax[[item]]=Union[syntax[[item]],source[["Syntax",item]]],
						{item,{"Chord","Function"}}];
						AppendTo[library,<|
							"Type"->"Package",
							"Path"->value,
							"Content"->source[["Tokenizer","Library"]]
						|>],
					_,
						AppendTo[undefined,<|"Type"->"FalseCmd","Arguments"->command|>]
				],
				
				(* code *)
				Switch[context,
					"Chord",
						AppendTo[contextData,chordCodeTok[line]],
					"Track",
						lineCount++;
						While[lineCount<=Length@rawData&&!blankLineQ[rawData[[lineCount]]],
							line=line<>"\n"<>rawData[[lineCount]];lineCount++;
						];
						trackData=StringCases[line,"<*"~~name:word~~"*>"~~score__:>Sequence[name,score]];
						If[trackData=={},
							AppendTo[messages,<|"Type"->"InvalidDef","Arguments"->line|>],
							AppendTo[syntax[["Macro"]],trackData[[1]]];
							AppendTo[macroData,trackData[[2]]];
						],
					"Function",
						lineCount++;
						While[lineCount<=Length@rawData&&!StringMatchQ[line,functionCode],
							line=line<>"\n"<>rawData[[lineCount]];lineCount++;
						];
						AppendTo[contextData,functionCodeTok[line]],
					"End",
						Break[];
				];
			];
		];
		lineCount++;
	];
	
	AppendTo[tokenizer,{
		"Library"->library,
		"Settings"->settings,
		"Undefined"->undefined
	}];
	return=<|
		"Syntax"->syntax,
		"Messages"->messages,
		"Tokenizer"->tokenizer
	|>;
	If[StringTake[filepath,-4]==".smp"&&macroData=={},Return[return]];
	
	(* note *)
	If[syntax[["Chord"]]!={},
		chordPatt=RE["["<>syntax[["Chord"]]<>"]*"];
		pitch=degree~~pitOp~~chordPatt;
		note=pitch|("["~~pitch..~~"]")~~pitOp~~volOp~~durOp~~postOp;
		pitchTok=StringCases[sd:degree~~po:pitOp~~ch:chordPatt:>
			<|"Degree"->sd,"PitOp"->po,"Chord"->ch|>
		];
	];
	
	(* function unsimplified *)
	argumentTok=StringCases[{
		arg:expression:><|"Type"->"Expression","Content"->arg|>,
		"\""~~arg:string~~"\"":><|"Type"->"String","Content"->arg|>,
		"{"~~n:unsigned~~"*"~~arg:subtrack~~"}":>
			<|"Type"->"Subtrack","Content"->trackTok[arg],"Repeat"->-ToExpression@n|>,
		"{"~~arg:subtrack~~"}":>
			<|"Type"->"Subtrack","Content"->trackTok[arg],"Repeat"->Max[-1,
				StringCases[arg,"/"~~i:orderListC~~":":>orderTok[i]]
			]|>
	}];
	funcName=Alternatives@@syntax[["Function",All,"Name"]];
	functionPattList={funcName~~"("~~rep[argument]~~")"};
	functionTokList={StringCases[#,{
		name:funcName~~"("~~arglist:rep[argument]~~")":><|
			"Type"->"FUNCTION",
			"Name"->name,
			"Simplified"->False,
			"Argument"->argumentTok[arglist]
		|>
	}][[1]]&};
	
	(* function simplified *)
	macroPatt="@"~~Alternatives@@syntax[["Macro"]];
	object=("{"~~subtrack~~"}")|(notationPadded~~note|macroPatt~~notationPadded);
	typePatt=<|"$"->string,"%"->expression,"&"->object,"!"->number,"@"->note..|>;
	typeTok[type_,str_]:=Switch[type,
		"$",<|"Type"->"String","Content"->str|>,
		"%",<|"Type"->"Expression","Content"->str|>,
		"!",<|"Type"->"Real","Content"->str|>,
		"&",<|"Type"->"Subtrack","Content"->trackTok[str],"Repeat"->-1|>,
		"@",<|"Type"->"Subtrack","Content"->StringCases[str,
			pts:pitch|("["~~pitch..~~"]")~~pit:pitOp~~vol:volOp~~dur:durOp~~pst:postOp:><|
				"Type"->"Note","Pitches"->pitchTok[pts],
				"PitOp"->pit,"DurOp"->dur,"VolOp"->vol,
				"Staccato"->StringCount[pst,"`"]
			|>
		],"Repeat"->-1|>
	];
	Do[
		argData=syntax[["Function",funcCount]];
		Do[
			AppendTo[functionPattList,StringExpression@@StringCases[abbr,{
				"\\"~~char_:>char,
				head:typeHead~~DigitCharacter:>typePatt[[head]],
				char_:>char
			}]];
			argType=#[[1]]&/@SortBy[
				StringCases[abbr,hd:typeHead~~id:DigitCharacter:>{hd,id}],
			#[[2]]&];
			argRule=StringExpression@@StringCases[abbr,{
				"\\"~~char_:>char,
				RuleDelayed[
					head:typeHead~~id:DigitCharacter,
					Pattern[Evaluate@Symbol["arg"<>id],typePatt[[head]]]
				],
				char_:>char
			}]:>Evaluate@Array[Symbol["arg"<>ToString@#]&,Length@argType];
			AppendTo[functionTokList,With[
				{tmpData=argData,tmpTok=typeTok,tmpType=argType,tmpRule=argRule},
				Function[str,<|
					"Type"->"FUNCTION",
					"Name"->tmpData[["Name"]],
					"Simplified"->True,
					"Argument"->MapThread[tmpTok,{tmpType,StringCases[str,tmpRule][[1]]}]
				|>]
			]],
		{abbr,argData[["Syntax"]]}],
	{funcCount,Length@syntax[["Function"]]}];
	
	functionPatt=Alternatives@@functionPattList;
	functionTok[str_]:=Block[{pattID},
		pattID=LengthWhile[functionPattList,!StringMatchQ[str,#]&]+1;
		Return[functionTokList[[pattID]][str]];
	];
	
	trackTok=StringCases[{
		func:functionPatt:>functionTok[func],
		"{"~~n:unsigned~~"*"~~sub:subtrack~~"}":>
			<|"Type"->"Subtrack","Content"->trackTok[sub],"Repeat"->-ToExpression@n|>,
		"{"~~sub:subtrack~~"}":>
			<|"Type"->"Subtrack","Content"->trackTok[sub],"Repeat"->Max[-1,
				StringCases[sub,"/"~~i:orderListC~~":":>orderTok[i]]
			]|>,
		pts:pitch|("["~~pitch..~~"]")~~pit:pitOp~~vol:volOp~~dur:durOp~~pst:postOp:><|
			"Type"->"Note","Pitches"->pitchTok[pts],
			"PitOp"->pit,"DurOp"->dur,"VolOp"->vol,
			"Staccato"->StringCount[pst,"`"]
		|>,
		"@"~~mcr:Alternatives@@syntax[["Macro"]]:><|"Type"->"Macrotrack","Name"->mcr|>,
		nota:notationPatt:>notationTok[nota][[1]],
		und_:><|"Type"->"Undefined","Content"->und|>
	}];
	
	(* macro tracks *)
	If[macroData!={},AppendTo[tokenizer[["Library"]],<|
		"Type"->"Track","Data"->(<|
			"Name"->syntax[["Macro",#]],
			"Content"->trackTok[macroData[[#]]]
		|>&/@Range[Length@macroData])
	|>]];
	
	(* music tokenize *)
	comments={};
	While[lineCount<=Length[rawData],
		line=rawData[[lineCount]];
		If[blankLineQ[line],
		
			(* new section *)
			If[sectionData!={},
				AppendTo[sections,<|
					"Comments"->comments,
					"Settings"->blankTrack,
					"Tracks"->sectionData
				|>];
				blankTrack={};
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
			trackData=<||>;
			trackMeta="";
			If[StringStartsQ[line,"<"~~__~~">"],
				trackMeta=StringCases[line,"<"~~mt:Shortest[__]~~">":>mt][[1]];
				line=StringDelete[line,StartOfString~~"<"~~Shortest[__]~~">"];
			];
			AppendTo[trackData,"Id"->If[StringContainsQ[trackMeta,":"],
				StringCases[trackMeta,id__~~":":>id][[1]],
			Null]];
			AppendTo[trackData,"Instruments"->(<|
				"Instrument"->StringCases[#,inst:(LetterCharacter~~WordCharacter..):>inst][[1]],
				"Proportion"->If[#=={},Null,ToExpression@#[[1]]/100]&@StringCases[#,ns:NumberString~~"%":>ns]
			|>&)/@StringCases[StringDelete[trackMeta,__~~":"],
				WordCharacter..~~""|("("~~NumberString~~"%"~~")")
			]];
			AppendTo[trackData,"Content"->trackTok[line]];
			If[ContainsNone[#Type&/@trackData[["Content"]],{"Note","Subtrack","Macrotrack"}],
				blankTrack=trackData[["Content"]],
				AppendTo[sectionData,trackData];	
			];
			
		];
		lineCount++;
	];
	If[sectionData!={},AppendTo[sections,<|
		"Comments"->comments,
		"Settings"->blankTrack,
		"Tracks"->sectionData
	|>]];
	
	AppendTo[tokenizer,"Sections"->sections];
	return[["Tokenizer"]]=tokenizer;
	Return[return];
	
];


(* ::Input:: *)
(*tokenize[NotebookDirectory[]<>"test.sml"][["Tokenizer","Sections",1,"Tracks"]]*)


(* ::Input:: *)
(*Export[NotebookDirectory[]<>"test/test7.json",tokenize[NotebookDirectory[]<>"test/test7.sml"][["Tokenizer"]]];//Timing*)
