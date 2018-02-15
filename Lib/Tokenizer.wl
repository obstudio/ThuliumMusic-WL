(* ::Package:: *)

(* ::Input:: *)
(*StringMatchQ["&",RegularExpression["[^\\{\\}]*"]]*)


BeginPackage["SMML`Tokenizer`"];

RE=RegularExpression;
rep[pat_]:=rep[pat,","~~" "...];
rep[pat_,sep_]:=pat~~(sep~~pat)...;
repRegex[pat_]:=repRegex[pat,", *"];
repRegex[pat_,sep_]:=RE[pat<>"("<>sep<>pat<>")*"];
sel[pat_]:=RE["("<>pat[[1]]<>")?"];
or[pat__]:=RE["("<>StringRiffle[#[[1]]&/@{pat},"|"]<>")"];
unsigned=RE["\\d+"];
signed=RE["[\\+\\-]\\d+"];
integer=RE["[\\+\\-]?\\d+"];

chordCodeTok[str_]:=StringCases[str,
	StringExpression[
		ntt:LetterCharacter~~"\t"..,
		cmt:Shortest[__]~~"\t"..,
		pts:repRegex["(([\\+\\-]?\\d+)|(\\[([\\+\\-]?\\d+)?(;([\\+\\-]?\\d+)?)?\\]([\\-\\+]\\d+)?))"]
	]:><|
		"Notation"->ntt,
		"Comment"->cmt,
		"Pitches"->StringCases[pts,{
			pit:integer:>{1,1,ToExpression[pit]},
			"["~~pos:sel[integer]~~"]"~~sft:sel[signed]:>{
				If[pos=="",1,ToExpression[pos]],
				If[pos=="",-1,ToExpression[pos]],
				If[sft=="",0,ToExpression[sft]]
			},
			"["~~pos1:sel[integer]~~";"~~pos2:sel[integer]~~"]"~~sft:sel[signed]:>{
				If[pos1=="",1,ToExpression[pos1]],
				If[pos2=="",-1,ToExpression[pos2]],
				If[sft=="",0,ToExpression[sft]]
			}
		}]
	|>
][[1]];

rif[lst_]:=rif[lst,WhitespaceCharacter...];
rif[lst_,sep_]:=StringExpression@@Riffle[lst,sep,{1,-1,2}];
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
		"Syntax"->StringCases[js,"/****"~~" "..~~stx:Shortest[__]~~" "..~~"****/":>stx]
	|>
][[1]];

preOperator="$"|"";
postOperator="``"|"`"|"";
volOperator=(">"|":")...;
pitOperator=("#"|"b"|"'"|",")...;
durOperator=("."|"-"|"_"|"=")...;
scaleDegree=RE["[0-7%x]"];

number=integer~~""|("."~~unsigned);
expression=(integer~~""|("/"|"."~~unsigned))|("Log2("~~unsigned~~")");
string=Except["\""|"("|")"|"{"|"}"|"["|"]"|"<"|">"]..;
subtrack=Nest[(("{"~~#~~"}")|Except["{"|"}"])...&,Except["{"|"}"]...,4];
argument=expression|("\""~~string~~"\"")|("{"~~subtrack~~"}");

(* notation *)
orderListC=""|rep[unsigned~~""|("~"~~unsigned),","];
orderListP=""|rep[unsigned~~""|("~"~~unsigned),"."];
orderTok=Union@@StringCases[#,{
	n:integer~~"~"~~m:integer:>Range[ToExpression@n,ToExpression@m],
	n:integer:>{ToExpression@n}
}]&;
notationPadded=RE["[&\\|\\s\\^\\*]*"];
notationPatt=Alternatives[
	"+"|"ToCoda"|"Coda"|"s"|"Segno"|"DC"|"DaCapo"|"DS"|"DaSegno",
	"||:"|":||"|("["~~orderListP~~".]"),
	"|"|"/"|("/"~~orderListC~~":")|"^"|"&"|"*"|Whitespace
];
notationTok=StringCases[{
	space:Whitespace:><|"Type"->"Whitespace","Content"->space|>,
	"||:":><|"Type"->"RepeatBegin"|>,
	":||":><|"Type"->"RepeatEnd"|>,
	"["~~ol:orderListP~~".]":><|"Type"->"Volta","Order"->orderTok[ol]|>,
	bl:"/"~~ol:orderListC~~":":>
		<|"Type"->"BarLine","Newline"->(bl=="\\"),"Skip"->False,"Order"->orderTok[ol]|>,
	bl:"|"|"/":>
		<|"Type"->"BarLine","Newline"->(bl=="\\"),"Skip"->(bl=="/"),"Order"->{0}|>,
	"^":><|"Type"->"Tie"|>,
	"&":><|"Type"->"PedalPress"|>,
	"*":><|"Type"->"PedalRelease"|>,
	"+"|"ToCoda"|"Coda":><|"Type"->"Coda"|>,
	"s"|"Segno":><|"Type"->"Segno"|>,
	"DC"|"DaCapo":><|"Type"->"DaCapo"|>,
	"DS"|"DaSegno":><|"Type"->"DaSegno"|>
}];

typeHead="$"|"%"|"&"|"!"|"@";
typeName=<|"$"->"String","%"->"Expression","@"->"Subtrack","&"->"Object","!"->"Number"|>;
contextList={"End","Chord","Function"};
settingList={"RecursionLimit","StaffDisplay","ForcedSpace"};
syntaxTemplate=<|"Chord"->{},"Function"->{}|>;
syntaxTags=<|
	"Chord"->"Notation",
	"Function"->{"Name","Syntax"}
|>;
blankLineQ[str_]:=Or[
	StringMatchQ[str,WhitespaceCharacter...],
	StringStartsQ[str,"//"]
];

Tokenizer[filepath_]:=Block[
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
		source,command,value,
		
		syntax=syntaxTemplate,
		chordPatt,
		pitch,pitches,note,pitchTok,
		objectPatt,objectTok,
		functionPatt,functionPattList,
		functionTok,functionTokList,
		argRule,argType,argData,
		typePatt,typeTok,
		funcName,argumentTok,
		object,trackTok
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
						source=Tokenizer[If[StringStartsQ[value,"\"./"],
							DirectoryName[filepath]<>StringTake[value,{4,-2}],
							StringTake[value,{2,-2}]
						]];
						Do[
							syntax[[item]]=Union[syntax[[item]],source[["Syntax",item]]],
						{item,{"Chord","Function"}}];
						AppendTo[library,<|
							"Type"->"Package",
							"Path"->StringTake[value,{2,-2}],
							"Content"->source[["Tokenizer","Library"]]
						|>],
					_,
						AppendTo[undefined,<|"Type"->"FalseCmd","Arguments"->command|>]
				],
				
				(* code *)
				Switch[context,
					"Chord",
						AppendTo[contextData,chordCodeTok[line]],
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
	If[StringTake[filepath,-4]==".smp",Return[return]];
	
	(* pitch *)
	chordPatt=RE["["<>syntax[["Chord"]]<>"]*"];
	pitch=scaleDegree~~pitOperator~~chordPatt;
	pitches=pitch|("["~~pitch..~~"]");
	note=preOperator~~pitches~~pitOperator~~volOperator~~durOperator~~postOperator;
	pitchTok=StringCases[
		StringExpression[
			sd:scaleDegree,
			po:pitOperator,
			ch:chordPatt
		]:><|
			"ScaleDegree"->sd,
			"PitchOperators"->po,
			"Chord"->ch
		|>
	];
	
	(* object *)
	objectPatt=("{"~~subtrack~~"}")|note;
	objectTok=StringCases[{
		"{"~~n:unsigned~~"*"~~sub:subtrack~~"}":>
			<|"Type"->"Subtrack","Content"->trackTok[sub],"Repeat"->-ToExpression@n|>,
		"{"~~sub:subtrack~~"}":>
			<|"Type"->"Subtrack","Content"->trackTok[sub],"Repeat"->Max[-1,
				StringCases[sub,"/"~~i:orderListC~~":":>orderTok[i]]
			]|>,
		StringExpression[
			pre:preOperator,
			""|"["~~pts:pitch..~~"]"|"",
			pit:pitOperator,
			vol:volOperator,
			dur:durOperator,
			pst:postOperator
		]:><|
			"Type"->"Note",
			"Pitches"->pitchTok[pts],
			"PitchOperators"->pit,
			"DurationOperators"->dur,
			"VolumeOperators"->vol,
			"Staccato"->StringCount[pst,"`"],
			"Arpeggio"->StringContainsQ[pre,"$"]
		|>
	}];
	
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
	object=("{"~~subtrack~~"}")|(notationPadded~~note~~notationPadded);
	typePatt=<|"$"->string,"%"->expression,"&"->object,"!"->number,"@"->Shortest[subtrack]|>;
	typeTok[type_,str_]:=Switch[type,
		"String",<|"Type"->"String","Content"->str|>,
		"Expression",<|"Type"->"Expression","Content"->str|>,
		"Number",<|"Type"->"Expression","Content"->str|>,
		"Subtrack",<|"Type"->"Subtrack","Content"->trackTok[str],"Repeat"->-1|>,
		"Object",<|"Type"->"Subtrack","Content"->trackTok[str],"Repeat"->-1|>
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
				StringCases[abbr,hd:typeHead~~id:DigitCharacter:>{typeName[[hd]],id}],
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
		spc:Whitespace:><|"Type"->"Whitespace","Content"->spc|>,
		func:functionPatt:>functionTok[func],
		objt:objectPatt:>objectTok[objt][[1]],
		nota:notationPatt:>notationTok[nota][[1]],
		und_:><|"Type"->"Undefined","Content"->und|>
	}];
	
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
				"Instrument"->StringCases[#,inst:(LetterCharacter~~WordCharacter..):>Sequence[inst]],
				"Proportion"->If[#=={},Null,ToExpression@#[[1]]/100]&@StringCases[#,ns:NumberString~~"%":>ns]
			|>&)/@StringCases[StringDelete[trackMeta,__~~":"],
				WordCharacter..~~""|("("~~NumberString~~"%"~~")")
			]];
			AppendTo[trackData,"Content"->trackTok@line];
			If[ContainsNone[#Type&/@trackData[["Content"]],{"Note","Subtrack"}],
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

EndPackage[];


(* ::Input:: *)
(*Tokenizer[NotebookDirectory[]<>"test.sml"][["Tokenizer","Sections",1,"Tracks",1,"Content"(*,All,"Type"*)]]*)


(* ::Input:: *)
(*Export[NotebookDirectory[]<>"test5.json",Tokenizer[NotebookDirectory[]<>"test5.sml"][["Tokenizer"]]];//Timing*)
