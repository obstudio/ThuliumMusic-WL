(* ::Package:: *)

RE=RegularExpression;
rep[pat_]:=rep[pat,","~~" "...];
rep[pat_,sep_]:=pat~~(sep~~pat)...;
repRegex[pat_]:=repRegex[pat,", *"];
repRegex[pat_,sep_]:=RE[pat<>"("<>sep<>pat<>")*"];
join[pat__]:=RE[StringJoin[#[[1]]&/@{pat}]];
unsigned=RE["\\d+"];
signed=RE["[\\+\\-]\\d+"];
integer=RE["[\\+\\-]?\\d+"];
word=RE["[a-zA-Z]\\w*"];

tokenize::chord = "`1` is not a invalid chord code.";
tokenize::function = "`1` is not a invalid function code.";

chordUnit="(([\\+\\-]?\\d+)|(\\[([\\+\\-]?\\d+)?(;([\\+\\-]?\\d+)?)?\\]([\\-\\+]\\d+)?))";
chordCode=RE["[a-zA-Z]\\t+.*\\t+"<>chordUnit<>"(, *"<>chordUnit<>")*"];
chordCodeTok[str_]:=If[StringMatchQ[str,chordCode],
	StringCases[str,
		ntt:LetterCharacter~~"\t"..~~cmt:___~~"\t"..~~pts:RE[chordUnit<>"(, *"<>chordUnit<>")*"]:><|
			"Notation"->ntt,
			"Comment"->cmt,
			"Pitches"->StringCases[pts,{
				pit:integer:>{1,1,ToExpression[pit]},
				"["~~pos:RE["([\\+\\-]?\\d+)?"]~~"]"~~sft:RE["([\\+\\-]\\d+)?"]:>{
					If[pos=="",1,ToExpression[pos]],
					If[pos=="",-1,ToExpression[pos]],
					If[sft=="",0,ToExpression[sft]]
				},
				"["~~pos1:RE["([\\+\\-]?\\d+)?"]~~";"~~pos2:RE["([\\+\\-]?\\d+)?"]~~"]"~~sft:RE["([\\+\\-]\\d+)?"]:>{
					If[pos1=="",1,ToExpression[pos1]],
					If[pos2=="",-1,ToExpression[pos2]],
					If[sft=="",0,ToExpression[sft]]
				}
			}]
		|>
	][[1]],
	Message[tokenize::chord,str];
	Return[Nothing];
];

jsCode=Nest[(("{"~~#~~"}")|Except["{"|"}"])...&,Except["{"|"}"]...,8];
functionCode=RE["\\s*function *[a-zA-Z]\\w*\\s*\\([^\\{\\}]*\\)\\s*\\{"]~~jsCode~~RE["\\}\\s*"];
functionCodeTok[str_]:=If[StringMatchQ[str,functionCode],
	StringCases[str,
		name:RE["[a-zA-Z]\\w*"]~~RE["\\s*\\([^\\{\\}]*\\)\\s*\\{"]~~js:jsCode~~RE["\\}"]:><|
			"Name"->name,
			"Code"->str,
			"VoidQ"->StringEndsQ[js,"return"~~jsCode],
			"Syntax"->StringCases[js,"/****"~~" "..~~stx:Shortest[__]~~" "..~~"****/":>stx]
		|>
	][[1]],
	Message[tokenize::function,str];
	Return[Nothing];
];

number=integer~~""|("."~~unsigned);
numsigned="+"|"-"~~numunsigned;
numunsigned=unsigned~~""|("."~~unsigned);
expression=RE["[+\\-]?\\d+([\\./]\\d*)?|Log2\\(\\d+\\)([+\\-]\\d+)?"];
expunsigned=RE["\\d+([\\./]\\d*)?|Log2\\(\\d+\\)([+\\-]\\d+)?"];
string=Except["\""|"("|")"|"{"|"}"|"["|"]"|"<"|">"]..;
subtrack=Nest[(("{"~~#~~"}")|Except["{"|"}"])...&,Except["{"|"}"]...,4];
argument=expression|("\""~~string~~"\"")|("{"~~subtrack~~"}");

(* notation *)
orderListC=RE["(\\d+(~\\d+)?(,\\d+(~\\d+)?)*)?"];
orderListP=RE["(\\d+(~\\d+)?\\.)*"];
orderTok=Union@@StringCases[#,{
	n:integer~~"~"~~m:integer:>Range[ToExpression@n,ToExpression@m],
	n:integer:>{ToExpression@n}
}]&;
notationPadded=RE["[&\\|\\s\\^\\*]*"];
notationPatt=Alternatives[
	"!"|"+"|"ToCoda"|"Coda"|"s"|"Segno"|"DC"|"DaCapo"|"DS"|"DaSegno"|"Fine",
	"||:"|":||"|("["~~orderListP~~"]")|Whitespace,
	("\\"~~orderListC~~":")|"|"|"/"|"\\"|"^"|"&"|"*"
];
notationTok=StringCases[{
	space:Whitespace:><|"Type"->"Whitespace","Content"->space|>,
	"||:":><|"Type"->"RepeatBegin"|>,
	":||":><|"Type"->"RepeatEnd"|>,
	"["~~ol:orderListP~~"]":><|"Type"->"Volta","Order"->orderTok[ol]|>,
	"\\"~~ol:orderListC~~":":>
		<|"Type"->"BarLine","Skip"->False,"Overlay"->False,"Order"->orderTok[ol]|>,
	bl:"|"|"\\"|"/":>
		<|"Type"->"BarLine","Skip"->(bl=="\\"),"Overlay"->(bl=="/"),"Order"->{0}|>,
	"^":><|"Type"->"Tie"|>,
	"&":><|"Type"->"PedalPress"|>,
	"*":><|"Type"->"PedalRelease"|>,
	"+"|"ToCoda"|"Coda":><|"Type"->"Coda"|>,
	"s"|"Segno":><|"Type"->"Segno"|>,
	"DC"|"DaCapo":><|"Type"->"DaCapo"|>,
	"DS"|"DaSegno":><|"Type"->"DaSegno"|>,
	"Fine":><|"Type"->"Fine"|>,
	"!":><|"Type"->"Local"|>
}];


