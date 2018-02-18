(* ::Package:: *)

number=integer~~""|("."~~unsigned);
expression=(integer~~""|("/"|"."~~unsigned))|("Log2("~~unsigned~~")");
string=Except["\""|"("|")"|"{"|"}"|"["|"]"|"<"|">"]..;
subtrack=Nest[(("{"~~#~~"}")|Except["{"|"}"])...&,Except["{"|"}"]...,4];
argument=expression|("\""~~string~~"\"")|("{"~~subtrack~~"}");

(* notation *)
orderListC="(\\d+(~\\d+)?(,\\d+(~\\d+)?)*)?";
orderListP="(\\d+(~\\d+)?\\.)*";
orderTok=Union@@StringCases[#,{
	n:integer~~"~"~~m:integer:>Range[ToExpression@n,ToExpression@m],
	n:integer:>{ToExpression@n}
}]&;
notationPadded=RE["[&\\|\\s\\^\\*]*"];
notationPatt=Alternatives[
	"+"|"ToCoda"|"Coda"|"s"|"Segno"|"DC"|"DaCapo"|"DS"|"DaSegno",
	"||:"|":||"|("["~~orderListP~~".]")|Whitespace,
	("/"~~orderListC~~":")|"|"|"/"|"^"|"&"|"*"
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

(* pitch *)
postOp=RE["`{0,2}"];
volOp=RE["[:>]*"];
pitOp=RE["[#b',]*"];
durOp=RE["[\\-._=]*"];
degree=RE["[0-7%x]"];
chordPatt=RE["[adhHijkmMpoPqQstTu]*"];
pitch=degree~~pitOp~~chordPatt;
note=pitch|("["~~pitch..~~"]")~~pitOp~~volOp~~durOp~~postOp;
pitchTok=StringCases[sd:degree~~po:pitOp~~ch:chordPatt:>
	<|"Degree"->sd,"PitOp"->po,"Chord"->ch|>
];
	




