(* ::Package:: *)

BeginPackage["MML`Standard`BarLine`"];
Needs["MML`Standard`"];

Begin["`Private`"];
subtrack=Nest[(("{"~~#~~"}")|Except["}"])...&,Except["}"]...,8];
order=""|rep[int~~""|(".."~~int)];
getOrder[ord_]:=Union@@StringCases[ord,{
	n:int~~".."~~m:int:>Range[ToExpression@n,ToExpression@m],
	n:int:>{ToExpression@n}
}];
End[];

BarLineTokens={
	"{"~~n:int~~"*"~~sub:`Private`subtrack~~"}":>
		{"Type"->"Track","Contents"->`Private`getTrack[sub],"Repeat"->-ToExpression@n},
	"{"~~sub:`Private`subtrack~~"}":>
		{"Type"->"Track","Contents"->MML`Standard`getTrack[sub],"Repeat"->Max[0,
			StringCases[sub,"\\"|"/"~~i:`Private`order~~":":>`Private`getOrder[i]]
		]},
	bl:"\\"|"/"~~i:`Private`order~~":":>
		{"Type"->"BarLine","Newline"->(bl=="\\"),"Skip"->False,"Order"->`Private`getOrder[i]},
	bl:"\\"|"|"|"/":>
		{"Type"->"BarLine","Newline"->(bl=="\\"),"Skip"->(bl=="/"),"Order"->{0}},
	space:Whitespace:>{"Type"->"Whitespace","Content"->space},
	undef__:>{"Type"->"Undefined","Content"->undef}
};

EndPackage[];
