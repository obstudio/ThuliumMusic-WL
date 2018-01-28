(* ::Package:: *)

BeginPackage["MML`Standard`FuncSimp`"];
Needs["MML`Standard`"];

Begin["`Private`"];

End[];

FuncSimpTokens={
	"<"~~speed:int~~">":>
		{"Type"->"FunctionToken","Simplified"->True,"Argument"->{"Speed"->ToExpression@speed}},
	"<"~~bar:int~~"/"~~beat:int~~">":>
		{"Type"->"FunctionToken","Simplified"->True,"Argument"->{"Bar"->ToExpression@bar,"Beat"->ToExpression@beat}}
};

EndPackage[];
