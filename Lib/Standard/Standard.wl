(* ::Package:: *)

(* ::Input:: *)
(*While[$Context!="Global`",End[]];*)


BeginPackage["MML`Standard`"];

keyDict=<|
	"C"->0,"G"->7,"D"->2,"A"->9,"E"->4,
	"B"->-1,"#F"->6,"#C"->1,"F"->5,"bB"->-2,
	"bE"->3,"bA"->8,"bD"->1,"bG"->6,"bC"->-1,
	"F#"->6,"C#"->1,"Bb"->-2,"Gb"->6,
	"Eb"->3,"Ab"->8,"Db"->1,"Cb"->-1
|>;
rep[pat_]:=rep[pat,","];
rep[pat_,sep_]:=pat~~(sep~~pat)...;
int=DigitCharacter..;
name=LetterCharacter~~WordCharacter...;
real=DigitCharacter...~~"."~~DigitCharacter...;
key=Alternatives@Keys@keyDict;

Begin["`Private`"];
path=NotebookDirectory[];
Get[path<>"FuncSimp.wl"];
Get[path<>"BarLine.wl"];
End[];

getTrack[score_]:=StringCases[score,Join[
	`FuncSimp`FuncSimpTokens,
	`BarLine`BarLineTokens
]];

EndPackage[];
