(* ::Package:: *)

color=RGBColor/@<|
	"Gray"->"#707070",
	"White"->"#F0F0F0",
	"Background"->"#F9F9F9"
|>;


button["Play"]:=Graphics[{
	color[["White"]],
	Disk[{0,0},1],
	color[["Gray"]],Thickness[0.06],
	Circle[{0,0},0.94],
	Triangle[{{-0.2,-0.4},{-0.2,0.4},{0.4,0}}],
	Thickness[0.08],JoinForm["Round"],CapForm["Round"],
	Line[{{-0.2,-0.4},{-0.2,0.4},{0.4,0},{-0.2,-0.4}}]
}];
button["Pause"]:=Graphics[{
	color[["White"]],
	Disk[{0,0},1],
	color[["Gray"]],Thickness[0.06],
	Circle[{0,0},0.94],
	Thickness[0.1],CapForm["Round"],
	Line[{{-0.3,-0.36},{-0.3,0.36}}],
	Line[{{0.3,-0.36},{0.3,0.36}}]
}];
button["Stop"]:=Graphics[{
	color[["White"]],
	Disk[{0,0},1],
	color[["Gray"]],Thickness[0.06],
	Circle[{0,0},0.94],	
	Rectangle[{-0.4,-0.4},{0.4,0.4},RoundingRadius->{0.1,0.1}]
}];
button["Return"]:=Graphics[{
	color[["White"]],
	Disk[{0,0},1],
	color[["Gray"]],Thickness[0.06],
	Circle[{0,0},0.94],	
	Thickness[0.1],CapForm["Round"],JoinForm["Round"],
	Line[{{-0.4,0},{0.4,0}}],
	Line[{{0,-0.4},{-0.4,0},{0,0.4}}]
}];
button[name_,size_]:=Graphics[button[name],ImageSize->size];


(* ::Input:: *)
(*button/@{"Play","Pause","Stop","Return"}*)
