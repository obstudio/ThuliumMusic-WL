(* ::Package:: *)

(* QYMP Logo *)
LogoCloud="M836.15,454.53c90.25,18.32,158.21,98.11,158.21,193.791c0,109.22-88.54,197.729-197.75,197.729h-568.5
c-122.87,0-222.46-99.6-222.46-222.46c0-96.97,62.07-179.44,148.62-209.9c-0.22-4.15-0.32-8.34-0.32-12.57
c0-136.5,110.67-247.17,247.18-247.17c91.59,0,171.54,49.82,214.24,123.84c23.55-15.77,51.88-24.97,82.37-24.97
c81.91,0,148.29,66.41,148.29,148.3c0,18.84-3.511,36.84-9.9,53.41";
LogoNote="M569.07,330.54c0,0-3.4-3.95-12.53-10.1
c-28.78-19.4-64.45-29.11-98.33-31.92c-10.33-0.86-23.36-1.46-33.73-1.62c-25.17-0.38-53.61,3.05-76.72,14.4L492.81,615.11
c-22.67-12.301-52.2-18.33-83.75-15.301c-65.69,6.311-115.29,49.521-110.78,96.53c4.51,47,61.43,79.99,127.13,73.69
c65.69-6.311,115.29-49.53,110.78-96.53c-0.53-5.48-1.771-10.77-3.65-15.83h-0.01L424.34,315.88c6.1-2.72,35.51-4.39,52.38-4.1
c22.98,0.39,59.32,7.82,69.48,10.78C555.89,325.38,569.07,330.54,569.07,330.54z";


logo=Scale[{
	RGBColor["#00A0E9"],
	FilledCurve[{BezierCurve[CurveMerge[#Segment]]}]&[SVGPathD[LogoCloud][[1]]],
	RGBColor["#FFFFFF"],
	FilledCurve[{BezierCurve[CurveMerge[#Segment]]}]&[SVGPathD[LogoNote][[1]]]
},{1,-1}];


(* ::Input:: *)
(*Graphics[{Scale[{*)
(*Texture[Table[{c,1-c,1},{c,0,1,1/256}]],*)
(*FilledCurve[{BezierCurve[CurveMerge[#Segment]]},*)
(*VertexTextureCoordinates->VertexAssign[CurveMerge[#Segment],{0,0}->0,{1000,0}->1]*)
(*]&[SVGPathD[LogoCloud][[1]]],*)
(*White,*)
(*FilledCurve[{BezierCurve[CurveMerge[#Segment]]}*)
(*]&[SVGPathD[LogoNote][[1]]]},*)
(*{1,-1}]}]*)


(* progress bar *)
progressController[prog_,len_]:=Graphics[{
	CapForm["Round"],JoinForm["Round"],
	Texture[Table[{c,1-c,1},{c,1/100,1/2,1/100}]],
	Polygon[{{-0.04,-0.96},{-0.04,0.96},{prog*len,0.96},{prog*len,-0.96}},
		VertexTextureCoordinates->{{0},{0},{1},{1}}
	],
	RGBColor["#00FFFF"],
	Disk[{0,0},0.96,{Pi/2,3Pi/2}],
	Rectangle[{0,-0.96},{0.2,0.96}],
	RGBColor["#B0B0B0"],Thickness[0.004],
	Circle[{0,0},0.96,{Pi/2,3Pi/2}],
	Circle[{len,0},0.96,{-Pi/2,Pi/2}],
	Line[{{0,0.96},{len,0.96}}],
	Line[{{0,-0.96},{len,-0.96}}],
	RGBColor["#F0F0F0"],
	Disk[{prog*len,0},1.56],
	RGBColor["#0088FF"],
	Disk[{prog*len,0},0.56],
	RGBColor["#909090"],Thickness[0.004],
	Circle[{prog*len,0},1.6],
	Circle[{prog*len,0},0.56]	
}];


(* basic graphics *)
squareRounded[t_,r_,scheme_]:=If[r==1,
	GraphicsGroup[{
		scheme[["Grounding"]],Disk[{0,0},1-t],
		scheme[["Margin"]],Thickness[t],Circle[{0,0},1-t]
	}],
	GraphicsGroup[{
		scheme[["Grounding"]],
		Rectangle[{t-1,t-1},{1-t,1-t},RoundingRadius->{r-t,r-t}],
		scheme[["Margin"]],Thickness[t],CapForm["Round"],
		Circle[{r-1,r-1},r-t,{Pi,3Pi/2}],Circle[{1-r,1-r},r-t,{0,Pi/2}],
		Circle[{1-r,r-1},r-t,{-Pi/2,0}],Circle[{r-1,1-r},r-t,{Pi/2,Pi}],
		Line[{{r-1,t-1},{1-r,t-1}}],Line[{{r-1,1-t},{1-r,1-t}}],
		Line[{{t-1,r-1},{t-1,1-r}}],Line[{{1-t,r-1},{1-t,1-r}}]
	}]
];


(* page selector *)
pageSelectorDisplay[num_Integer,style_,size_]:=If[style=="Default",
	Mouseover[pageSelectorDisplay[num,"Basic",size],pageSelectorDisplay[num,"Mouseover",size]],
	Block[{scheme=pageSelectorColor[[style]]},Graphics[{
		squareRounded[0.06,0.3,scheme],
		Text[num,BaseStyle->{
			FontWeight->If[StringContainsQ[style,"Current"],Bold,Plain],
			FontSize->size,
			FontColor->scheme[["Body"]]
		}]
	}]]
];
pageSelectorDisplay[name_String,style_]:=If[style=="Default",
	Mouseover[pageSelectorDisplay[name,"Basic"],pageSelectorDisplay[name,"Mouseover"]],
	Block[{scheme=pageSelectorColor[[style]]},Graphics[{
		squareRounded[0.06,0.3,scheme],
		scheme[["Body"]],
		pageSelectorData[name]
	}]]
];
pageSelectorData=<|
	"Prev"->GraphicsGroup[{
		Thickness[0.08],CapForm["Round"],JoinForm["Round"],
		Line[{{0.32,0.48},{-0.36,0},{0.32,-0.48}}]
	}],
	"Next"->GraphicsGroup[{
		Thickness[0.08],CapForm["Round"],JoinForm["Round"],
		Line[{{-0.32,0.48},{0.36,0},{-0.32,-0.48}}]
	}],
	"First"->GraphicsGroup[{
		Thickness[0.08],CapForm["Round"],JoinForm["Round"],
		Line[{{-0.4,-0.48},{-0.4,0.48}}],
		Line[{{0.44,0.48},{-0.16,0},{0.44,-0.48}}]
	}],
	"Last"->GraphicsGroup[{
		Thickness[0.08],CapForm["Round"],JoinForm["Round"],
		Line[{{0.4,-0.48},{0.4,0.48}}],
		Line[{{-0.44,0.48},{0.16,0},{-0.44,-0.48}}]
	}]
|>;


(* button *)
buttonDisplay[name_]:=buttonDisplay[name,"Default"];
buttonDisplay[name_,style_]:=If[style=="Default",
	Mouseover[buttonDisplay[name,"Basic"],buttonDisplay[name,"Mouseover"]],
	Block[{scheme=buttonColor[[style]]},Graphics[{
		squareRounded[0.06,1,scheme],
		scheme[["Body"]],buttonData[[name]]
	}]]
];
buttonData=<|
	"Play"->GraphicsGroup[{
		Thickness[0.08],JoinForm["Round"],CapForm["Round"],
		Triangle[{{-0.2,-0.4},{-0.2,0.4},{0.4,0}}],
		Line[{{-0.2,-0.4},{-0.2,0.4},{0.4,0},{-0.2,-0.4}}]
	}],
	"Pause"->GraphicsGroup[{
		Rectangle[{-0.4,-0.4},{-0.08,0.4},RoundingRadius->{0.1,0.1}],
		Rectangle[{0.08,-0.4},{0.4,0.4},RoundingRadius->{0.1,0.1}]
	}],
	"Stop"->GraphicsGroup[{
		Rectangle[{-0.4,-0.4},{0.4,0.4},RoundingRadius->{0.1,0.1}]
	}],
	"ArrowL"->GraphicsGroup[{
		Thickness[0.1],CapForm["Round"],JoinForm["Round"],
		Line[{{-0.4,0},{0.4,0}}],
		Line[{{0,-0.4},{-0.4,0},{0,0.4}}]
	}],
	"ArrowR"->GraphicsGroup[{
		Thickness[0.1],CapForm["Round"],JoinForm["Round"],
		Line[{{-0.4,0},{0.4,0}}],
		Line[{{0,-0.4},{0.4,0},{0,0.4}}]
	}],
	"Settings"->GraphicsGroup[{
		Thickness[0.12],
		Circle[{0,0},0.3],
		Table[Rotate[
			Rectangle[{-0.15,0.3},{0.15,0.53},RoundingRadius->{0.05,0.05}],
		theta,{0,0}],{theta,0,2Pi,Pi/3}]
	}],
	"Add"->GraphicsGroup[{
		Thickness[0.12],CapForm["Round"],
		Line[{{0,-0.4},{0,0.4}}],
		Line[{{-0.4,0},{0.4,0}}]
	}],
	"About"->GraphicsGroup[{
		Thickness[0.1],CapForm["Round"],
		Line[{{0,-0.44},{0,0.1}}],
		PointSize[0.1],
		Point[{0,0.44}]
	}],
	"Exit"->GraphicsGroup[{
		Thickness[0.08],JoinForm["Round"],CapForm["Round"],
		Line[{{0.24,-0.24},{0.24,-0.4},{-0.36,-0.4},{-0.36,0.4},{0.24,0.4},{0.24,0.24}}],
		Thickness[0.06],
		Line[{{0,0},{0.52,0}}],
		Line[{{0.4,0.12},{0.52,0},{0.4,-0.12}}]
	}],
	"Modify"->GraphicsGroup[{
		Thickness[0.06],JoinForm["Round"],CapForm["Round"],
		Line[{{-0.4,-0.4},{-0.36,-0.16},{0.24,0.44},{0.44,0.24},{-0.16,-0.36},{-0.4,-0.4}}],
		Line[{{0.12,-0.4},{0.4,-0.4}}],
		Thickness[0.04],
		Line[{{0.12,0.32},{0.32,0.12}}],
		Line[{{-0.12,-0.32},{-0.32,-0.12}}]
	}],
	"PrevSong"->GraphicsGroup[{
		Thickness[0.06],CapForm["Round"],JoinForm["Round"],
		Line[{{-0.32,-0.36},{-0.32,0.36}}],
		Triangle[{{0.36,-0.36},{0.36,0.36},{-0.16,0}}],
		Line[{{0.36,-0.36},{0.36,0.36},{-0.16,0},{0.36,-0.36}}]
	}],
	"NextSong"->GraphicsGroup[{
		Thickness[0.06],CapForm["Round"],JoinForm["Round"],
		Line[{{0.32,-0.36},{0.32,0.36}}],
		Triangle[{{-0.36,-0.36},{-0.36,0.36},{0.16,0}}],
		Line[{{-0.36,-0.36},{-0.36,0.36},{0.16,0},{-0.36,-0.36}}]
	}],
	"EnterPlaylist"->With[{l=0.36,w=0.44,a=0.12,q=0.08,p=-0.16,l1=-0.16,w1=0.24,w2=0.08},
	GraphicsGroup[{
		Thickness[0.06],CapForm["Round"],JoinForm["Round"],
		Line[{{l,p-0.16},{l,-w},{-l,-w},{-l,w},{l-0.2,w},{l,w-0.2},{l,p+0.16}}],
		Thickness[0.04],
		Line[{{q,p},{0.52,p}}],Line[{{q+a,p-a},{q,p},{q+a,p+a}}],
		Line[{{l1,w1},{0.08,w1}}],Line[{{l1,w2},{0.08,w2}}],
		Line[{{l1,-w2},{-0.04,-w2}}],Line[{{l1,-w1},{-0.04,-w1}}]
	}]],
	"Tick"->GraphicsGroup[{
		Thickness[0.12],CapForm["Round"],JoinForm["Round"],
		Line[{{-0.4,-0.04},{-0.12,-0.32},{0.44,0.24}}]
	}],
	"Cross"->GraphicsGroup[{
		Thickness[0.12],CapForm["Round"],JoinForm["Round"],
		Line[{{-0.3,-0.3},{0.3,0.3}}],
		Line[{{-0.3,0.3},{0.3,-0.3}}]
	}],
	"Browse"->GraphicsGroup[{
		Disk[{0,0},0.12],
		Disk[{-0.4,0},0.12],
		Disk[{0.4,0},0.12]
	}],
	"Refresh"->With[{r=0.48},GraphicsGroup[{
		Thickness[0.08],CapForm["Round"],JoinForm["Round"],
		Circle[{0,0},r,{0,7/4Pi}],
		Line[{{r,0},{r-0.16,0.08}}],
		Line[{{r,0},{r+0.08,0.16}}]
	}]]
|>;


(* ::Input:: *)
(*buttonNames=Keys[buttonData];*)
(*buttonNamePaged=Partition[buttonNames,UpTo@Ceiling[Length@buttonNames/Ceiling[Length@buttonNames/9]]];*)
(*Grid[buttonDisplay/@#&/@buttonNamePaged,ItemSize->{6,6},Spacings->{.5,0}]*)
