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
	},ContentSelectable->False]]
];
pageSelectorDisplay[name_String,style_]:=If[style=="Default",
	Mouseover[pageSelectorDisplay[name,"Basic"],pageSelectorDisplay[name,"Mouseover"]],
	Block[{scheme=pageSelectorColor[[style]]},Graphics[{
		squareRounded[0.06,0.3,scheme],
		scheme[["Body"]],
		pageSelectorData[name]
	},ContentSelectable->False]]
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
	With[{scheme=buttonColor[[style]]},Graphics[{
		squareRounded[0.06,1,scheme],
		scheme[["Body"]],name/.buttonData
	},ContentSelectable->False]]
];
