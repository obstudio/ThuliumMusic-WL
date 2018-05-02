(* ::Package:: *)

BeginPackage["graphics`"];

SVGPathD::usage = "SVGPathD parses a svg path command.";
CurveMerge::usage = "CurveMerge merges a Bezier curve from segments.";
VertexAssign::usage = "VertexAssign assigns vertices with a coordinate.";
progressBar::usage = "Draw a progress bar.";
progressBlock::usage = "Draw a progress block.";
progressLocate::usage = "Location to progression";
squareRounded::usage = "Draw a rounded square.";

Begin["`Private`"];

SVGPathD[string_]:=Block[
	{
		commands,
		prev={0,0},this,ctrl,
		init={0,0},move,
		segments={},points={{0,0}},
		components={Line[{{0,0}}]}
	},
	commands=StringCases[string,
		cmd:LetterCharacter~~n:(NumberString|","|WhitespaceCharacter)...:><|
			"Name"->cmd,
			"Args"->ToExpression/@StringCases[n,NumberString]
		|>
	];
	Do[
		move=If[UpperCaseQ@command[["Name"]],Set,AddTo];
		Switch[ToUpperCase@command[["Name"]],
			"M",
				move[this,command[["Args"]]];
				init=this;
				If[!MatchQ[components,{Line[{_}]}],
					AppendTo[segments,<|
						"Points"->points,
						"Segment"->components
					|>];
				];
				points={};components={},
			"H",
				move[this,{command[["Args",1]],0}],
			"V",
				move[this,{0,command[["Args",1]]}],
			"Z",
				this=init,
			"L",
				move[this,command[["Args"]]],
			"C",
				move[this,command[["Args",5;;6]]];
				ctrl=If[UpperCaseQ@command[["Name"]],0,prev]+#&/@Partition[command[["Args"]],2],
			_,
				Echo@command[["Name"]];
		];
		If[MemberQ[Characters["MmZzLlHhVv"],command[["Name"]]],
			AppendTo[points,this];
			AppendTo[components,Line[{this}]],
			points=Join[points,ctrl];
			AppendTo[components,BezierCurve[ctrl]];
		];
		prev=this,
	{command,commands}];
	If[!MatchQ[components,{Line[{_}]}],
		AppendTo[segments,<|
			"Points"->points,
			"Segment"->components
		|>];
	];
	Return[segments];
];

CurveMerge[segment_]:=Block[
	{pts={Level[segment[[1]],1]}},
	Do[
		Switch[Head[component],
			Line,
				AppendTo[pts,ConstantArray[Level[component,{2}],3]],
			BezierCurve,
				AppendTo[pts,Level[component,1]]
		],
	{component,Drop[segment,1]}];
	Return[Flatten[pts,2]];
];

VertexAssign[vertices_,{pt1x_,pt1y_}->pos1_,{pt2x_,pt2y_}->pos2_]:={
	((pos1-pos2)*((pt1x-pt2x)*#[[1]]+(pt1y-pt2y)*#[[2]])
	 +pos1*(-pt1x*pt2x+pt2x^2-pt1y*pt2y+pt2y^2)
	 +pos2*(pt1x^2-pt1x*pt2x+pt1y^2-pt1y*pt2y)
	)/((pt1x-pt2x)^2+(pt1y-pt2y)^2),
0}&/@vertices;

progressBarShape[l_,r_,t_]:={
	{l,1},{l-t,1},{l-1,t},{l-1,0},
	{l-1,-t},{l-t,-1},{l,-1},{l,-1},
	{r,-1},{r,-1},{r+t,-1},{r+1,-t},
	{r+1,0},{r+1,t},{r+t,1},{r,1}
};

progressBar[pro_,len_]:=With[
	{	
		content=progressBarShape[-len,len*(2pro-1),3/5],
		profile=progressBarShape[-len,len,3/5]
	},
	GraphicsGroup[{
		RGBColor["#D0D0F0"],Thickness[1/len/4],
		BezierCurve[profile],Line[{{-len,1},{len,1}}],
		RGBColor["#F0F0FF"],FilledCurve[{BezierCurve[profile]}],
		Texture[Table[{c,1-c,1},{c,0,1,1/256}]],
		FilledCurve[{BezierCurve[content]},
			VertexTextureCoordinates->VertexAssign[content,{-len,0}->1/5,{len,0}->4/5]
		]
	}]
];

progressLocate[posx_,len_]:=Piecewise[
	{{0,posx<-len},{1,posx>len||!NumberQ[posx]}},
	(posx/len+1)/2
];

progressBlock[pro_,len_]:=GraphicsGroup[{
	Directive[RGBColor["#F0F0FB"],Thickness[0.35/len]],
	Circle[{len*(2pro-1),0},1.05],
	Directive[RGBColor["#D0D0E0"],Thickness[0.1/len]],
	Circle[{len*(2pro-1),0},1.4],
	Circle[{len*(2pro-1),0},0.7]
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

End[];

EndPackage[];



DeclarePackage["graphics`",{
	"SVGPathD",
	"CurveMerge",
	"VertexAssign",
	"progressBar",
	"progressBlock",
	"progressLocate",
	"squareRounded"
}]
