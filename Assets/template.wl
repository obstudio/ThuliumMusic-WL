(* ::Package:: *)

Import[localPath<>"package\\Standard\\tutorial\\GraceNote.smd","List"]


docUsage[usage_,effect_]:=Block[
	{funcName,ArgList},
	Cell[Column[{
		RowBox[RenderText[usage]],
		RowBox[RenderText[effect]]
	}],CellMargins->{{0,0},{0,0}},
	ShowCellBracket->False,Editable->False,
	Background->LightBlue,CellFrame->{{0,0},{2,2}}]
];


TutorialOptions={
	ShowCellBracket->False,
	Editable->False
};


Options[RenderText]={
	"Spacer"->0,
	"Font"->{FontSize->20}
};
RenderTextOptions=Keys@Options[RenderText];
RenderText[line_String,OptionsPattern[]]:=Block[{output},
	output=StringCases[line,{
		"("~~text:Except["("|")"]..~~")":>RowBox[
			Prepend[StyleBox["(",OptionValue["Font"]]]@
			Append[StyleBox[")",OptionValue["Font"]]]@
			RenderText[text,#->OptionValue[#]&/@RenderTextOptions]
		],
		"*"~~text:Except["*"]..~~"*":>StyleBox[text,"TI",OptionValue["Font"]],
		"\n":>Sequence["\n",TemplateBox[{OptionValue["Spacer"]},"Spacer1"]],
		text:Except[Characters["*()\n"]]..:>StyleBox[text,OptionValue["Font"]]
	}];
	Return[output];
];


RenderTutorial[data_]:=Block[
	{output},
	output={
		Cell[data["Title"],
			CellMargins->{{40,22},{20,40}},
			FontFamily->"Arial",
			FontSize->37,
			FontWeight->"Bold",
			FontColor->GrayLevel[0.2],
			TutorialOptions
		],
		Cell[CellGroupData[
			Cell[
				StyleBox[RowBox[
					Prepend[TemplateBox[{20},"Spacer1"]]@
					RenderText[#,"Spacer"->40,"Font"->{FontSize->20,FontFamily->"Cambria"}]
				]],
				CellMargins->{{0,0},{0,0}},
				CellFrame->{{0,0},{1,1}},
				CellFrameColor->RGBColor["#77BBFF"],
				Background->RGBColor["#DDEEFF"],
				TutorialOptions
			]&/@data["Usage"]]
		]
	};
	Return[output];
];


(* ::Input:: *)
(*CreateDocument[RenderTutorial[<|*)
(*"Title"->"GraceNote",*)
(*"Usage"->{*)
(*"GraceNote(*subtrack*,*note*)\nprepend *subtrack* to *note* as gracenote.",*)
(*"GraceNote(*subtrack*,*note*)\nprepend *subtrack* to *note* as gracenote."*)
(*}*)
(*|>]];*)
