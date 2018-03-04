(* ::Package:: *)

Import[localPath<>"package\\Standard\\tutorial\\GraceNote.smd","List"]


Options[RenderText]={
	"Spacer"->0,
	"Font"->{FontSize->20}
};
TextOptions=Keys@Options[RenderText];
RenderText[line_String,OptionsPattern[]]:=Block[{output},
	output=StringCases[line,{
		"("~~text:Except["("|")"]..~~")":>RowBox[
			Prepend[StyleBox["(",OptionValue["Font"]]]@
			Append[StyleBox[")",OptionValue["Font"]]]@
			RenderText[text,#->OptionValue[#]&/@TextOptions]
		],
		"*"~~text:Except["*"]..~~"*":>StyleBox[text,"TI",OptionValue["Font"]],
		"\n":>Sequence["\n",TemplateBox[{OptionValue["Spacer"]},"Spacer1"]],
		text:Except[Characters["*()\n"]]..:>StyleBox[text,OptionValue["Font"]]
	}];
	Return[output];
];


TutorialOptions={
	ShowCellBracket->False,
	Editable->False
};


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
(*"(*subtrack*^)*note*\nis a simplified form of GraceNote()."*)
(*}*)
(*|>]];*)


FrameBox[StyleBox["subtrack","TI"],BaseStyle->{Background->RGBColor["#DDDDFF"]},RoundingRadius->{6,6}
,FrameStyle->Directive[RGBColor["#111133"],Dashing[4],Thickness[1]]]//DisplayForm


Options[BaseStyle]
