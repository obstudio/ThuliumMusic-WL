(* ::Package:: *)

(* Box Tools *)
BoxApply[StyleBox[boxspec__], options___] := StyleBox[boxspec, options];
BoxApply[RowBox[boxes_List], options___] := RowBox[BoxApply[#, options]& /@ boxes];
BoxSimplify[StyleBox[box_StyleBox, options___]] := Insert[box, Unevaluated[options], 2];
BoxSimplify[StyleBox[box_RowBox, options___]] := StyleBox[BoxSimplify[box], options];
BoxSimplify[RowBox[boxes_List]] := BoxSimplify[boxes];
BoxSimplify[boxes_List] := If[Length[boxes] == 1, boxes[[1]],
	RowBox @ ReplaceRepeated[boxes, {
		{pre___, PatternSequence[
			StyleBox[str1_String, options___],
			StyleBox[str2_String, options___]
		], post___} :> {pre, StyleBox[str1 <> str2, options], post},
		RowBox[boxes1_List] :> BoxSimplify[boxes1]
	}]
];

(* Spacer Cell *)
Options[SpacerCell] = {
	CellFrameColor -> Automatic,
	Background -> Inherited
};
SpacerCell[t_Integer, op:OptionsPattern[]] := SpacerCell[{0, 0}, t, op];
SpacerCell[{w_Integer:0, h_Integer}, t_Integer:0, OptionsPattern[]] := Cell["", "Text",
	CellSize -> {Inherited, 1},
	CellMargins -> {{w, w}, {h, h}},
	CellElementSpacings -> {"CellMinHeight" -> 1},
	CellFrame -> If[t >= 0, {{0, 0}, {0, t}}, {{0, 0}, {-t, 0}}],
	CellFrameColor -> OptionValue[CellFrameColor],
	CellFrameMargins -> 0, 
	Background -> OptionValue[Background],
	Selectable -> False
];

DingBatList = {"\[FilledDiamond]", "\[EmptyDiamond]", "\[FilledCircle]", "\[EmptyCircle]", "\[FilledSquare]", "\[EmptySquare]", "\[FilledUpTriangle]", "\[EmptyUpTriangle]"};




(* some functions *)
textLength[string_]:=2StringLength[string]-StringCount[string,Alternatives@CharacterRange[32,127]];
timeDisplay[time_]:=Block[
	{sec=Floor[QuantityMagnitude[UnitConvert[time,"Seconds"]]]},
	IntegerString[Floor[sec/60],10,2]<>":"<>IntegerString[Mod[sec,60],10,2]
];
completeText[raw_,arg_]:=StringReplace[raw,{
	"&"~~i:DigitCharacter:>ToString[arg[[ToExpression@i]],FormatType->InputForm],
	"$"~~i:DigitCharacter:>"\""<>arg[[ToExpression@i]]<>"\"",
	"#"~~i:DigitCharacter:>StringRiffle[ToString[#,FormatType->InputForm]&/@arg[[ToExpression@i]],", "]
}];
caption[string_String]:=caption[string,"None",{}];
caption[string_String,argument_List]:=caption[string,"None",argument];
caption[string_String,style_String]:=caption[string,style,{}];
caption[string_String,style_String,argument_List]:=Style[completeText[Which[
	StringLength@string>0&&StringPart[string,1]=="_",text[[StringDrop[string,1]]],
	True,string
],argument],styleDict[[style]]];
