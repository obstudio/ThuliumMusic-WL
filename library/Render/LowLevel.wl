(* ::Package:: *)

$GeneratedList = {};
DingBatList = {"\[FilledDiamond]", "\[EmptyDiamond]", "\[FilledCircle]", "\[EmptyCircle]", "\[FilledSquare]", "\[EmptySquare]", "\[FilledUpTriangle]", "\[EmptyUpTriangle]"};


ListSow[list_List, tag_String] := Scan[Sow[#, tag]&, list];


ExternSheet[sheet_String] := Sequence @@ First @ StyleSheet[sheet];


RenderLanguage[text_String, style_String] := RowBox @ StringCases[text, {
	text$$__?(First @ ToCharacterCode[#, "Unicode"] < 8000&) :> StyleBox[text$$, style],
	text$$__?(First @ ToCharacterCode[#, "Unicode"] >= 8000&) :> StyleBox[text$$, style <> "-chs"]
}];


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


TemplateCell[name_String, style_String, args_List] := Cell[StyleData[name],
	TemplateBoxOptions -> {DisplayFunction -> Function[TemplateBox[args, style]]}
];

SpacerBox[arg_] := MakeBoxes @ Spacer[arg];
SpacerBox[arg_, args__] := MakeBoxes @ Spacer[{arg, args}];

(* Spacer Cell *)
Options[SpacerCell] = {FrameStyle -> Automatic, CellTags -> None};
SpacerCell[t_Integer, op:OptionsPattern[]] := SpacerCell[{0, {0, 0}}, t, op];
SpacerCell[{w_Integer:0, top_Integer}, t_Integer:0, op:OptionsPattern[]] := SpacerCell[{w, {0, top}}, t, op];
SpacerCell[{w_Integer:0, {bottom_Integer, top_Integer}}, t_Integer:0, OptionsPattern[]] := Cell["", "Text",
	FontSize -> 1,
	FontColor -> RGBColor[0, 0, 0, 0],
	CellSize -> {Inherited, 1},
	CellMargins -> {{w, w}, {bottom, top}},
	CellElementSpacings -> {"CellMinHeight" -> 1},
	CellFrame -> If[t >= 0, {{0, 0}, {0, t}}, {{0, 0}, {-t, 0}}],
	System`CellFrameStyle -> OptionValue[FrameStyle],
	CellFrameMargins -> 0, 
	CellTags -> OptionValue[CellTags],
	Background -> Inherited,
	ShowCellBracket -> False,
	Selectable -> False,
	Deployed -> True
];


(* ::Input:: *)
(*CellPrint@SpacerCell[{60,0},2,FrameStyle->Directive[Red,Dashing[2StringToDashing["-- -- --"]]]]*)


StringToDashing[str_] := With[
	{
		dash = StringLength @ First @ StringCases[str, ("-"|"=")..],
		sep = StringLength @ First @ StringCases[str, ("."|" ")..]
	},
	Switch[str,
		_?(StringContainsQ[" "]),
			Return @ {dash * 6, sep * 6},
		_?(StringContainsQ["."]),
			Return @ {dash * 6, Sequence @@ ConstantArray[2, 2 * sep + 1]},
		_,
			Return @ {}
	];
];
