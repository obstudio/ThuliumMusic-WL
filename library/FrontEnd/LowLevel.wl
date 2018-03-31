(* ::Package:: *)

$GeneratedList = {};
DingBatList = {"\[FilledDiamond]", "\[EmptyDiamond]", "\[FilledCircle]", "\[EmptyCircle]", "\[FilledSquare]", "\[EmptySquare]", "\[FilledUpTriangle]", "\[EmptyUpTriangle]"};


ListSow[list_List, tag_String] := Scan[Sow[#, tag]&, list];


ExternSheet[sheet_String] := Sequence @@ First @ StyleSheet[sheet];


(* some functions *)
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
	CellFrameStyle -> OptionValue[FrameStyle],
	CellFrameMargins -> 0, 
	CellTags -> OptionValue[CellTags],
	Background -> Inherited,
	ShowCellBracket -> False,
	Selectable -> False,
	Deployed -> True
];


(* ::Input:: *)
(*CellPrint@SpacerCell[{60,0},2,FrameStyle->Dashing[2StringToDashing["-- -- --"]]]*)


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


textLength[str_String] := 2 StringLength[str] - StringCount[str, Alternatives @ CharacterRange[32, 127]];
timeDisplay[time_Quantity, levelspec_Integer:2] := With[
	{sec = Floor[QuantityMagnitude[UnitConvert[time, "Seconds"]]]},
	StringRiffle[{
		If[StringLength[#] == 1, "0" <> #, #]&[IntegerString[Floor[sec / (60 ^ (levelspec - 1))], 10]],
		Sequence @@ Table[
			IntegerString[Floor[Mod[sec / (60 ^ (level - 1)), 60]], 10, 2],
		{level, levelspec - 1, 1, -1}]
	}, ":"]
];
