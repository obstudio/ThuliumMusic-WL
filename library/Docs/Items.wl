(* ::Package:: *)

RenderText[style_String] := Function[RenderText[#1, style]];
RenderText[line_String, style_String] := Block[{output},
	output = StringCases[line, {
		"`"~~text:RegularExpression["([^`\\\\]|\\\\.)+"]~~"`" :>
			TemplateBox[{StyleBox[text, style <> "-code"]}, "CodeBox"],
		"(("~~text:RegularExpression["([^\\)\\\\]|\\\\.)+"]~~"))" :>
			BoxApply[RenderText[text, style], FontSize -> Inherited * 0.7, FontColor -> RGBColor["#555555"]],
		"**"~~text:RegularExpression["([^\\*\\\\]|\\\\.)+"]~~"**" :>
			BoxApply[RenderText[text, style], FontWeight -> Bold],
		"*"~~text:RegularExpression["([^\\*\\\\]|\\\\.)+"]~~"*" :>
			BoxApply[RenderText[text, style], FontSlant -> Italic],
		"--"~~text:RegularExpression["([^\\-\\\\]|\\\\.)+"]~~"--" :>
			BoxApply[RenderText[text, style], FontVariations -> {"StrikeThrough" -> True}],
		"__"~~text:RegularExpression["([^_\\\\]|\\\\.)+"]~~"__" :>
			BoxApply[RenderText[text, style], FontVariations -> {"Underline" -> True}],
		"\\"~~text_ :> StyleBox[text, style],
		text_ :> RenderLanguage[text, style]
	}];
	Return[BoxSimplify @ RowBox @ output];
];


Options[RenderItem] = {FontWeight -> Automatic, Alignment -> Automatic, Background -> None};
RenderItem[spec:OptionsPattern[]] := Function[RenderItem[#1, spec]];
RenderItem[string_String, OptionsPattern[]] := Block[
	{alignment, text},
	If[string == "-", Return["\[SpanFromLeft]"]];
	If[string == "|", Return["\[SpanFromAbove]"]];
	If[string == "+", Return["\[SpanFromBoth]"]];
	If[StringStartsQ[string, "<"|"="|">"],
		text = StringDelete[string, StartOfString~~("<"|"="|">")];
		alignment = Switch[StringPart[string, 1], "<", Left, "=", Center, ">", Right],
		text = string;
		alignment = OptionValue[Alignment]
	];
	Return @ ItemBox[
		BoxApply[RenderText[text, "Table"], FontWeight -> OptionValue[FontWeight]],
		Alignment -> alignment,
		Background -> RGBColor @ OptionValue[Background]
	];
];


RenderCode[code_String] := Block[
	{output},
	output = StyleBox[FormBox["\"" <> code <> "\"", InputForm], "Code"];
	Return @ output;
];
