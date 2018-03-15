(* ::Package:: *)

If[!ValueQ[Rate],
	Unprotect[Rate];
	Rate = "Rate";
	Protect[Rate];
];


Options[Adapter] = {"Rate" -> 1, Format -> "Audio"};
Adapter[filename_, OptionsPattern[]] := 
With[{rawData = Parse[localPath <> "Songs/" <> filename]},
Interpretation[
	Deploy @ Tooltip[
		Framed[
			Pane[
				Style[FileNameTake[filename], "AdapterSong"],
				ImageMargins -> {{1, 1}, {0, 0}}
			],
			Background -> RGBColor[0.9, 0.96, 1, 0.5],
			FrameStyle -> None,
			RoundingRadius -> 8
		],
		Pane[Grid[
			{
				{"Music:", filename},
				{"Format:", OptionValue[Format]},
				{"Rate:", OptionValue[Rate]}
			},
			Alignment -> {Left, Center},
			Spacings -> {0.5, 0.5},
			ItemStyle -> {{"AdapterTag", "AdapterMeta"}},
			BaselinePosition -> 0
		], ImageMargins -> {{8, 8}, {8, 8}}],
		TooltipStyle -> {
			Background -> RGBColor[0.9, 0.93, 0.95, 0.9],
			CellFrame -> 1,
			CellFrameColor -> RGBColor[0.76, 0.84, 0.88, 0.5]
		}
	],
	Switch[OptionValue[Format],
		"MIDI", MIDIAdapt[rawData],
		"Audio", AudioAdapt[rawData],
		_, Message[Adapt::format,OptionValue[Format]]; Return[];
	]
]];


WorkBenchTemplate = Hold[{
	Cell[BoxData @ MakeBoxes[
		MusicStop[];
	], "Input"],
	Cell[BoxData @ MakeBoxes[
		Adapter["Touhou/TH11-Chireiden/3rd_Eye.tm", Format -> "MIDI"]
	], "Input"]
}];
