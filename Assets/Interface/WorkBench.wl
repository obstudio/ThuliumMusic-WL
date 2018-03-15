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


InstrTest[p___]:=EmitSound@Sound@SoundNote[p];


WorkBenchTemplate = Hold[{
	Cell[BoxData @ MakeBoxes[
		MusicStop[];
	], "Input"],
	Cell[BoxData @ MakeBoxes[
		Diagnose @ Parse[localPath<>"Songs/Touhou/TH11-Chireiden/3rd_Eye.tm"]
	], "Input"],
	Cell[BoxData @ MakeBoxes[
		InstrTest[5, 1, "ElectricBass"]
	], "Input"],
	Cell[BoxData @ MakeBoxes[
		InstrTest["BassDrum", 1]
	], "Input"],
	Cell[BoxData @ MakeBoxes[
		MIDIPlay @ MIDIAdapt[Parse[localPath <> "Songs/Touhou/TH11-Chireiden/3rd_Eye.tm"], Rate -> 1.1]
	], "Input"],
	Cell[BoxData @ MakeBoxes[
		AudioPlay @ AudioAdapt[Parse[localPath <> "Songs/Touhou/TH11-Chireiden/3rd_Eye.tm"], Rate -> 1.1]
	], "Input"]
}];


SaveWorkBench := Block[{benchData},
	benchData = NotebookRead @ Cells[CellStyle -> {"Input", "Music", "Text"}];
	Export[userPath <> "WorkBench.nb", benchData];
];


LoadWorkBench := Block[{benchData},
	NotebookDelete[Cells[CellStyle -> {"Input", "Output", "Music", "Text"}]];
	benchData = First @ Import[userPath <> "WorkBench.nb", "Notebook"];
	SelectionMove[First @ Cells[CellTags -> "$init"], After, Cell, AutoScroll -> False];
	NotebookWrite[EvaluationNotebook[], benchData];
];
