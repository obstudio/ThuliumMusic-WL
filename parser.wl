(* ::Package:: *)

beatCalc[operators_]:=Module[{beats=1,i=1},
	Do[
		Switch[operator,
			"-",beats+=1,
			"_",beats/=2,
			_,beats*=2-2^(-StringLength@operator)
		],
	{operator,StringCases[operators,{"-","_","."..}]}];
	Return[beats];
];
pitchCalc[token_,settings_,previous_]:=Module[{pitches,chordSymbol,pitchDict},
	pitchDict=<|1->0,2->2,3->4,4->5,5->7,6->9,7->11,10->10|>;
	If[KeyExistsQ[token,"Pitches"],
		pitches=Flatten[pitchCalc[#,settings,previous]&/@Association/@token[["Pitches"]]],
		pitches=Switch[token[["ScaleDegree"]],
			-1,previous[[settings[["Trace"]]]],
			0,None,
			_,Key[token[["ScaleDegree"]]]@pitchDict+settings[["Key"]]+12*settings[["Oct"]]
		]
	];
	If[KeyExistsQ[token,"SemitonesCount"],pitches+=token[["SemitonesCount"]]];
	If[KeyExistsQ[token,"OctavesCount"],pitches+=12*token[["OctavesCount"]]];
	If[KeyExistsQ[token,"ChordSymbol"],
		chordSymbol=token[["ChordSymbol"]];
		Switch[chordSymbol,
			"$",pitches+=settings[["Chord"]],
			Except[""],pitches+=chordDict[[chordSymbol]]
		];
	];
	Return[pitches];
];


restTemplate={"ScaleDegree"->0,"SemitonesCount"->0,"OctavesCount"->0,"ChordSymbol"->""};
percTemplate={"ScaleDegree"->10,"SemitonesCount"->0,"OctavesCount"->0,"ChordSymbol"->""};(* of any use? *)


trackParse[tokens_,global_]:=Module[
	{
		i,settings=global,
		functionData,pitches,
		beatCount,duration=0,
		barBeat=0,prevBeat,
		barCount=0,
		
		(* notations *)
		appoggiatura={},
		tuplet=0,tupletRatio,
		tremolo1=0,tremolo2=0,
		tie=False,staccato,
		portamento=False,portRate,
		previous=Array[None&,4],
		
		(* return value *)
		soundData={},lastRepeat={},
		trackDuration=0,messages={}
	},
	Do[
		Switch[token[["Type"]],
			"BarLine",
				If[token[["Volta"]],lastRepeat=soundData];
				If[barBeat!=0,
					barCount++;
					If[barBeat!=settings[["Bar"]],AppendTo[messages,<|
						"Type"->"BarLengthError",
						"Info"->{barCount,settings[["Bar"]],barBeat}
					|>]];
					barBeat=0;
				],
			"FunctionToken",
				functionData=Association@token[["Argument"]];
				Do[
					settings[[function]]=functionData[[function]],
				{function,Keys@functionData}],
			"Tuplet",
				tuplet=token[["NotesCount"]];
				tupletRatio=(2^Floor[Log2[tuplet]])/tuplet,
			"Tremolo1",
				tremolo1=token[["StrokesCount"]],
			"Tremolo2",
				tremolo2=token[["StrokesCount"]];
				trackDuration-=duration,
			"Appoggiatura",
				appoggiatura=pitchCalc[token,settings,previous],
			"Tie",
				tie=True,
			"Portamento",
				portamento=True;
				trackDuration-=duration,
			"Note",
				pitches=pitchCalc[token,settings,previous];
				If[!MemberQ[token[["Pitches"]],restTemplate],
					previous=Prepend[Drop[previous,-1],pitches];
				];
				beatCount=beatCalc[token[["DurationOperators"]]];
				beatCount*=2^(-settings[["Dur"]]);
				If[tuplet>0,beatCount*=tupletRatio;tuplet--];
				barBeat+=beatCount;
				duration=240/settings[["Speed"]]/settings[["Beat"]]*beatCount;
				trackDuration+=duration;
				staccato=token[["Staccato"]];
				If[token[["Arpeggio"]],
					appoggiatura=Flatten/@Array[Take[pitches,#]&,Length@pitches-1]
				];
				Which[
					MemberQ[instrData[["Percussion"]],settings[["Instr",1]]],
						If[pitches==={None},
							AppendTo[soundData,{None,duration}],
							AppendTo[soundData,{True,duration}]
						],
					tie&&pitches==previous[[2]],
						soundData[[-1,2]]+=duration;
						prevBeat+=beatCount;
						tie=False,
					tremolo1!=0,
						duration/=(beatCount*2^tremolo1);
						Do[
							AppendTo[soundData,{pitches,duration}],
						{k,beatCount*2^tremolo1}];
						tremolo1=0,
					tremolo2!=0,
						duration/=(beatCount*2^tremolo2);
						barBeat-=prevBeat;
						soundData=Drop[soundData,-1];
						Do[
							AppendTo[soundData,{previous[[2]],duration}];
							AppendTo[soundData,{pitches,duration}],
						{k,beatCount*2^(tremolo2-1)}];
						tremolo2=0,
					portamento,
						portRate=(pitches[[1]]-previous[[2,1]]+1)/beatCount/settings[["Port"]];
						duration/=(beatCount*settings[["Port"]]);
						barBeat=barBeat-prevBeat;
						soundData=Drop[soundData,-1];
						Do[
							AppendTo[soundData,{Floor[k],duration}],
						{k,previous[[2,1]],pitches[[1]],portRate}];
						portamento=False,
					True,
						If[appoggiatura!={},
							beatCount-=settings[["Appo"]]*Min[4,Length@appoggiatura]/4;
							duration=240*settings[["Appo"]]/settings[["Speed"]]/settings[["Beat"]]/Max[4,Length@appoggiatura];
							Do[
								AppendTo[soundData,{pitch,duration}],
							{pitch,appoggiatura}];
							appoggiatura={};
						];
						duration=240/settings[["Speed"]]/settings[["Beat"]]*beatCount;
						prevBeat=beatCount;
						If[staccato,
							AppendTo[soundData,{pitches,duration*(1-settings[["Stac"]])}];
							AppendTo[soundData,{None,duration*settings[["Stac"]]}];
							staccato=False,
							AppendTo[soundData,{pitches,duration}];
						];
				];
		],
	{token,Association/@tokens}];
	Return[<|
		"RealTracks"->{<|"SoundData"->soundData,"MetaSettings"->settings[[metaSettings]]|>},
		"Duration"->trackDuration,
		"Messages"->messages,
		"LastRepeat"->{{}}
	|>]
];


(* ::Input:: *)
(*QYS`getTrackToken["<Piano><0.6><1=bA,,>2_5o_%%#%|"]*)


(* ::Input:: *)
(*tmp=trackParse[QYS`getTrackToken["<Piano><0.6><1=bA,,>2_5o_%%#%|"],defaultParameter]*)


(* ::Input:: *)
(*AudioStop[];AudioPlay@integrate@tmp[["RealTracks"]];*)


parse[tokenizer_]:=Module[
	{
		audio,settings,
		functionData,
		trackData,
		realTracks={},
		sectionDuration,
		duration=0
	},
	settings=defaultParameter;
	Do[
		sectionDuration=0;
		Do[
			functionData=Association@token[["Argument"]];
			Do[
				settings[[function]]=functionData[[function]],
			{function,Keys@functionData}],
		{token,Association/@sectionToken[["GlobalSettings"]]}];
		Do[
			trackData=trackParse[trackToken,settings];
			realTracks=Join[realTracks,<|
				"SoundData"->Prepend[#SoundData,{None,duration}],
				"MetaSettings"->#MetaSettings
			|>&/@trackData[["RealTracks"]]];
			sectionDuration=Max[sectionDuration,trackData[["Duration"]]],
		{trackToken,sectionToken[["Tracks"]]}];
		duration+=sectionDuration,
	{sectionToken,Association/@Association[tokenizer][["Sections"]]}];
	Return[realTracks];
];


(* ::Input:: *)
(*parse[QYS`Tokenize[path<>"Songs\\Anima.qys"]][[1,"SoundData"]]//Column*)


(* ::Input:: *)
(*AudioStop[];AudioPlay@integrate[parse[QYS`Tokenize[path<>"Songs\\Touhou\\TH11-Chireiden\\Nuclear_Fusion.qys"]]];*)


integrate[tracks_]:=Module[
	{audio=0,settings,instrCount,instrument,soundData,generate},
	Do[
		settings=trackData[["MetaSettings"]];
		instrCount=Length@settings[["Instr"]];
		If[Length@settings[["Volume"]]<instrCount,
			settings[["Volume"]]=Array[settings[["Volume",1]]&,instrCount];
		];
		Do[
			instrument=settings[["Instr",i]];
			soundData=trackData[["SoundData"]];
			If[MemberQ[instrData[["Style"]],instrument],
				generate=SoundNote[#[[1]],#[[2]],instrument]&,
				generate=SoundNote[If[TrueQ@#[[1]],instrument,None],#[[2]]]&
			];
			audio+=settings[["Volume",i]]*AudioFade[
				Sound[generate/@soundData],
			{settings[["FadeIn"]],settings[["FadeOut"]]}],
		{i,instrCount}],
	{trackData,tracks}];
	Return[audio];
];
