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
	pitchDict=<|0->None,1->0,2->2,3->4,4->5,5->7,6->9,7->11,10->10|>;
	If[KeyExistsQ[token,"Pitches"],
		pitches=Flatten[pitchCalc[#,settings,previous]&/@Association/@token[["Pitches"]]],
		pitches=Switch[token[["ScaleDegree"]],
			-1,previous[[settings[["Trace"]]]],
			_,Key[token[["ScaleDegree"]]]@pitchDict
		]
	];
	pitches+=settings[["Key"]]+12*settings[["Oct"]];
	If[KeyExistsQ[token,"SemitonesCount"],pitches+=token[["SemitonesCount"]]];
	If[KeyExistsQ[token,"OctavesCount"],pitches+=token[["OctavesCount"]]];
	If[KeyExistsQ[token,"ChordSymbol"],
		chordSymbol=token[["ChordSymbol"]];
		Switch[chordSymbol,
			"$",pitches+=settings[["Chord"]],
			Except[""],pitches+=chordDict[[chordSymbol]]
		];
	];
	Return[pitches];
];


(* ::Input:: *)
(*pitchCalc[Association[QYS`getTrackToken["00"][[1]]],defaultParameter,Array[None&,4]]*)


restTemplate={"ScaleDegree"->0,"SemitonesCount"->0,"OctavesCount"->0,"ChordSymbol"->""};
percTemplate={"ScaleDegree"->10,"SemitonesCount"->0,"OctavesCount"->0,"ChordSymbol"->""};(* of any use? *)


trackParse[tokens_,global_]:=Module[
	{
		i,k,settings=global,
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
							AppendTo[soundData,{False,duration}],
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
						portRate=(pitches-previous[[2]]+1)/beatCount/settings[["Port"]];
						duration/=(beatCount*settings[["Port"]]);
						barBeat=barBeat-prevBeat;
						soundData=Drop[soundData,-1];
						For[k=previous[[2,1]],k<pitches[[1]],k+=portRate,
							AppendTo[soundData,{Floor[k],duration}]
						];
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
		"SoundData"->soundData,
		"Duration"->trackDuration,
		"Messages"->messages,
		"LastRepeat"->lastRepeat,
		"MetaSettings"->settings[[metaSettings]]
	|>]
];


(* ::Input:: *)
(*trackParse[Association/@QYS`getTrackToken["<BassDrum,Shaker(0.3)>xx_0_x"],defaultParameter]*)


(* ::Input:: *)
(*trackParse[Association/@QYS`getTrackToken["<60>10[%#5]-"],defaultParameter]*)


(* ::Input:: *)
(*AudioStop[];AudioPlay@integrate@{trackParse[Association/@QYS`getTrackToken["<BassDrum,Shaker(0.3)><160>xx_xx_x|xx_xx_x|"],defaultParameter]};*)


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
				generate=SoundNote[If[#[[1]],instrument,None],#[[2]]]&
			];
			audio+=settings[["Volume",i]]*AudioFade[Sound[
				generate/@soundData
			],{settings[["FadeIn"]],settings[["FadeOut"]]}],
		{i,instrCount}],
	{trackData,tracks}];
	Return[audio];
];


SoundNote[#[[1]],#[[2]],"Piano"]&@{1,2}


parse[tokenizer_]:=Module[
	{
		audio,settings,
		functionData,
		trackData
	},
	settings=defaultParameter;
	Do[
		Do[
			functionData=Association@token[["Argument"]];
			Do[
				settings[[function]]=functionData[[function]],
			{function,Keys@functionData}],
		{token,Association/@sectionToken[["GlobalSettings"]]}];
		Do[
			trackData=trackParse[trackToken,settings];
			Print[trackData],
		{trackToken,sectionToken[["Tracks"]]}];
		,
	{sectionToken,Association/@Association[tokenizer][["Sections"]]}];
	Return[];
];


(* ::Input:: *)
(*parse[QYSTokenize[path<>"Songs\\test.qys"]];*)
