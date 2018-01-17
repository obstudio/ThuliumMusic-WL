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


pitchDict=<|0->None,1->0,2->2,3->4,4->5,5->7,6->9,7->11|>;
pitchCalc[token_,settings_,previous_]:=Module[{pitches,chordSymbol},
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
(*pitchCalc[Association[QYSTrackTokenize["3"][[1]]],defaultParameter,Array[None&,4]]*)


trackParse[tokens_,global_]:=Module[
	{
		i,settings=global,
		functionData,
		pitches,percussion,
		beatCount,duration,
		barBeat,
		
		(* notations *)
		appoggiatura={},
		tuplet=0,tupletTime,
		previous=Array[None&,4],
		
		(* return value *)
		soundData={},trackDuration=0,
		messages={},instruments={}
	},
	Do[
		Switch[token[["Type"]],
			"FunctionToken",                           (* function *)
				functionData=Association@token[["Argument"]];
				Do[
					settings[[function]]=functionData[[function]],
				{function,Keys@functionData}],
			"Appoggiatura",                            (* appoggiatura *)
				appoggiatura=pitchCalc[token,settings,previous],
			"Note",                                    (* note *)
				If[token,
					pitches=pitchCalc[token,settings,previous];
					previous=Prepend[Drop[previous,-1],pitches];
				];
				beatCount=beatCalc[token[["DurationOperators"]]];
				beatCount*=2^(-settings[["Dur"]]);
				If[tuplet>0,beatCount*=tupletTime;tuplet--];
				barBeat+=beatCount;
				duration=240/settings[["Speed"]]/settings[["Beat"]]*beatCount;
				trackDuration+=duration;
		],
	{token,Association/@tokens}];
	Return[<|
		"SoundData"->soundData,
		"Duration"->duration,
		"Messages"->messages,
		"Instruments"->instruments
	|>]
];


(* ::Input:: *)
(*trackParse[Association/@QYSTrackTokenize["13%#-"],defaultParameter]*)


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



