(* ::Package:: *)

(* temporary function *)
QYMParse[filename_]:=integrate[#MusicClips,#Effects]&@parse[QYM`tokenizer[filename]];
QYSParse[filename_]:=integrate[#MusicClips,#Effects]&@parse[QYS`Tokenize[filename]];


pitchDict=<|1->0,2->2,3->4,4->5,5->7,6->9,7->11,10->10|>;
chordDict=<|
	"M"->{0,4,7},"m"->{0,3,7},"a"->{0,4,8},"d"->{0,3,6},
	"t"->{0,3},"T"->{0,4},"q"->{0,5},"Q"->{0,6},
	"p"->{0,7},"P"->{0,8},"h"->{0,8},"H"->{0,9}
|>;
chordOpDict=<|
	"o"->(Join[#,#[[{1}]]+12]&),"u"->(Join[#[[{-1}]]-12,#]&),
	"i"->(Join[#[[;;1]]+12,#[[2;;]]]&),
	"j"->(Join[#[[;;2]]+12,#[[3;;]]]&),
	"k"->(Join[#[[;;3]]+12,#[[4;;]]]&)
|>;
defaultSettings=<|
	"Volume"->1,"Speed"->90,"Key"->0,"Beat"->4,"Bar"->4,"Instr"->{"Piano"},
	"Dur"->0,"FadeIn"->0,"FadeOut"->0,"Stac"->1/2,"Appo"->1/4,"Oct"->0,
	"Port"->6,"Spac"->0,"Chord"->{0,12},"Trace"->1,"InstProp"->{1}
|>;
functionList=Keys@defaultSettings;
metaSettingTag={"Instr","InstProp","FadeIn","FadeOut"};
effectSettingTag={"FadeIn","FadeOut"};


beatCalc[operators_]:=Module[{beats=1,i=1},
	Do[
		Switch[operator,
			"-",beats+=1,
			"_",beats/=2,
			"=",beats/=4,
			_,beats*=2-2^(-StringLength@operator)
		],
	{operator,StringCases[operators,{"-","_","=","."..}]}];
	Return[beats];
];
pitchCalc[token_,settings_,previous_]:=Module[{pitches,chord,chordOp},
	If[KeyExistsQ[token,"Pitches"],
		pitches=Flatten[pitchCalc[#,settings,previous]&/@Association/@token[["Pitches"]]],
		pitches=Switch[token[["ScaleDegree"]],
			0,None,
			-1,previous[[settings[["Trace"]]]],
			_,Key[token[["ScaleDegree"]]]@pitchDict+settings[["Key"]]+12*settings[["Oct"]]
		]
	];
	If[KeyExistsQ[token,"SemitonesCount"],pitches+=token[["SemitonesCount"]]];
	If[KeyExistsQ[token,"OctavesCount"],pitches+=12*token[["OctavesCount"]]];
	If[KeyExistsQ[token,"ChordSymbol"],
		chord=Switch[token[["ChordSymbol"]],
			"$",settings[["Chord"]],
			"",{0},
			_,chordDict[[token[["ChordSymbol"]]]]
		];
		pitches+=If[token[["ChordOperator"]]=="",
			If[chord=={0},0,chord],
			chordOpDict[token[["ChordOperator"]]]@chord
		];
	];
	Return[pitches];
];


trackParse[tokenizer_,global_]:=Module[
	{
		(* basic variables *)
		tokens=Association/@tokenizer[["Contents"]],
		repeat=tokenizer[["Repeat"]],
		settings=global,
		
		(* notations *)
		functionData,pitches,
		beatCount,duration=0,
		barBeat=0,prevBeat,
		graceNote={},appoggiatura,
		tuplet=0,tupletRatio,
		tremolo1=0,tremolo2=0,
		tie=False,staccato,
		portamento=False,portRate,
		previous=Array[None&,4],
		
		(* repeat and subtrack *)
		voltaData,voltaSettings,
		voltaDefault,volta={},
		master,lastRepeat=<||>,trackData,
		
		(* return value *)
		MusicClips,soundData={},
		trackDuration,durCount=0,
		messages={},barCount=0
	},
	MusicClips=<|"Main"->Null,"Accent"->Nothing,"Subtracks"->{}|>;
	If[repeat>0,
		voltaData=ConstantArray[<||>,repeat];
		voltaDefault=Complement[Range@repeat,Union@@Cases[tokens,
			v:<|"Type"->"Volta",__|>:>v[["Order"]]
		]]
	];
	Do[
		Switch[token[["Type"]],
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
				durCount-=duration,
			"GraceNote",
				graceNote=pitchCalc[token,settings,previous],
			"Appoggiatura",
				appoggiatura=pitchCalc[token,settings,previous];
				soundData[[-1,2]]-=240*settings[["Appo"]]/settings[["Speed"]]/settings[["Beat"]]*Min[4,Length@appoggiatura]/4;
				duration=240*settings[["Appo"]]/settings[["Speed"]]/settings[["Beat"]]/Max[4,Length@appoggiatura];
				Do[
					AppendTo[soundData,{pitch,duration,settings[["Volume"]]}],
				{pitch,appoggiatura}],
			"Tie",
				tie=True,
			"Portamento",
				portamento=True;
				durCount-=duration,
			"Note",
				pitches=pitchCalc[token,settings,previous];
				If[!pitches==={None},previous=Prepend[Drop[previous,-1],pitches]];
				beatCount=beatCalc[token[["DurationOperators"]]];
				beatCount*=2^(-settings[["Dur"]]);
				If[tuplet>0,beatCount*=tupletRatio;tuplet--];
				barBeat+=beatCount;
				duration=240/settings[["Speed"]]/settings[["Beat"]]*beatCount;
				durCount+=duration;
				staccato=token[["Staccato"]];
				If[token[["Arpeggio"]],
					graceNote=Flatten/@Array[Take[pitches,#]&,Length@pitches-1]
				];
				Which[
					MemberQ[instrData[["Percussion"]],settings[["Instr",1]]],
						If[pitches==={None},
							AppendTo[soundData,{None,duration,settings[["Volume"]]}],
							AppendTo[soundData,{True,duration,settings[["Volume"]]}]
						],
					tie&&pitches==previous[[2]],
						soundData[[-1,2]]+=duration;
						prevBeat+=beatCount,
					tremolo1!=0,
						duration/=(beatCount*2^tremolo1);
						Do[
							AppendTo[soundData,{pitches,duration,settings[["Volume"]]}],
						{k,beatCount*2^tremolo1}];
						tremolo1=0,
					tremolo2!=0,
						duration/=(beatCount*2^tremolo2);
						barBeat-=prevBeat;
						soundData=Drop[soundData,-1];
						Do[
							AppendTo[soundData,{previous[[2]],duration,settings[["Volume"]]}];
							AppendTo[soundData,{pitches,duration,settings[["Volume"]]}],
						{k,beatCount*2^(tremolo2-1)}];
						tremolo2=0,
					portamento,
						portRate=(pitches[[1]]-previous[[2,1]]+1)/beatCount/settings[["Port"]];
						duration/=(beatCount*settings[["Port"]]);
						barBeat=barBeat-prevBeat;
						soundData=Drop[soundData,-1];
						Do[
							AppendTo[soundData,{Floor[k],duration,settings[["Volume"]]}],
						{k,previous[[2,1]],pitches[[1]],portRate}];
						portamento=False,
					True,
						If[graceNote!={},
							beatCount-=settings[["Appo"]]*Min[4,Length@graceNote]/4;
							duration=240*settings[["Appo"]]/settings[["Speed"]]/settings[["Beat"]]/Max[4,Length@graceNote];
							Do[
								AppendTo[soundData,{pitch,duration,settings[["Volume"]]}],
							{pitch,graceNote}];
							graceNote={};
						];
						duration=240/settings[["Speed"]]/settings[["Beat"]]*beatCount;
						prevBeat=beatCount;
						If[staccato,
							AppendTo[soundData,{pitches,duration*(1-settings[["Stac"]]),settings[["Volume"]]}];
							AppendTo[soundData,{None,duration*settings[["Stac"]],settings[["Volume"]]}];
							staccato=False,
							AppendTo[soundData,{pitches,duration,settings[["Volume"]]}];
						];
				];
				tie=False,
			"BarLine",
				If[token[["Skip"]],lastRepeat=<|"SoundData"->soundData,"Duration"->durCount|>];
				If[token[["Order"]]!={0},
					If[volta=={},
						voltaSettings=settings;
						master=<|"SoundData"->soundData,"Duration"->durCount|>,
						voltaData[[volta]]=<|"SoundData"->soundData,"Duration"->durCount|>;
					];
					volta=If[#Order=={},voltaDefault,#Order]&[token];
					settings=voltaSettings;
					soundData={};durCount=0
				];
				If[barBeat!=0,
					barCount++;
					If[barBeat!=settings[["Bar"]],AppendTo[messages,<|
						"Type"->"BarLengthError",
						"Bar"->barCount,
						"Info"->{settings[["Bar"]],barBeat}
					|>]];
					barBeat=0;
				],
			"Track",
				trackData=trackParse[token,settings];
				Do[
					If[musicClip[["MetaSettings"]]==settings[[metaSettingTag]],
						soundData=Join[soundData,musicClip[["SoundData"]]],
						soundData=AppendTo[soundData,{None,trackData[["Duration"]],settings[["Volume"]]}];
						AppendTo[MusicClips[["Subtracks"]],<|
							"SoundData"->musicClip[["SoundData"]],
							"Beginning"->musicClip[["Beginning"]]+durCount,
							"Duration"->musicClip[["Duration"]],
							"MetaSettings"->musicClip[["MetaSettings"]]
						|>]
					],
				{musicClip,trackData[["MusicClips"]]}];
				messages=Join[messages,<|
					"Type"->#Type,
					"Bar"->#Bar+barCount,
					"Info"->#Info
				|>&/@trackData[["Messages"]]];
				durCount+=trackData[["Duration"]];
				barCount+=trackData[["BarCount"]];
		],
	{token,Association/@tokens}];
	If[barBeat!=0,
		barCount++;
		If[barBeat!=settings[["Bar"]],AppendTo[messages,<|
			"Type"->"BarLengthError",
			"Bar"->barCount,
			"Info"->{settings[["Bar"]],barBeat}
		|>]];
		barBeat=0;
	];
	
	(* build main music clip *)
	Switch[repeat,
		_?Positive,
			voltaData[[volta]]=<|"SoundData"->soundData,"Duration"->durCount|>;
			trackDuration=repeat*master[["Duration"]]+Total[#Duration&/@voltaData];
			MusicClips[["Main"]]=<|
				"SoundData"->Flatten[Join[master[["SoundData"]],#SoundData]&/@voltaData,1],
				"Beginning"->0,"Duration"->trackDuration,"MetaSettings"->settings[[metaSettingTag]]
			|>,
		_?Negative,
			If[lastRepeat==<||>,lastRepeat=<|"SoundData"->soundData,"Duration"->durCount|>];
			trackDuration=(-repeat-1)*durCount+lastRepeat[["Duration"]];
			MusicClips[["Main"]]=<|
				"SoundData"->Flatten[Append[ConstantArray[soundData,(-repeat-1)],lastRepeat[["SoundData"]]],1],
				"Beginning"->0,"Duration"->trackDuration,"MetaSettings"->settings[[metaSettingTag]]
			|>,
		_,
			trackDuration=durCount;
			MusicClips[["Main"]]=<|
				"SoundData"->soundData,"Beginning"->0,
				"Duration"->trackDuration,"MetaSettings"->settings[[metaSettingTag]]
			|>
	];
	Return[<|
		"MusicClips"->Join[{#Main,#Accent},#Subtracks]&[MusicClips],
		"Duration"->trackDuration,
		"BarCount"->barCount,
		"Messages"->messages
	|>]
];


(* ::Input:: *)
(*debug[#Messages]&@parse[QYS`Tokenize[localPath<>"Songs\\test.qys"],All]*)


(* ::Input:: *)
(*AudioStop[];AudioPlay[#[[2]]]&@*)
(*EchoFunction["time: ",#[[1]]&]@*)
(*Timing[integrate[#MusicClips,#Effects]&[parse[QYS`Tokenize[localPath<>"Songs\\test.qys"],All]]];*)


parse[tokenizer_]:=parse[tokenizer,All];
parse[tokenizer_,sections_]:=Module[
	{
		settings,functionData,
		tokenData,sectionToken,
		trackData,MusicClips={},
		sectionDuration,duration=0,
		trackDuration,
		messages,effects,
		sectionCount,trackCount,
		startSection,endSection
	},
	settings=defaultSettings;
	effects=settings[[effectSettingTag]];
	tokenData=Association[tokenizer][["Sections"]];
	sectionCount=Length[tokenData];
	messages=ConstantArray[<|"TrackMessages"->{},"GlobalMessages"->{}|>,sectionCount];
	Switch[sections,
		_?ListQ,{startSection,endSection}=sections,
		_?NumberQ,{startSection,endSection}={sections,sections},
		_,{startSection,endSection}={1,sectionCount}
	];
	
	Do[
		sectionToken=Association@tokenData[[i]];
		sectionDuration=0;
		Do[
			If[token[["Type"]]!="FunctionToken",Continue[]];
			functionData=Association@token[["Argument"]];
			Do[
				settings[[function]]=functionData[[function]],
			{function,Keys@functionData}],
		{token,Association/@sectionToken[["GlobalSettings"]]}];
		If[Length@sectionToken[["Tracks"]]==0,effects=settings[[effectSettingTag]]];
		If[i<startSection||i>endSection,Continue[]];
		trackCount=Length@sectionToken[["Tracks"]];
		messages[[i,"TrackMessages"]]=ConstantArray[{},trackCount];
		trackDuration={};
		Do[
			trackData=trackParse[Association@sectionToken[["Tracks",j]],settings];
			AppendTo[trackDuration,trackData[["Duration"]]];
			MusicClips=Join[MusicClips,<|
				"SoundData"->#SoundData,
				"Beginning"->duration+#Beginning,
				"End"->duration+#Beginning+#Duration,
				"MetaSettings"->#MetaSettings
			|>&/@trackData[["MusicClips"]]];
			messages[[i,"TrackMessages",j]]=trackData[["Messages"]],
		{j,trackCount}];
		sectionDuration=Max[trackDuration];
		If[!SameQ@@trackDuration,AppendTo[messages[[i,"GlobalMessages"]],<|
			"Type"->"DiffDuration",
			"Info"->{trackDuration}
		|>]];
		duration+=sectionDuration,
	{i,sectionCount}];
	
	Return[<|
		"Infomation"-><|"Duration"->duration|>,
		"MusicClips"->MusicClips,
		"Messages"->messages,
		"Effects"->effects
	|>];
];


(* ::Input:: *)
(*AudioStop[];*)


(* ::Input:: *)
(*debug[#Messages]&@parse[QYS`Tokenize[localPath<>"Songs\\test.qys"],All]*)


(* ::Input:: *)
(*debug[#Messages]&@parse[QYS`Tokenize[localPath<>"Songs\\Touhou\\Deep_Mountain.qys"],All]*)


(* ::Input:: *)
(*AudioStop[];AudioPlay[#[[2]]]&@*)
(*EchoFunction["time: ",#[[1]]&]@*)
(*Timing[integrate[#MusicClips,#Effects]&[parse[QYS`Tokenize[localPath<>"Songs\\Touhou\\TH11-Chireiden\\Kyuujigoku_Kaidou.qys"],All]]];*)


(* ::Input:: *)
(*AudioStop[];AudioPlay[#[[2]]]&@*)
(*EchoFunction["time: ",#[[1]]&]@*)
(*Timing[integrate[#MusicClips,#Effects]&[parse[QYS`Tokenize[localPath<>"Songs\\Touhou\\Deep_Mountain.qys"],2]]];*)


(* ::Input:: *)
(*EmitSound@Sound@SoundNote[10,1,"SlapBass"]*)


(* ::Input:: *)
(*EmitSound@Sound@SoundNote[-20,1,"SynthBrass"]*)


(* ::Input:: *)
(*QYMP;*)


debug[messages_]:=Module[{output={},sectionOutput},
	Do[
		If[Flatten@Values@messages[[i]]!={},
			sectionOutput={};
			Do[
				If[messages[[i,"TrackMessages",j]]!={},
					AppendTo[sectionOutput,OpenerView[{
						caption["_Track",{j}],
						Column[Row[{
							caption["_Bar",{#Bar}],
							caption[": "],
							caption[msgData[[#Type]],#Info]
						}]&/@messages[[i,"TrackMessages",j]]]
					},True,Method->"Active"]]
				],
			{j,Length@messages[[i,"TrackMessages"]]}];
			sectionOutput=Join[sectionOutput,Row[{
				caption[text[[#Type]],#Info]
			}]&/@messages[[i,"GlobalMessages"]]];
			AppendTo[output,OpenerView[{
				caption["_Section",{i}],
				Column[sectionOutput]
			},True,Method->"Active"]]
		],
	{i,Length@messages}];
	If[output!={},Print[Column[output]]];
];


generate=If[MemberQ[instrData[["Style"]],#[[4]]],
	SoundNote[#[[1]],#[[2]],#[[4]],SoundVolume->#[[3]]],
	SoundNote[If[TrueQ@#[[1]],#[[4]],None],#[[2]],SoundVolume->#[[3]]]
]&;
integrate[tracks_]:=integrate[tracks,defaultSettings[[effectSettingTag]]];
integrate[tracks_,effects_]:=Module[
	{
		j,settings,instrCount,
		final={},audio=0
	},
	
	(* simplify *)
	Do[
		settings=trackData[["MetaSettings"]];
		instrCount=Length@settings[["Instr"]];
		If[Length@settings[["InstProp"]]<instrCount,
			settings[["InstProp"]]=Array[settings[["InstProp",1]]&,instrCount];
		];
		Do[
			j=LengthWhile[final,Or[
				#Duration>trackData[["Beginning"]],
				#FadeOut!=0,
				settings[["FadeIn"]]!=0
			]&]+1;
			If[j>Length@final,
				AppendTo[final,<|
					"SoundData"->(Append[#,settings[["Instr",i]]]&)/@Prepend[trackData[["SoundData"]],{None,trackData[["Beginning"]],1}],
					"Duration"->trackData[["End"]],
					"FadeIn"->settings[["FadeIn"]],
					"FadeOut"->settings[["FadeOut"]]
				|>],
				AppendTo[final[[j,"SoundData"]],{None,trackData[["Beginning"]]-final[[j,"Duration"]],1,"Piano"}];
				final[[j,"SoundData"]]=Join[final[[j,"SoundData"]],Append[#,settings[["Instr",i]]]&/@trackData[["SoundData"]]];
				final[[j,"Duration"]]=trackData[["End"]];
				final[[j,"FadeOut"]]=settings[["FadeOut"]];
			];
			Do[
				final[[j,"SoundData",k,3]]*=settings[["InstProp",i]],
			{k,Length@final[[j,"SoundData"]]}],
		{i,instrCount}],
	{trackData,tracks}];
	
	(* integrate *)
	Do[
		audio+=AudioFade[
			Sound[generate/@data[["SoundData"]]],
		{data[["FadeIn"]],data[["FadeOut"]]}],
	{data,final}];
	
	(* add effects *)
	If[effects[["FadeIn"]]+effects[["FadeOut"]]>0,
		audio=AudioFade[audio,{effects[["FadeIn"]],effects[["FadeOut"]]}]
	];
	Return[audio];
];


(* ::Input:: *)
(*AudioStop[];AudioPlay@QYSParse[localPath<>"Songs\\TouHou\\TH11-Chireiden\\Nuclear_Fusion.qys"];*)


(* ::Input:: *)
(*AudioStop[];*)


(* ::Input:: *)
(*EmitSound@Sound@SoundNote["HighTom",1]*)


(* ::Input:: *)
(*Export["E:\\1.mp3",QYSParse[localPath<>"Songs\\temp.qys"]];*)


(* ::Input:: *)
(*AudioStop[];AudioPlay@QYSParse[localPath<>"Songs\\Gate_of_Steiner.qys"];*)


(* ::Text:: *)
(*ElectricSnare, BassDrum, Shaker, RideCymbal, Snare, CrashCymbal, HiHatPedal, HiHatClosed*)
(*Ocarina, Oboe, Clarinet, Recorder, BrassSection, Harpsichord, BrightPiano, Organ, DrawbarOrgan, FretlessBass*)
