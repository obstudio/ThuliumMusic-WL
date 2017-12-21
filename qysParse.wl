(* ::Package:: *)

getPitch[score_,pos_,key_]:=Module[
	{i=pos,note,pitch},
	If[StringPart[score,i]=="[",
		i++;
		pitch={};
		While[i<=StringLength[score],
			AppendTo[pitch,getPitch[score,i,key][[1]]];
			i=getPitch[score,i,key][[2]]
		];
		i++,
		note=ToExpression@StringPart[score,i];
		pitch=If[note==0,None,pitchDict[[note]]+key];
		i++;
		While[i<=StringLength@score && MemberQ[{"#","b","'",","},StringPart[score,i]],
			Switch[StringPart[score,i],
				"#",pitch++,
				"b",pitch--,
				"'",pitch+=12,
				",",pitch-=12
			];
			i++;
		];
	];
	Return[{pitch,i}];
];


trackData[score_,global_,trackCount_]:=Module[
	{
		i,j,k,char,content,match,position,              (* loop related *)
		soundData,audio,                                (* score and tracks *)
		instrList={},                                   (* instrument *)
		percussion=False,messages={},
		parameter=global,
		
		tercet,tercetTime,                              (* tercet *)
		portamento=False,portaRate,                     (* portamento *)
		tremolo1=0,tremolo2=0,                          (* tremolo *)
		appoggiatura={},appoChord=False,                (* appoggiatura *)
		staccato,
		fermata,
		function,argument,                              (* function *)
		
		note,pitch,tonality,                            (* pitch *)
		beatCount,beam=False,extend,timeDot,            (* number of beats *)
		duration,trackDuration=0,                       (* duration *)
		lastPitch,lastBeat,
		barCount,barBeat                                (* trans-note *)
	},
	j=1;
	soundData={};
	barBeat=0;
	barCount=0;
	beam=False;
	staccato=False;
	lastPitch=Null;
	(* local variables *)
	While[j<=StringLength[score],
		char=StringPart[score,j];
		Switch[char,
			"|",
				If[barBeat!=0,
					barCount++;
					If[barBeat!=parameter[["Bar"]] && barBeat*parameter[["Bar"]]!=16,
						AppendTo[messages,generateMessage["BarLengthError",{trackCount+1,barCount,parameter[["Bar"]],barBeat}]];
					];
					barBeat=0;
				];
				j++;
				Continue[],
			"<",
				match=Select[Transpose[StringPosition[score,">"]][[1]],#>j&][[1]];
				content=StringTake[score,{j+1,match-1}];
				Which[
					StringContainsQ[content,":"],            (* function *)
						position=StringPosition[content,":"][[1,1]];
						function=StringTake[content,position-1];
						argument=ToExpression@StringDrop[content,position];
						If[MemberQ[functionList,function],
							parameter[[function]]=argument,
							AppendTo[messages,generateMessage["InvFunction",{trackCount+1,barCount+1,function}]]
						],
					StringContainsQ[content,"="],            (* key *)
						parameter[["Oct"]]=StringCount[content,"'"]-StringCount[content,","];
						tonality=StringDelete[StringTake[content,{3,StringLength@content}],","|"'"];
						If[KeyExistsQ[tonalityDict,tonality],
							parameter[["Key"]]=tonalityDict[[tonality]],
							AppendTo[messages,generateMessage["InvTonality",{trackCount+1,barCount+1,content}]];
						],
					StringContainsQ[content,"/"],            (* beat *)
						position=StringPosition[content,"/"][[1,1]];
						parameter[["Beat"]]=ToExpression[StringDrop[content,position]];
						parameter[["Bar"]]=ToExpression[StringTake[content,position-1]],
					StringContainsQ[content,"."],            (* volume *)
						parameter[["Volume"]]=ToExpression[content],
					StringMatchQ[content,NumberString],      (* speed *)
						parameter[["Speed"]]=ToExpression[content],
					True,                                    (* instrument *)
						Which[
							MemberQ[instrData[["Style"]],content],
								parameter[["Instr"]]=content;
								instrList=Union[instrList,{parameter[["Instr"]]}];
								percussion=False,
							MemberQ[instrData[["Percussion"]],content],
								parameter[["Instr"]]=content;
								instrList=Union[instrList,{parameter[["Instr"]]}];
								percussion=True,
							_,
								AppendTo[messages,generateMessage["InvInstrument",{trackCount+1,barCount+1,content}]];
						];
				];
				j=match+1;
				Continue[],
			"(",
				match=Select[Transpose[StringPosition[score,")"]][[1]],#>j&][[1]];
				content=StringTake[score,{j+1,match-2}];
				Switch[StringTake[score,{match-1}],
					"~",                            (* tercet *)
						tercet=ToExpression[content];
						tercetTime=(2^Floor[Log2[tercet]])/tercet,
					"-",                            (* single tremolo *)
						tremolo1=ToExpression[content],
					"=",                            (* double tremolo *)
						tremolo2=ToExpression[content],
					"^",                            (* appoggiatura *)
						k=1;
						While[k<=StringLength@content,
							AppendTo[appoggiatura,getPitch[content,k,12*parameter[["Oct"]]+parameter[["Key"]]][[1]]];
							k=getPitch[content,k,12*parameter[["Oct"]]+parameter[["Key"]]][[2]];
						];
				];
				j=match+1;
				Continue[],
			"~",                               (* portamento *)
				portamento=True;
				j++;
				Continue[];
		];
		(* find out the pitch *)
		j++;
		extend=False;
		Which[
			char=="x"||char=="X",
				pitch=0,
			char=="%",                                  (* the same as the last pitch *)
				pitch=lastPitch;
				If[lastPitch===Null,
					AppendTo[messages,generateMessage["NoFormerPitch",{trackCount+1,barCount+1}]]
				],
			DigitQ[char],                               (* single tone *)
				note=ToExpression@char;
				pitch=If[note==0,None,pitchDict[[note]]+12*parameter[["Oct"]]+parameter[["Key"]]],
			char=="[",                                  (* harmony *)
				match=Select[Transpose[StringPosition[score,"]"]][[1]],#>=j&][[1]];
				content=StringTake[score,{j,match-1}];
				pitch={};
				k=1;
				If[StringContainsQ[content,"^"],
					content=StringDelete[content,"^"];           (* appoggiatura & harmony *)
					While[k<=StringLength@content,
						AppendTo[pitch,getPitch[content,k,12*parameter[["Oct"]]+parameter[["Key"]]][[1]]];
						k=getPitch[content,k,12*parameter[["Oct"]]+parameter[["Key"]]][[2]];
						AppendTo[appoggiatura,pitch];
					];
					appoggiatura=Drop[appoggiatura,-1];
					appoChord=True,
					While[k<=StringLength@content,               (* common harmony *)
						AppendTo[pitch,getPitch[content,k,12*parameter[["Oct"]]+parameter[["Key"]]][[1]]];
						k=getPitch[content,k,12*parameter[["Oct"]]+parameter[["Key"]]][[2]];
					]
				];
				j=match+1,
			True,
				AppendTo[messages,generateMessage["InvCharacter",{trackCount+1,barCount+1,char}]];
		];
		While[j<=StringLength[score] && MemberQ[{"#","b","'",","},StringPart[score,j]],
			char=StringPart[score,j];
			Switch[char,
				"#",pitch++;If[appoChord,appoggiatura++],
				"b",pitch--;If[appoChord,appoggiatura--],
				"'",pitch+=12;If[appoChord,appoggiatura+=12],
				",",pitch-=12;If[appoChord,appoggiatura-=12]
			];
			j++;
		];
		If[lastPitch==pitch && beam==True,extend=True];
		(* find out the duration *)
		beatCount=1;
		beam=False;
		While[j<=StringLength[score] && MemberQ[{"-","_",".","^","`"},StringPart[score,j]],
			char=StringPart[score,j];
			Switch[char,
				"-",beatCount+=1,
				"_",beatCount/=2,
				".",
					timeDot=1/2;
					While[j<=StringLength[score] && StringPart[score,j+1]==".",
						timeDot/=2;
						j++;
					];
					beatCount*=(2-timeDot),
				"^",beam=True,
				"`",staccato=True
			];
			j++;
		];
		If[tercet>0,beatCount*=tercetTime;tercet--];
		beatCount*=2^(-parameter[["Dur"]]);
		barBeat+=beatCount;
		duration=15/parameter[["Speed"]]*beatCount*parameter[["Beat"]];
		trackDuration+=duration;
		Which[
			percussion,
				If[pitch===None,
					AppendTo[soundData,{None,duration}],
					AppendTo[soundData,{parameter[["Instr"]],duration}]
				],
			extend,
				soundData[[-1,2]]+=duration;
				lastBeat+=beatCount,
			tremolo1!=0,
				duration/=(beatCount*2^tremolo1);
				Do[
					AppendTo[soundData,{pitch,duration,parameter[["Instr"]]}],
				{k,beatCount*2^tremolo1}];
				tremolo1=0,
			tremolo2!=0,
				duration/=(beatCount*2^tremolo2);
				barBeat=barBeat-lastBeat;
				soundData=Drop[soundData,-1];
				Do[
					AppendTo[soundData,{lastPitch,duration,parameter[["Instr"]]}];
					AppendTo[soundData,{pitch,duration,parameter[["Instr"]]}],
				{k,beatCount*2^(tremolo2-1)}];
				tremolo2=0,
			portamento,
				portaRate=(pitch-lastPitch+1)/beatCount/parameter[["Port"]];
				duration/=(beatCount*parameter[["Port"]]);
				barBeat=barBeat-lastBeat;
				soundData=Drop[soundData,-1];
				Do[
					AppendTo[soundData,{Floor[k],duration,parameter[["Instr"]]}],
				{k,lastPitch,pitch,portaRate}];
				portamento=False,
			True,
				If[appoggiatura!={},
					If[Length@appoggiatura<4,
						beatCount-=Length@appoggiatura*parameter[["Appo"]]/4;
						duration=15/parameter[["Speed"]]*parameter[["Appo"]]/4*parameter[["Beat"]];
						Do[
							AppendTo[soundData,{appoggiatura[[k]],duration,parameter[["Instr"]]}],
						{k,Length@appoggiatura}],
						beatCount-=parameter[["Appo"]];
						duration=15/parameter[["Speed"]]*parameter[["Appo"]]/Length@appoggiatura*parameter[["Beat"]];
						Do[
							AppendTo[soundData,{appoggiatura[[k]],duration,parameter[["Instr"]]}],
						{k,Length@appoggiatura}];
					];
					appoggiatura={};
					appoChord=False;
					duration=15/parameter[["Speed"]]*beatCount*parameter[["Beat"]];
				];
				lastBeat=beatCount;
				If[!pitch===None,lastPitch=pitch];
				If[staccato,
					AppendTo[soundData,{pitch,duration*(1-parameter[["Stac"]]),parameter[["Instr"]]}];
					AppendTo[soundData,{None,duration*parameter[["Stac"]]}];
					staccato=False,
					AppendTo[soundData,{pitch,duration,parameter[["Instr"]]}];
				];
		];
	];
	Return[<|
		"Audio"->If[soundData=={},0,                     (* empty track *)
			If[StringPart[score,j-1]!="|",
				AppendTo[messages,generateMessage["TerminatorAbsent",{trackCount+1}]]
			];
			parameter[["Volume"]]*AudioFade[Sound[SoundNote@@#&/@soundData],{parameter[["FadeIn"]],parameter[["FadeOut"]]}]
		],
		"Local"->parameter,
		"Messages"->messages,
		"Duration"->trackDuration,
		"Instruments"->instrList
	|>];
];


QYSParse[filename_]:=Module[
	{
		i,j,
		data1,data2,score,join,repeatL,repeatR,
		parameter=defaultParameter,
		messages={},instrList={},track,
		audio=0,duration=0,
		trackCount=0,trackDuration={},
		sectionCount=0,sectionDuration=0
	},
	If[!FileExistsQ[filename],
		AppendTo[messages,generateMessage["FileNotFound",{filename}]];
	];
	data1=StringJoin/@Import[filename,"Table"];             (* delete the spacings *)
	data1=Select[data1,!StringContainsQ[#,"//"]&];          (* delete the comments *)
	data1=Cases[data1,Except[""]];                          (* delete the blank lines *)
	data2={};j=0;join=False;                                (* join multiple lines of music scores together *)
	Do[
		If[join,
			join=False;data2[[j]]=data2[[j]]<>"|"<>data1[[i]],
			j++;AppendTo[data2,data1[[i]]]
		];
		If[StringPart[data1[[i]],-1]=="\\",join=True],
	{i,Length@data1}];
	data2=StringDelete[#,"\\"]&/@data2;                     (* delete the joint marks *)
	score=Array[""&,Length@data2];
	Do[
		If[StringPosition[data2[[i]],"|:"]=={},score[[i]]=data2[[i]],
			repeatL=Transpose[StringPosition[data2[[i]],"|:"]][[1]];			(* repeat *)
			repeatR=Transpose[StringPosition[data2[[i]],":|"]][[1]];
			If[Length@repeatL!=Length@repeatR,
				AppendTo[messages,generateMessage["RepeatError",{score}]];
				Continue[];
			];
			score[[i]]=StringTake[data2[[i]],repeatL[[1]]-1]<>"|";
			Do[
				score[[i]]=score[[i]]<>StringTake[data2[[i]],{repeatL[[j]]+2,repeatR[[j]]-1}]<>"|";
				score[[i]]=score[[i]]<>StringTake[data2[[i]],{repeatL[[j]]+2,repeatR[[j]]-1}]<>"|";
				If[repeatR[[j]]+2<repeatL[[j+1]],
					score[[i]]=score[[i]]<>StringTake[data2[[i]],{repeatR[[j]]+2,repeatL[[j+1]]-1}]<>"|"
				],
			{j,Length@repeatL-1}];
			score[[i]]=score[[i]]<>StringTake[data2[[i]],{repeatL[[-1]]+2,repeatR[[-1]]-1}]<>"|";
			score[[i]]=score[[i]]<>StringTake[data2[[i]],{repeatL[[-1]]+2,repeatR[[-1]]-1}]<>"|";
			If[repeatR[[-1]]+1<StringLength@data2[[i]],
				score[[i]]=score[[i]]<>StringTake[data2[[i]],{repeatR[[-1]]+2,StringLength@data2[[i]]}]
			];
		],
	{i,Length@data2}];	
	Do[
		track=trackData[score[[i]],parameter,trackCount];
		instrList=Union[instrList,track[["Instruments"]]];
		messages=Join[messages,track[["Messages"]]];
		If[track[["Duration"]]==0,
			(* empty soundtrack *)
			parameter=track[["Local"]];
			If[sectionDuration!={},duration+=Max@sectionDuration];
			sectionCount++;
			If[!SameQ@@sectionDuration,
				AppendTo[messages,generateMessage["DiffDuration",{sectionCount}]];
			];
			sectionDuration={},
			(* real soundtrack *)
			trackCount++;
			AppendTo[sectionDuration,track[["Duration"]]];
			If[duration!=0,
				audio+=AudioPad[track[["Audio"]],{duration,0}],
				audio+=track[["Audio"]]
			];
		],
	{i,Length[score]}];
	If[sectionDuration!={},duration+=Max@sectionDuration];
	If[!SameQ@@sectionDuration,
		AppendTo[messages,generateMessage["DiffDuration",{sectionCount,sectionDuration}]];
	];
	If[parameter[["FadeIn"]]+parameter[["FadeOut"]]>0,audio=AudioFade[audio,{parameter[["FadeIn"]],parameter[["FadeOut"]]}]];
	If[messages!={},Print[Column@messages]];
	Return[Audio[audio,MetaInformation-><|
		"Format"->"qys",
		"TrackCount"->trackCount,
		"Duration"->duration,
		"Instruments"->instrList,
		"Messages"->messages
	|>]];
];


(* ::Input:: *)
(*AudioStop[];AudioPlay@QYSParse["E:\\QingyunMusicPlayer\\Songs\\test.qys"];*)


(* ::Input:: *)
(*AudioStop[];AudioPlay@QYSParse["E:\\QingyunMusicPlayer\\Songs\\Noushyou_Sakuretsu_Garu.qys"];*)


(* ::Input:: *)
(*AudioStop[];*)


(* ::Input:: *)
(*EmitSound@Sound@SoundNote["Snare",1]*)


(* ::Input:: *)
(*Export["e:\\1.mp3",QYSParse["E:\\QingyunMusicPlayer\\Songs\\Noushyou_Sakuretsu_Garu.qys"]];*)


(* ::Text:: *)
(*ElectricSnare, BassDrum, Shaker, RideCymbal, Snare, CrashCymbal, HiHatPedal*)
(*Ocarina, Oboe, Clarinet, Recorder*)
