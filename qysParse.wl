(* ::Package:: *)

getPitch[score_,pos_,para_]:=Module[
	{i=pos,note,pitch,pitchData,appoQ=False},
	If[StringPart[score,i]=="[",
		i++;
		pitch={};
		While[i<=StringLength[score] && StringPart[score,i]!="]",
			pitchData=getPitch[score,i,para];
			AppendTo[pitch,pitchData[[1]]+12*para[["Oct"]]+para[["Key"]]];
			i=pitchData[[2]];
			appoQ=appoQ||pitchData[[3]];
		],
		note=ToExpression@StringPart[score,i];
		pitch=If[note==0,None,pitchDict[[note]]+12*para[["Oct"]]+para[["Key"]]]
	];
	i++;
	If[i<=StringLength@score && StringPart[score,i]=="^",appoQ=True;i++];
	While[i<=StringLength@score && MemberQ[pitchOpList,StringPart[score,i]],
		If[StringPart[score,i]=="$",
			pitch+=para[["Chord"]],
			pitch+=pitchOpDict[[StringPart[score,i]]]
		];
		i++;
	];
	Return[{pitch,i,appoQ}];
];


track[score_,global_,location_]:=Module[
	{
		i,j,k,char,                                     (* loop related *)
		content,match,position,                         (* operators *)
		function,argument,                              (* functions *)
		soundData={},audio,                             (* soundnotes *)
		instrList={},messages={},                       (* metainformation *)
		parameter=global,
		pitchData,
		
		percussion=False,                               (* percussion *)
		tercet,tercetTime,                              (* tercet *)
		portamento=False,portaRate,                     (* portamento *)
		tremolo1=0,tremolo2=0,                          (* tremolo *)
		appoggiatura={},                                (* appoggiatura *)
		staccato,fermata,                               (* duration related *)
		beam=False,extend,                              (* extend a note *)
		
		note,pitch,tonality,                            (* pitch *)
		beatCount,timeDot,                              (* number of beats *)
		duration,trackDuration=0,                       (* duration *)
		barCount,barBeat,                               (* trans-note *)
		lastPitch,lastBeat
	},
	j=1;
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
						AppendTo[messages,generateMessage["BarLengthError",Join[location,{barCount,parameter[["Bar"]],barBeat}]]];
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
						argument=toArgument@StringDrop[content,position];
						If[MemberQ[funcList,function],
							parameter[[function]]=argument,
							AppendTo[messages,generateMessage["InvFunction",Join[location,{barCount+1,function}]]]
						],
					StringContainsQ[content,"="],            (* key *)
						parameter[["Oct"]]=StringCount[content,"'"]-StringCount[content,","];
						tonality=StringDelete[StringTake[content,{3,StringLength@content}],","|"'"];
						If[KeyExistsQ[tonalityDict,tonality],
							parameter[["Key"]]=tonalityDict[[tonality]],
							AppendTo[messages,generateMessage["InvTonality",Join[location,{barCount+1,content}]]];
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
								AppendTo[messages,generateMessage["InvInstrument",Join[location,{barCount+1,content}]]];
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
							pitchData=getPitch[content,k,parameter];
							AppendTo[appoggiatura,pitchData[[1]]];
							k=pitchData[[2]];
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
					AppendTo[messages,generateMessage["NoFormerPitch",Join[location,{barCount+1}]]]
				];
				While[j<=StringLength@score && MemberQ[pitchOpList,StringPart[score,j]],
					If[StringPart[score,j]=="$",
						pitch+=parameter[["Chord"]],
						pitch+=pitchOpDict[[StringPart[score,j]]]
					];
					j++;
				],
			DigitQ[char]||char=="[",
				pitchData=getPitch[score,j-1,parameter];
				pitch=pitchData[[1]];
				j=pitchData[[2]];
				If[pitchData[[3]],appoggiatura=Flatten/@Array[Take[pitch,#]&,Length@pitch-1]];
				If[char=="[",pitch=Flatten@pitch],
			True,
				AppendTo[messages,generateMessage["InvCharacter",Join[location,{barCount+1,char}]]];
				pitch=None;
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
				AppendTo[messages,generateMessage["TerminatorAbsent",location]]
			];
			parameter[["Volume"]]*AudioFade[Sound[
				SoundNote@@#&/@soundData
			],{parameter[["FadeIn"]],parameter[["FadeOut"]]}]
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
		messages={},instrList={},
		trackCount=0,trackData,
		sectionCount=0,sections={},
		audio=0,duration=0,
		sectionDuration={}
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
		trackCount++;
		trackData=track[score[[i]],parameter,{sectionCount,trackCount}];
		instrList=Union[instrList,trackData[["Instruments"]]];
		messages=Join[messages,trackData[["Messages"]]];
		If[trackData[["Duration"]]==0,
			(* empty soundtrack *)
			parameter=trackData[["Local"]];
			If[sectionDuration!={},duration+=Max@sectionDuration];
			sectionCount++;
			If[!SameQ@@sectionDuration,
				AppendTo[messages,generateMessage["DiffDuration",{sectionCount,sectionDuration}]];
			];
			sectionDuration={};
			trackCount=0;
			AppendTo[sections,0],
			(* real soundtrack *)
			trackCount++;
			If[sectionCount==0,sectionCount++;AppendTo[sections,0]];
			AppendTo[sectionDuration,trackData[["Duration"]]];
			sections[[sectionCount]]+=trackData[["Audio"]];
		],
	{i,Length[score]}];
	If[sectionDuration!={},duration+=Max@sectionDuration];
	If[!SameQ@@sectionDuration,
		AppendTo[messages,generateMessage["DiffDuration",{sectionCount,sectionDuration}]];
	];
	audio=AudioJoin@Cases[sections,Except[0]];
	If[parameter[["FadeIn"]]+parameter[["FadeOut"]]>0,
		audio=AudioFade[audio,{parameter[["FadeIn"]],parameter[["FadeOut"]]}]
	];
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
(*AudioStop[];AudioPlay@QYSParse[path<>"Songs\\TouHou\\Hartmann_No_Youkai_Otome.qys"];*)


(* ::Input:: *)
(*AudioStop[];*)


(* ::Input:: *)
(*EmitSound@Sound@SoundNote[-12,1,"Xylophone"]*)


(* ::Input:: *)
(*EmitSound@Sound@SoundNote["HiHatPedal",1]*)


(* ::Input:: *)
(*AudioStop[];AudioPlay@QYSParse[path<>"Songs\\temp.qys"];*)


(* ::Text:: *)
(*ElectricSnare, BassDrum, Shaker, RideCymbal, Snare, CrashCymbal, HiHatPedal, HiHatClosed*)
(*Ocarina, Oboe, Clarinet, Recorder, BrassSection, Harpsichord, BrightPiano, Organ, DrawbarOrgan, FretlessBass*)
