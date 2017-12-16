(* ::Package:: *)

debug=True;


getPitch[score_,pos_,tonality_]:=Module[
	{i=pos,note,pitch},
	note=ToExpression@StringPart[score,i];
	pitch=If[note==0,None,pitchDict[[note]]+tonality];
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
	Return[{pitch,i}];
];


getScore[filename_]:=Module[
	{i,j,data1,data2,score,join,repeatL,repeatR},
	If[!FileExistsQ[filename],Return[{}]];
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
		If[StringPosition[data2[[i]],"|:"]=={},
			score[[i]]=data2[[i]],
			(* repeat *)
			repeatL=Transpose[StringPosition[data2[[i]],"|:"]][[1]];
			repeatR=Transpose[StringPosition[data2[[i]],":|"]][[1]];
			If[Length@repeatL!=Length@repeatR,Print[error[["RepeatError"]],"!"]];
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
	Return[score];
];


parse[filename_,"qys"]:=Module[
	{
		i,j,k,char,content,match,position,              (* loop related *)
		score,trackCount=0,track,audio=0,               (* score and tracks *)
		instrument="Piano",instrList={},                (* instrument *)
		tercet,tercetTime,                              (* tercet *)
		portamento,rate,                                (* portamento *)
		tremolo1,tremolo2,                              (* tremolo *)
		appoggiatura,appogHarmony,                     (* appoggiatura *)
		note,pitch,scale,tonality=0,                    (* pitch *)
		beatCount,extend,timeDot,beat=4,                (* number of beats *)
		duration,space,speed=88,                        (* duration *)
		lastPitch,lastBeat,
		barCount,barBeat,barLength=4,                   (* trans-note *)
		volume=1,fade,fadeAll={0,0}                     (* volume *)
	},
	score=getScore[filename];
	If[debug && score=={},Print[error[["FileNotFound"]],"!"]];
	Do[
		j=1;
		space=True;
		portamento=False;
		appogHarmony=False;
		appoggiatura={};
		tremolo2=0;
		fade={0,0};
		barBeat=0;
		barCount=0;
		track={};
		While[j<=StringLength[score[[i]]],
			char=StringPart[score[[i]],j];
			Switch[char,
				"|",
					If[debug && barBeat!=0,
						barCount++;
						If[debug && barBeat!=barLength && barBeat*barLength!=16,
							Print[error[["BarLengthError"]],"!"];
							Print["[Info] Track:",trackCount+1,"; Bar:",barCount,"; BarLength:",barLength,"; BarBeat:",barBeat];
						];
						barBeat=0;
					];
					j++;
					Continue[],
				"<",
					match=Select[Transpose[StringPosition[score[[i]],">"]][[1]],#>j&][[1]];
					content=StringTake[score[[i]],{j+1,match-1}];
					Which[
						StringContainsQ[content,"="],            (* tonality *)
							scale=StringCount[content,"'"]-StringCount[content,","];
							tonality=12*scale+tonalityDict[[StringDelete[StringTake[content,{3,StringLength@content}],","|"'"]]],
						StringContainsQ[content,"/"],            (* beat *)
							position=StringPosition[content,"/"][[1,1]];
							beat=ToExpression[StringDrop[content,position]];
							barLength=ToExpression[StringTake[content,position-1]],
						StringContainsQ[content,"."],            (* volume *)
							volume=ToExpression[content],
						StringMatchQ[content,NumberString],      (* speed *)
							speed=ToExpression[content],
						StringContainsQ[content,"Fade:"],        (* fade *)
							position=StringPosition[content,":"][[1,1]];
							If[#>0,fade[[1]]=#,fade[[2]]=-#]&@ToExpression[StringDrop[content,position]];
					];
					j=match+1;
					Continue[],
				"(",
					match=Select[Transpose[StringPosition[score[[i]],")"]][[1]],#>j&][[1]];
					content=StringTake[score[[i]],{j+1,match-2}];
					Switch[StringTake[score[[i]],{match-1}],
						"-",                            (* tercet *)
							tercet=ToExpression[content];
							tercetTime=(2^Floor[Log2[tercet]])/tercet,
						"~",                            (* single tremolo *)
							tremolo1=ToExpression[content],
						"=",                            (* double tremolo *)
							tremolo2=ToExpression[content],
						"^",                            (* appoggiatura *)
							k=1;
							While[k<=StringLength@content,
								AppendTo[appoggiatura,getPitch[content,k,tonality][[1]]];
								k=getPitch[content,k,tonality][[2]];
							];
					];
					j=match+1;
					Continue[],
				"{",                               (* instrument *)
					match=Select[Transpose[StringPosition[score[[i]],"}"]][[1]],#>j&][[1]];
					instrument=StringTake[score[[i]],{j+1,match-1}];
					If[debug && !MemberQ[instrData[["Style"]],instrument],Print[error[["InstrNotExist"]],"!"]];
					instrList=Union[instrList,{instrument}];
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
				char=="%",                                  (* the same as the last pitch *)
					pitch=lastPitch,
				DigitQ[char],                               (* single tone *)
					note=ToExpression@char;
					pitch=If[note==0,None,pitchDict[[note]]+tonality],
				char=="[",                                  (* harmony *)
					match=Select[Transpose[StringPosition[score[[i]],"]"]][[1]],#>=j&][[1]];
					content=StringTake[score[[i]],{j,match-1}];
					pitch={};
					k=1;
					If[StringContainsQ[content,"^"],
						content=StringDelete[content,"^"];           (* appoggiatura & harmony *)
						While[k<=StringLength@content,
							AppendTo[pitch,getPitch[content,k,tonality][[1]]];
							k=getPitch[content,k,tonality][[2]];
							AppendTo[appoggiatura,pitch];
						];
						appoggiatura=Drop[appoggiatura,-1];
						appogHarmony=True,
						While[k<=StringLength@content,               (* common harmony *)
							AppendTo[pitch,getPitch[content,k,tonality][[1]]];
							k=getPitch[content,k,tonality][[2]];
						]
					];
					j=match+1,
				debug,
					Print[error[["ExpectPitch"]]];
			];
			While[j<=StringLength[score[[i]]] && MemberQ[{"#","b","'",","},StringPart[score[[i]],j]],
				char=StringPart[score[[i]],j];
				Switch[char,
					"#",pitch++;If[appogHarmony,appoggiatura++],
					"b",pitch--;If[appogHarmony,appoggiatura--],
					"'",pitch+=12;If[appogHarmony,appoggiatura+=12],
					",",pitch-=12;If[appogHarmony,appoggiatura-=12]
				];
				j++;
			];
			If[lastPitch==pitch && space==False,extend=True];
			(* find out the duration *)
			beatCount=1;
			space=True;
			While[j<=StringLength[score[[i]]]&&MemberQ[{"-","_",".","^"},StringPart[score[[i]],j]],
				char=StringPart[score[[i]],j];
				Switch[char,
					"-",beatCount+=1,
					"_",beatCount/=2,
					".",
						timeDot=1/2;
						While[j<=StringLength[score[[i]]] && StringPart[score[[i]],j+1]==".",
							timeDot/=2;
							j++;
						];
						beatCount*=(2-timeDot),
					"^",space=False
				];
				j++;
			];
			If[tercet>0,beatCount*=tercetTime;tercet--];
			barBeat+=beatCount;
			duration=15/speed*beatCount*beat;
			If[tremolo1!=0,
				duration/=(beatCount*2^tremolo1);
				Do[
					AppendTo[track,{pitch,duration,instrument}],
				{k,beatCount*2^tremolo1}];
				tremolo1=0;
				Continue[];
			];
			If[tremolo2!=0,
				duration/=(beatCount*2^tremolo2);
				barBeat=barBeat-lastBeat;
				track=Drop[track,-2];
				Do[
					AppendTo[track,{lastPitch,duration,instrument}];
					AppendTo[track,{pitch,duration,instrument}],
				{k,beatCount*2^(tremolo2-1)}];
				tremolo2=0;
				Continue[];
			];
			If[portamento,
				rate=(pitch-lastPitch+1)/beatCount/6;
				duration/=(beatCount*6);
				barBeat=barBeat-lastBeat;
				track=Drop[track,-2];
				Do[
					AppendTo[track,{Floor[k],duration,instrument}],
				{k,lastPitch,pitch,rate}];
				portamento=False;
				Continue[];
			];
			If[appoggiatura!={},
				If[Length@appoggiatura<4,
					beatCount-=Length@appoggiatura/12;
					duration=15/speed/12*beat;
					Do[
						AppendTo[track,{appoggiatura[[k]],duration,instrument}],
					{k,Length@appoggiatura}],
					beatCount-=1/3;
					duration=15/speed/Length@appoggiatura/2*beat;
					Do[
						AppendTo[track,{appoggiatura[[k]],duration,instrument}],
					{k,Length@appoggiatura}];
				];
				appoggiatura={};
				appogHarmony=False;
				duration=15/speed*beatCount*beat;
			];
			lastBeat=beatCount;
			If[!pitch===None,lastPitch=pitch];
			If[extend,
				If[space,
					track[[-1,2]]+=duration*7/8;
					AppendTo[track,{None,duration/8}],
					track[[-1,2]]+=duration;
				],
				If[space,
					AppendTo[track,{pitch,duration*7/8,instrument}];
					AppendTo[track,{None,duration/8}],
					AppendTo[track,{pitch,duration,instrument}];
				];
			];
		];
		If[track!={},
			trackCount++;
			If[debug && StringTake[score[[i]],{j-1}]!="|",
				Print[error[["TerminatorAbsent"]]];
				Print["[Info] Track:",trackCount];
			];
			audio+=volume*AudioFade[Sound[SoundNote@@#&/@track],fade],
			fadeAll=fade;
		],
	{i,Length[score]}];
	If[fadeAll!={0,0},audio=AudioFade[audio,fadeAll]];
	Return[Audio[audio,MetaInformation-><|
		"Format"->"qys",
		"TrackCount"->trackCount,
		"Duration"->QuantityMagnitude@UnitConvert[Duration@audio,"Seconds"],
		"Instruments"->instrList
	|>]];
];


(* ::Input:: *)
(*debug=True;*)


(* ::Input:: *)
(*AudioStop[];AudioPlay@parse["E:\\QingyunMusicPlayer\\Songs\\Lonely_Night.qys","qys"];*)


(* ::Input:: *)
(*AudioStop[];*)


(* ::Input:: *)
(*EmitSound@Sound@SoundNote[0,1,"Xylophone"]*)


(* ::Input:: *)
(*Export["e:\\1.mp3",parse["E:\\QingyunMusicPlayer\\Songs\\Lonely_Night.qys","qys"]];*)
