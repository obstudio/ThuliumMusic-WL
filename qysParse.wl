(* ::Package:: *)

debug=True;


getPitch[score_,pos_,tonality_]:=Module[
	{i=pos,note,pitch},
	note=ToExpression@StringPart[score,i];
	pitch=If[note==0,None,pitchDict[[note]]+tonality];
	i++;
	While[i<=StringLength[score] && MemberQ[{"#","b","'",","},StringPart[score,i]],
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
	{i,j,data1,data2,score,join,repeat},
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
		If[StringPosition[data2[[i]],":"]=={},
			score[[i]]=data2[[i]],
			(* repeat *)
			repeat=Partition[Transpose[StringPosition[data2[[i]],":"]][[1]],2];
			score[[i]]=StringTake[data2[[i]],repeat[[1,1]]-1];
			Do[
				score[[i]]=score[[i]]<>StringTake[data2[[i]],{repeat[[j,1]]+1,repeat[[j,2]]-1}]<>"|";
				score[[i]]=score[[i]]<>StringTake[data2[[i]],{repeat[[j,1]]+1,repeat[[j,2]]-1}];
				score[[i]]=score[[i]]<>StringTake[data2[[i]],{repeat[[j,2]]+1,repeat[[j+1,1]]-1}],
			{j,Length@repeat-1}];
			score[[i]]=score[[i]]<>StringTake[data2[[i]],{repeat[[-1,1]]+1,repeat[[-1,2]]-1}]<>"|";
			score[[i]]=score[[i]]<>StringTake[data2[[i]],{repeat[[-1,1]]+1,repeat[[-1,2]]-1}];
			score[[i]]=score[[i]]<>StringTake[data2[[i]],{repeat[[-1,2]]+1,StringLength@data2[[i]]}];
		],
	{i,Length@data2}];
	Return[score];
];


parse[filename_,"qys"]:=Module[
	{
		i,j,k,char,comment,match,position,              (* loop related *)
		score,trackCount=0,track,audio=0,               (* score and tracks *)
		instrument="Piano",instrList={},                (* instrument *)
		tercet,tercetTime,                              (* tercet *)
		portamento,rate,                                (* portamento *)
		tremolo,appoggiatura,                           (* tremolo and appoggiatura *)
		note,pitch,scale,tonality=0,                    (* pitch *)
		beatCount,extend,timeDot,beat=4,                (* number of beats *)
		duration,space,speed=88,                        (* duration *)
		lastPitch,
		barCount,barBeat,barLength=4,                   (* trans-note *)
		volume=1,fade=False                             (* volume *)
	},
	score=getScore[filename];
	If[debug && score=={},Print[error[["FileNotFound"]],"!"]];
	Do[
		j=1;
		space=True;
		portamento=False;
		appoggiatura=False;
		tremolo=0;
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
					comment=StringTake[score[[i]],{j+1,match-1}];
					Which[
						StringContainsQ[comment,"="],            (* tonality *)
							scale=StringCount[comment,"'"]-StringCount[comment,","];
							tonality=12*scale+tonalityDict[[StringDelete[StringTake[comment,{3,StringLength@comment}],","|"'"]]],
						StringContainsQ[comment,"/"],            (* beat *)
							position=StringPosition[comment,"/"][[1,1]];
							beat=ToExpression[StringDrop[comment,position]];
							barLength=ToExpression[StringTake[comment,position-1]],
						StringContainsQ[comment,"."],            (* volume *)
							volume=ToExpression[comment],
						StringMatchQ[comment,NumberString],      (* speed *)
							speed=ToExpression[comment],
						comment=="Fade",
							fade=True
					];
					j=match+1;
					Continue[],
				"(",
					match=Select[Transpose[StringPosition[score[[i]],")"]][[1]],#>j&][[1]];
					comment=StringTake[score[[i]],{j+1,match-2}];
					Switch[StringTake[score[[i]],{match-1}],
						"-",                       (* tercet *)
							tercet=ToExpression[comment];
							tercetTime=(2^Floor[Log2[tercet]])/tercet,
						"=",                       (* tremolo *)
							tremolo=ToExpression[comment],
						"^",                       (* appoggiatura *)
							lastPitch={};
							appoggiatura=True;
							k=1;
							While[k<=StringLength@comment,
								AppendTo[lastPitch,getPitch[comment,k,tonality][[1]]];
								k=getPitch[comment,k,tonality][[2]];
							];
					];
					j=match+1;
					Continue[],
				"{",                               (* instrument *)
					match=Select[Transpose[StringPosition[score[[i]],"}"]][[1]],#>j&][[1]];
					instrument=StringTake[score[[i]],{j+1,match-1}];
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
			If[char=="%",
				pitch=lastPitch,                            (* the same as the last pitch *)
				If[DigitQ[char],
					note=ToExpression@char;                 (* single-tone *)
					pitch=If[note==0,None,pitchDict[[note]]+tonality],
					pitch={};                               (* harmony *)
					While[StringPart[score[[i]],j]!="]",
						AppendTo[pitch,getPitch[score[[i]],j,tonality][[1]]];
						j=getPitch[score[[i]],j,tonality][[2]];
					];
					j++;
				];
			];
			While[j<=StringLength[score[[i]]] && MemberQ[{"#","b","'",","},StringPart[score[[i]],j]],
				char=StringPart[score[[i]],j];
				Switch[char,
					"#",pitch++,
					"b",pitch--,
					"'",pitch+=12,
					",",pitch-=12
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
			If[appoggiatura,
				beatCount-=1/4;
				duration=15/speed/Length@lastPitch*beat;
				Do[
					AppendTo[track,{lastPitch[[k]],duration,instrument}],
				{k,Length@lastPitch}];
				appoggiatura=False;
			];
			duration=15/speed*beatCount*beat;
			If[tremolo!=0,
				duration/=(beatCount*2^tremolo);
				track=Drop[track,-2];
				Do[
					AppendTo[track,{lastPitch,duration,instrument}];
					AppendTo[track,{pitch,duration,instrument}],
				{k,beatCount*2^(tremolo-1)}];
				tremolo=0;
				Continue[];
			];
			If[portamento,
				rate=(pitch-lastPitch+1)/beatCount/6;
				duration/=(beatCount*6);
				track=Drop[track,-2];
				Do[
					AppendTo[track,{Floor[k],duration,instrument}],
				{k,lastPitch,pitch,rate}];
				portamento=False;
				Continue[];
			];
			lastPitch=pitch;		
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
			AddTo[audio,volume*Audio[Sound[SoundNote@@#&/@track]]]
		],
	{i,Length[score]}];
	If[fade,audio=AudioFade[audio,{0,2}]];
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
(*AudioStop[];AudioPlay@parse["E:\\QingyunMusicPlayer\\Songs\\Bios.qys","qys"];*)


(* ::Input:: *)
(*AudioStop[];*)


(* ::Input:: *)
(*EmitSound@Sound@SoundNote[0,1,"Xylophone"]*)


(* ::Input:: *)
(*Export["e:\\1.mp3",parse["E:\\QingyunMusicPlayer\\Songs\\Bios.qys","qys"]];*)


(* ::Input:: *)
(*getScore["E:\\QingyunMusicPlayer\\Songs\\Bios_3.qys"]*)
