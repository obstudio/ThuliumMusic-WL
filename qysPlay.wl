(* ::Package:: *)

tonalityDict=<|
	"C"->0,"G"->7,"D"->2,"A"->-3,"E"->4,
	"B"->-1,"#F"->6,"#C"->1,"F"->5,"bB"->-2,
	"bE"->3,"bA"->-4,"bD"->1,"bG"->6,"bC"->-1
|>;
pitchDict=<|"1"->0,"2"->2,"3"->4,"4"->5,"5"->7,"6"->9,"7"->11|>;


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
	If[!FileExistsQ[filename],
		MessageDialog[TextCell["File not found!"],WindowTitle->"Error"];
		Return[];
	];
	data1=StringJoin/@Import[filename,"Table"];             (* delete the spacings *)
	data1=Select[data1,!StringContainsQ[#,"//"]&];          (* delete the comments *)
	data1=Cases[data1,Except[""]];                          (* delete the blank lines *)
	data2={};j=0;join=False;                                (* join multiple lines of music scores together *)
	Do[
		If[join,
			join=False;data2[[j]]=data2[[j]]<>data1[[i]],
			j++;AppendTo[data2,data1[[i]]]
		];
		If[StringPart[data1[[i]],-1]=="\\",join=True],
	{i,Length@data1}];
	data2=StringDelete[#,"|"|"\\"]&/@data2;                 (* delete the joint marks *)
	score=Array[""&,Length@data2];
	Do[
		If[StringPosition[data2[[i]],":"]=={},
			score[[i]]=data2[[i]],
			(* repeat *)
			repeat=Partition[Transpose[StringPosition[data2[[i]],":"]][[1]],2];
			score[[i]]=StringTake[data2[[i]],repeat[[1,1]]-1];
			Do[
				score[[i]]=score[[i]]<>StringTake[data2[[i]],{repeat[[j,1]]+1,repeat[[j,2]]-1}];
				score[[i]]=score[[i]]<>StringTake[data2[[i]],{repeat[[j,1]]+1,repeat[[j,2]]-1}];
				score[[i]]=score[[i]]<>StringTake[data2[[i]],{repeat[[j,2]]+1,repeat[[j+1,1]]-1}],
			{j,Length@repeat-1}];
			score[[i]]=score[[i]]<>StringTake[data2[[i]],{repeat[[-1,1]]+1,repeat[[-1,2]]-1}];
			score[[i]]=score[[i]]<>StringTake[data2[[i]],{repeat[[-1,1]]+1,repeat[[-1,2]]-1}];
			score[[i]]=score[[i]]<>StringTake[data2[[i]],{repeat[[-1,2]]+1,StringLength@data2[[i]]}];
		],
	{i,Length@data2}];
	Return[score];
];


qysPlay[filename_]:=Module[
	{
		i,j,k,char,                                     (* loop related *)
		score,track={},voicePart,                       (* score and tracks*)
		comment,match,                                  (* comment related *)
		instrument="Piano",instrumentList={},           (* instrument *)
		tonality=0,beat=1,speed=88,volume=1,            (* angle bracket related *)
		tercet=0,tercetTime,tremolo,appoggiatura,       (* round bracket related *)
		note,pitch,time,space,timeDot,duration,
		lastPitch,extend,portamento,rate		
	},
	score=getScore[filename];
	Do[
		j=1;
		space=True;
		portamento=False;
		appoggiatura=False;
		tremolo=0;
		voicePart={};
		While[j<=StringLength[score[[i]]],
			char=StringPart[score[[i]],j];
			Switch[char,
				"<",
					match=Select[Transpose[StringPosition[score[[i]],">"]][[1]],#>j&][[1]];
					comment=StringTake[score[[i]],{j+1,match-1}];
					Switch[StringPart[comment,2],
						"=",                       (* tonality *)
							tonality=tonalityDict[[StringTake[comment,{3,StringLength@comment}]]],
						"/",                       (* beat *)
							beat=ToExpression[StringTake[comment,{3,StringLength@comment}]]/4,
						".",                       (* volume *)
							volume=ToExpression[comment],
						_,                         (* speed *)
							speed=ToExpression[comment];
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
					instrumentList=Union[instrumentList,{instrument}];
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
			];
			If[lastPitch==pitch && space==False,extend=True];
			(* find out the duration *)
			time=1;
			space=True;
			While[j<=StringLength[score[[i]]]&&MemberQ[{"-","_",".","^"},StringPart[score[[i]],j]],
				char=StringPart[score[[i]],j];
				Switch[char,
					"-",time+=1,
					"_",time/=2,
					".",
						timeDot=1/2;
						While[j<=StringLength[score[[i]]] && StringPart[score[[i]],j+1]==".",
							timeDot/=2;
							j++;
						];
						time*=(2-timeDot),
					"^",space=False
				];
				j++;
			];
			If[tercet>0,time*=tercetTime;tercet--];
			If[appoggiatura,
				time-=1/4;
				duration=60/speed/4/Length@lastPitch*beat;
				Do[
					AppendTo[voicePart,{lastPitch[[k]],duration,instrument}],
				{k,Length@lastPitch}];
				appoggiatura=False;
			];
			duration=60/speed*time*beat;
			If[tremolo!=0,
				duration/=(time*2^tremolo);
				voicePart=Drop[voicePart,-2];
				Do[
					AppendTo[voicePart,{lastPitch,duration,instrument}];
					AppendTo[voicePart,{pitch,duration,instrument}],
				{k,time*2^(tremolo-1)}];
				tremolo=0;
				Continue[];
			];
			If[portamento,
				rate=(pitch-lastPitch+1)/time/6;
				duration/=(time*6);
				voicePart=Drop[voicePart,-2];
				Do[
					AppendTo[voicePart,{Floor[k],duration,instrument}],
				{k,lastPitch,pitch,rate}];
				portamento=False;
				Continue[];
			];
			lastPitch=pitch;		
			If[extend,
				If[space,
					voicePart[[-1,2]]+=duration*7/8;
					AppendTo[voicePart,{None,duration/8}],
					voicePart[[-1,2]]+=duration;
				],
				If[space,
					AppendTo[voicePart,{pitch,duration*7/8,instrument}];
					AppendTo[voicePart,{None,duration/8}],
					AppendTo[voicePart,{pitch,duration,instrument}];
				];
			];
		];
		If[voicePart!={},AppendTo[track,volume*Audio[Sound[SoundNote@@#&/@voicePart]]]],
	{i,Length[score]}];
	Return[Audio[AudioOverlay[track],MetaInformation-><|
		"TrackCount"->Length@track,
		"Duration"->Max@Duration/@track,
		"Instruments"->instrumentList
	|>]];
];


(* ::Input:: *)
(*AudioPlay@qysPlay["E:\\QingyunMusicPlayer\\Songs\\Rainbow.qys"];*)


(* ::Input:: *)
(*AudioStop[];*)
