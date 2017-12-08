(* ::Package:: *)

tonalityDict=<|
	"C"->0,"G"->7,"D"->2,"A"->-3,"E"->4,
	"B"->-1,"#F"->6,"#C"->1,"F"->5,"bB"->-2,
	"bE"->3,"bA"->-4,"bD"->1,"bG"->6,"bC"->-1
|>;
pitchDict=<|"1"->0,"2"->2,"3"->4,"4"->5,"5"->7,"6"->9,"7"->11|>;
LaunchKernels[4];
debug=False;


qysPlay[filename_]:=Module[
	{
		i,j,k,
		data,data1,data2,data3,data4,join,char,
		music={},voicePart,instrument="Piano",
		tonality=0,beat=1,speed=88,volume=1,
		pitch,time,space,tercet=0,tercetTime,
		comment,match,timeDot,note,duration,
		lastPitch,extend,portamento,rate,
		tremolo,score,repeat
	},
	volume=1;
	data1=StringJoin/@Import[filename,"Table"];             (* delete the spacings *)
	data2=Select[data1,!StringContainsQ[#,"//"]&];          (* delete the comments *)
	data3=Cases[data2,Except[""]];                          (* delete the blank lines *)
	data4={};j=0;join=False;                                (* join multiple lines of music scores together *)
	Do[
		If[join,
			join=False;data4[[j]]=data4[[j]]<>data3[[i]],
			j++;AppendTo[data4,data3[[i]]]
		];
		If[StringTake[data3[[i]],{-1}]=="\\",join=True],
	{i,Length@data3}];
	data=StringDelete[#,"|"|"\\"]&/@data4;                 (* delete the joint marks and add a ending mark *)
	Do[
		j=1;
		space=True;
		portamento=False;
		tremolo=0;
		voicePart={};
		If[StringPosition[data[[i]],":"]=={},
			score=data[[i]],
			(* repeat *)
			repeat=Partition[Transpose[StringPosition[data[[i]],":"]][[1]],2];
			score=StringTake[data[[i]],repeat[[1,1]]-1];
			Do[
				score=score<>StringTake[data[[i]],{repeat[[j,1]]+1,repeat[[j,2]]-1}];
				score=score<>StringTake[data[[i]],{repeat[[j,1]]+1,repeat[[j,2]]-1}];
				score=score<>StringTake[data[[i]],{repeat[[j,2]]+1,repeat[[j+1,1]]-1}],
			{j,Length@repeat-1}];
			score=score<>StringTake[data[[i]],{repeat[[-1,1]]+1,repeat[[-1,2]]-1}];
			score=score<>StringTake[data[[i]],{repeat[[-1,1]]+1,repeat[[-1,2]]-1}];
			score=score<>StringTake[data[[i]],{repeat[[-1,2]]+1,StringLength@data[[i]]}];
		];		
		While[j<=StringLength[score],
			char=StringTake[score,{j}];
			Switch[char,
				"<",
					match=Select[Transpose[StringPosition[score,">"]][[1]],#>j&][[1]];
					comment=StringTake[score,{j+1,match-1}];
					Switch[StringTake[comment,{2}],
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
					match=Select[Transpose[StringPosition[score,")"]][[1]],#>j&][[1]];
					comment=StringTake[score,{j+1,match-2}];
					Switch[StringTake[score,{match-1}],
						"-",                       (* tercet *)
							tercet=ToExpression[comment];
							tercetTime=(2^Floor[Log2[tercet]])/tercet,
						"=",                       (* tremolo *)
							tremolo=ToExpression[comment]
					];
					j=match+1;
					Continue[],
				"{",                               (* instrument *)
					match=Select[Transpose[StringPosition[score,"}"]][[1]],#>j&][[1]];
					instrument=StringTake[score,{j+1,match-1}];
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
					pitch={};k=0;                           (* harmony *)
					While[StringTake[score,{j}]!="]",
						char=StringTake[score,{j}];
						Switch[char,
							"#",pitch[[k]]++,
							"b",pitch[[k]]--,
							"'",pitch[[k]]+=12,
							",",pitch[[k]]-=12,
							_,
								k++;
								AppendTo[pitch,pitchDict[[ToExpression[char]]]+tonality];
						];
						j++;
					];
					j++;
				];
				While[j<=StringLength[score] && MemberQ[{"#","b","'",","},StringTake[score,{j}]],
					char=StringTake[score,{j}];
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
			While[j<=StringLength[score]&&MemberQ[{"-","_",".","^"},StringTake[score,{j}]],
				char=StringTake[score,{j}];
				Switch[char,
					"-",time+=1,
					"_",time/=2,
					".",
						timeDot=1/2;
						While[j<=StringLength[score] && StringTake[score,{j+1}]==".",
							timeDot/=2;
							j++;
						];
						time*=(2-timeDot),
					"^",space=False
				];
				j++;
			];
			If[tercet>0,time*=tercetTime;tercet--];
			duration=60/speed*time*beat;
			If[tremolo!=0,
				duration/=(time*2^tremolo);
				voicePart=Drop[voicePart,-2];
				Do[
					AppendTo[voicePart,{lastPitch,duration,instrument,SoundVolume->volume}];
					AppendTo[voicePart,{pitch,duration,instrument,SoundVolume->volume}],
				{k,time*2^(tremolo-1)}];
				tremolo=0;
				Continue[];
			]
			If[portamento,
				rate=(pitch-lastPitch+1)/time/6;
				duration/=(time*6);
				voicePart=Drop[voicePart,-2];
				Do[
					AppendTo[voicePart,{Floor[k],duration,instrument,SoundVolume->volume}],
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
					AppendTo[voicePart,{pitch,duration*7/8,instrument,SoundVolume->volume}];
					AppendTo[voicePart,{None,duration/8}],
					AppendTo[voicePart,{pitch,duration,instrument,SoundVolume->volume}];
				];
			];
		];
		If[voicePart!={},AppendTo[music,Sound[SoundNote@@#&/@voicePart]]],
	{i,Length[data]}];
	If[debug,Print[music],Parallelize[Map[EmitSound,music]]];
];


(* ::Input:: *)
(*debug=False;*)
(*qysPlay["E:\\QingyunMusicPlayer\\Songs\\Necro_Fantasia.qys"]*)


(* ::Input:: *)
(*debug=True;*)
(*qysPlay["E:\\QingyunMusicPlayer\\Songs\\Necro_Fantasia.qys"]*)



