(* ::Package:: *)

tonalityDict=<|
	"C"->0,"G"->7,"D"->2,"A"->-3,"E"->4,
	"B"->-1,"#F"->6,"#C"->1,"F"->5,"bB"->-2,
	"bE"->3,"bA"->-4,"bD"->1,"bG"->6,"bC"->-1
|>;
pitchDict=<|"1"->0,"2"->2,"3"->4,"4"->5,"5"->7,"6"->9,"7"->11|>;
LaunchKernels[4];


qysPlay[filename_]:=Module[
	{
		i,j,k,
		data,data1,data2,data3,data4,join,char,
		music={},voicePart,instrument="Piano",
		tonality=0,beat=1,speed=88,volume=1,
		pitch,time,space,tercet=0,tercetTime,
		comment,match,timeDot,note,duration
	},
	volume=1;
	data1=StringJoin/@Import[filename,"Table"];             (* delete the spacings *)
	data2=Select[data1,!StringContainsQ[#,"//"]&];         (* delete the comments *)
	data3={};j=0;join=False;                                (* join multiple lines of music scores together *)
	Do[
		If[join,
			join=False;data3[[j]]=data3[[j]]<>data2[[i]],
			j++;AppendTo[data3,data2[[i]]]
		];
		If[StringTake[data2[[i]],{-1}]=="\\",join=True],
	{i,Length@data2}];
	data=StringDelete[#,"|"|"\\"]&/@data3;                 (* delete the joint marks and add a ending mark *)
	Do[
		j=1;
		voicePart={};
		While[j<=StringLength[data[[i]]],
			char=StringTake[data[[i]],{j}];
			Switch[char,
				"<",
					match=Select[Transpose[StringPosition[data[[i]],">"]][[1]],#>j&][[1]];
					comment=StringTake[data[[i]],{j+1,match-1}];
					Switch[StringTake[comment,{2}],
						"=",
							tonality=tonalityDict[[StringTake[comment,{3,StringLength@comment}]]],
						"/",
							beat=ToExpression[StringTake[comment,{3}]]/4,
						".",
							volume=ToExpression[comment],
						_,
							speed=ToExpression[comment];
					];
					j=match+1;
					Continue[],
				"(",
					match=Select[Transpose[StringPosition[data[[i]],")"]][[1]],#>j&][[1]];
					comment=StringTake[data[[i]],{j+1,match-1}];
					tercet=ToExpression[comment];
					tercetTime=(2^Floor[Log2[tercet]])/tercet;
					j=match+1;
					Continue[],
				"{",
					match=Select[Transpose[StringPosition[data[[i]],"}"]][[1]],#>j&][[1]];
					instrument=StringTake[data[[i]],{j+1,match-1}];
					j=match+1;
					Continue[],
				_,
					If[DigitQ[char],
						(* single-tone *)
						note=ToExpression@char;
						pitch=If[note==0,None,pitchDict[[note]]+tonality];
						j++;
						If[StringTake[data[[i]],{j}]=="#",pitch++;j++];
						If[StringTake[data[[i]],{j}]=="b",pitch--;j++],
						(* harmony *)
						pitch={};
						j++;
						k=0;
						While[StringTake[data[[i]],{j}]!="]",
							char=StringTake[data[[i]],{j}];
							Switch[char,
								"#",pitch[[k]]++,
								"b",pitch[[k]]--,
								"'",pitch+=12,
								",",pitch-=12,
								_,
									k++;
									AppendTo[pitch,pitchDict[[ToExpression[char]]]+tonality];
							];
							j++;
						];
						j++;
					];
					time=1;
					space=True;					
					While[j<=StringLength[data[[i]]]&&MemberQ[{"-","_","'",",",".","^"},StringTake[data[[i]],{j}]],
						char=StringTake[data[[i]],{j}];
						Switch[char,
							"-",time+=1,
							"_",time/=2,
							"'",pitch+=12,
							",",pitch-=12,
							".",
								timeDot=1/2;
								While[j<=StringLength[data[[i]]] && StringTake[data[[i]],{j+1}]==".",
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
					If[space,
						AppendTo[voicePart,SoundNote[pitch,duration*7/8,instrument,SoundVolume->volume]];
						AppendTo[voicePart,SoundNote[None,duration/8]],
						AppendTo[voicePart,SoundNote[pitch,duration,instrument,SoundVolume->volume]];
					];
			];
		];
		If[voicePart!={},AppendTo[music,Sound@voicePart]],
	{i,Length[data]}];
	Parallelize[Map[EmitSound,music]];
];


(* ::Input:: *)
(*qysPlay["C:\\Users\\kouch\\Documents\\QingyunMusicPlayer-WL\\Songs\\Sumizome_Sakura.qys"]*)


EmitSound@Sound@SoundNote["C",.5,"Tuba"]



