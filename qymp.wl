(* ::Package:: *)

(* ::Text:: *)
(*Qingyun Music Player*)
(*Version 1.0.0*)
(**)
(*Usage:*)
(*QingyunPlay[filename]*)
(*filename: Path to the .qym file you want to play*)
(**)


QingyunPlay[filename_]:=Module[
	{
		i,j,
		file,char,
		speed=88,tonality=0,beat=1,
		pitch,sharp=0,time,space
	},
	file=Import[filename,"Table"];
	Do[
		j=1;
		While[j<=StringLength[file[[i]][[1]]],
			char=StringTake[file[[i]][[1]],{j}];
			Switch[char,
				"/",
					If[StringTake[file[[i]][[1]],{j+1}]=="/",
						Break[];
					],
				"#",
					sharp++;
					j++;
					Continue[],
				"b",
					sharp--;
					j++;
					Continue[],
				"<",
					If[StringTake[file[[i]][[1]],{j+1,j+2}]=="1=",
						If[StringTake[file[[i]][[1]],{j+4}]==">",
							char=StringTake[file[[i]][[1]],{j+3}];
							j+=4,
							char=StringTake[file[[i]][[1]],{j+3,j+4}];
							j+=5
						];
						Switch[char,
							"C",tonality=0,
							"G",tonality=7,
							"D",tonality=2,
							"A",tonality=-3,
							"E",tonality=4,
							"B",tonality=-1,
							"#F",tonality=6,
							"#C",tonality=1,
							"F",tonality=5,
							"bB",tonality=-2,
							"bE",tonality=3,
							"bA",tonality=-4,
							"bD",tonality=1,
							"bG",tonality=6,
							"bC",tonality=-1
						],
						If[StringTake[file[[i]][[1]],{j+2}]=="/",
							beat=ToExpression[StringTake[file[[i]][[1]],{j+3}]]/4;
							j+=4;
						];
					];
					j++;
					Continue[];
			];
			If[MemberQ[{"0","1","2","3","4","5","6","7"},char],
				time=1;
				space=True;
				pitch=440*2^((tonality+sharp)/12);
				sharp=0;
				Switch[char,
					"0",pitch*=0,
					"1",pitch*=2^(-9/12),
					"2",pitch*=2^(-7/12),
					"3",pitch*=2^(-5/12),
					"4",pitch*=2^(-4/12),
					"5",pitch*=2^(-2/12),
					"6",pitch*=2^(0/12),
					"7",pitch*=2^(2/12)
				];
				j++;
				While[j<=StringLength[file[[i]][[1]]] && MemberQ[{"-","_","'",",",".","^"},StringTake[file[[i]][[1]],{j}]],
					char=StringTake[file[[i]][[1]],{j}];
					Switch[char,
						"-",time+=1,
						"_",time/=2,
						"'",pitch*=2,
						",",pitch/=2,
						".",time*=(3/2),
						"^",space=False
					];
					j++;
				];
				time=60/speed*time*beat;
				If[space,
					EmitSound[Play[Sin[pitch*2*Pi*t],{t,0,time*7/8}]];
					EmitSound[Play[0,{t,0,time/8}]],
					EmitSound[Play[Sin[pitch*2*Pi*t],{t,0,time}]];
				]
				
			,j++];
		];
	,{i,Length[file]}];
]


QingyunPlay[NotebookDirectory[]<>"The_Internationale.qym"]
