(* ::Package:: *)

(* ::Text:: *)
(*Qingyun Music Player*)
(*Version 1.0.0*)
(**)
(*Usage:*)
(*QingyunPlay[song]*)
(**)


(* Initialization *)
$favorite=NotebookDirectory[];
$TonalityDict=<|
	"C"->0,"G"->7,"D"->2,"A"->-3,"E"->4,
	"B"->-1,"#F"->6,"#C"->1,"F"->5,"bB"->-2,
	"bE"->3,"bA"->-4,"bD"->1,"bG"->6,"bC"->-1
|>;


setFavorite[name_]:=If[ToLowerCase@name=="default",
	$favorite=NotebookDirectory[],
	$favorite=name
];
QingyunPlay[song_]:=Module[{filename},
	filename=$favorite<>song<>".qym";
	If[FileExistsQ[filename],
		qymPlay[filename],
		Print["Not Found!"];Return[];
	]
];


qymPlay[filename_]:=Module[
	{
		i,j,
		file,
		char,
		tonality=0,beat=1,speed=88,
		pitch,sharp=0,time,space,
		comment,match
	},
	file=Import[filename,"Table"];
	Do[
		j=1;
		While[j<=StringLength[file[[i,1]]],
			char=StringTake[file[[i,1]],{j}];
			Switch[char,
				"/",
					If[StringTake[file[[i,1]],{j+1}]=="/",Break[]],
				"#",
					sharp++;
					j++;
					Continue[],
				"b",
					sharp--;
					j++;
					Continue[],
				"<",
					match=Select[Transpose[StringPosition[file[[i,1]],">"]][[1]],#>j&][[1]]-1;
					comment=StringTake[file[[i,1]],{j+1,match}];
					Switch[StringTake[comment,{2}],
						"=",
							tonality=$TonalityDict[[StringTake[comment,{3,StringLength@comment}]]],
						"/",
							beat=ToExpression[StringTake[comment,{3}]]/4,
						_,
							speed=ToExpression[comment];
					];
					j=match;
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
				While[j<=StringLength[file[[i,1]]] && MemberQ[{"-","_","'",",",".","^"},StringTake[file[[i,1]],{j}]],
					char=StringTake[file[[i,1]],{j}];
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
				],
			j++];
		],
	{i,Length[file]}];
]


(* ::Input:: *)
(*QingyunPlay["The_Internationale"]*)
