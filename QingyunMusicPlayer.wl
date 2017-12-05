(* ::Package:: *)

(* ::Text:: *)
(*Qingyun Music Player*)
(*Version 1.0.0*)


(* Initialization *)
$CharacterEncoding="UTF-8";
$Language="ChineseSimplified";
$favorite=NotebookDirectory[];
$TonalityDict=<|
	"C"->0,"G"->7,"D"->2,"A"->-3,"E"->4,
	"B"->-1,"#F"->6,"#C"->1,"F"->5,"bB"->-2,
	"bE"->3,"bA"->-4,"bD"->1,"bG"->6,"bC"->-1
|>;
index=Import[$favorite<>"Index.xml","CDATA"];
$songCount=Length@index/2;
$songTitle=Take[index,{1,Length@index,2}];
$songPath=Take[index,{2,Length@index,2}];
$songDict=Association[#->$songTitle[[#]]&/@Range[$songCount]];
$playing="Null";


QingyunPlay[song_]:=Module[{filename},
	filename=$favorite<>"Songs\\"<>song;
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
					match=Select[Transpose[StringPosition[file[[i,1]],">"]][[1]],#>j&][[1]];
					comment=StringTake[file[[i,1]],{j+1,match-1}];
					Switch[StringTake[comment,{2}],
						"=",
							tonality=$TonalityDict[[StringTake[comment,{3,StringLength@comment}]]],
						"/",
							beat=ToExpression[StringTake[comment,{3}]]/4,
						_,
							speed=ToExpression[comment];
					];
					j=match+1;
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
						".",
							If[StringTake[file[[i,1]],{j+1}]==".",
								time*=(7/4),
								time*=(3/2)
							],
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


CreateDialog[Column[{
	Style["\:9752\:4e91\:64ad\:653e\:5668",Bold,20],
	SetterBar[Dynamic[choice],$songDict,Appearance->"Vertical"],
	Button["\:64ad\:653e",
		$playing=$songTitle[[choice]];
		QingyunPlay[$songPath[[choice]]];
		$playing="Null",
	ImageSize->150],
	If[$playing=="Null",
		"\:70b9\:51fb\[OpenCurlyDoubleQuote]\:64ad\:653e\[CloseCurlyDoubleQuote]\:6309\:94ae\:5f00\:59cb\:6f14\:594f\:3002",
		"\:6b63\:5728\:64ad\:653e\:ff1a"<>Dynamic[$playing]
	]
},Center],
WindowTitle->"\:9752\:4e91\:64ad\:653e\:5668"];
