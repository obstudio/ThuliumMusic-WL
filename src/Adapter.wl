(* ::Package:: *)

temp[]


(* ::InheritFromParent:: *)
(*"C:/Users/shigma/AppData/Local/obstudio/QYMP/tmp$272.json"//SystemOpen*)


temp[]:=userPath<>"tmp$"<>ToString[Floor@SessionTime[]]<>".json";
MIDIStop=Sound`StopMIDI;

Parse::usage = "\!\(\*RowBox[{\"Parse\",\"[\",RowBox[{
StyleBox[\"filepath\",\"TI\"],\",\",StyleBox[\"partspec\",\"TI\"]
}],\"]\"}]\)\n\!\(\*RowBox[{\"Parse\",\"[\",RowBox[{
StyleBox[\"tokenizer\",\"TI\"],\",\",StyleBox[\"partspec\",\"TI\"]
}],\"]\"}]\)

Valid file format include:
1. SML file: tokenize and parse the SML file.
2. JSON file: parse the tokenized SML file.

Valid part specification include: 
1. positive number n: parse the first n sections.
2. negative number -n: parse the last n sections.
3. nonzero number {n}: parse the nth section.
4. nonzero number {m,n}: parse from mth section to nth section.
The default value of partspec is {1,-1}.";

Parse::nfound = "Cannot find file `1`.";
Parse::suffix = "Cannot Parse file with suffix `1`.";
Parse::nsuffix = "Cannot Parse file with no suffix.";
Parse::failure = "A failure occured in the process.";
Parse::invspec = "Part specification `1` is invalid.";
Parse::nosect = "No section was found through part specification `1`.";

Parse[filepath_]:=Parse[filepath,{1,-1}];
Parse[filepath_,partspec_]:=Block[
	{
		tempFile,rawData,
		sectCount,abspec,
		startSect,endSect
	},
	Switch[filepath,
		_?(!FileExistsQ@#&),                               (* file not found *)
			Message[Parse::nfound,filepath];Return[],
		_?(StringEndsQ[".json"]),                           (* json *)
			rawData=ExternalEvaluate[JS,"Parse('"<>filepath<>"')"],
		_?(StringEndsQ[".sml"]),                            (* sml *)
			tempFile=temp[];
			Export[tempFile,Tokenize[filepath][["Tokenizer"]]];
			rawData=ExternalEvaluate[JS,"Parse('"<>tempFile<>"')"];
			DeleteFile[tempFile],
		_?(StringEndsQ["."~~WordCharacter..]),              (* other files *)
			Message[Parse::suffix,
				StringCases[filepath,"."~~sfx:WordCharacter..:>sfx][[-1]]
			];Return[],
		_,
			Message[Parse::nsuffix];Return[];
	];
	If[FailureQ[rawData],
		Echo[rawData];
		Message[Parse::failure];
		Return[];
	];
	sectCount=Length@rawData;
	abspec=If[#<0,sectCount+1+#,#]&;
    Switch[partspec,
        {_Integer,_Integer},
            {startSect,endSect}=abspec/@partspec,
        {_Integer},
            startSect=endSect=abspec@@partspec,
        _?(Positive[#]&&IntegerQ[#]&),
            startSect=1;endSect=partspec,
        _?(Negative[#]&&IntegerQ[#]&),
            startSect=sectCount+1+partspec;endSect=sectCount,
        _,
            Message[Parse::invspec,partspec]
    ];
    If[startSect>endSect,
		Message[Parse::nosect,partspec];Return[],
		Return[rawData[[startSect;;endSect]]];
    ];
	
];

MIDIAdapt[rawData_]:=Block[
    {
		duration=0,
		musicClip={}
    },
	If[!ListQ[rawData],Return[]];
	Do[
		AppendTo[musicClip,Table[
			If[MemberQ[instrData[["Style"]],trackData[["Instrument"]]],
				SoundNote[
					#Pitch,
					duration+#StartTime+{0,#Duration},
					trackData[["Instrument"]],
					SoundVolume->#Volume
				],
				SoundNote[
					trackData[["Instrument"]],
					duration+#StartTime+{0,#Duration},
					SoundVolume->#Volume
				]
			]&/@trackData[["Content"]],
		{trackData,sectionData[["Tracks"]]}]];
		duration+=Max[sectionData[["Tracks",All,"Meta","Duration"]]],
	{sectionData,rawData}];
	Return[Sound@Flatten@musicClip];
];

AudioAdapt[rawData_]:=Block[
    {
		duration=0,groups,
		musicClips={},targetClip,
		compactData,clipUsed
    },
	If[!ListQ[rawData],Return[]];
	Do[
		clipUsed={};
		groups=GatherBy[sectionData[["Tracks"]],#Meta[[{"FadeIn","FadeOut"}]]&];
		Do[
			compactData=Flatten@Table[
				If[#Pitch!=Null,SoundNote[
					#Pitch,
					duration+#StartTime+{0,#Duration},
					trackData[["Instrument"]],
					SoundVolume->#Volume
				],SoundNote[
					trackData[["Instrument"]],
					duration+#StartTime+{0,#Duration},
					SoundVolume->#Volume
				]]&/@trackData[["Content"]],
			{trackData,group}];
			targetClip=If[group[[1,"Meta","FadeIn"]]==0,
				LengthWhile[Range@Length@musicClips,Or[
					musicClips[[#,"FadeOut"]]>0,
					MemberQ[clipUsed,#]
				]&]+1,
				Length@musicClips+1
			];
			AppendTo[clipUsed,targetClip];
			If[targetClip>Length@musicClips,
				AppendTo[musicClips,<|
					"FadeIn"->group[[1,"Meta","FadeIn"]],
					"FadeOut"->group[[1,"Meta","FadeOut"]],
					"Events"->{compactData}
				|>],
				musicClips[[targetClip,"FadeOut"]]=group[[1,"Meta","FadeOut"]];
				AppendTo[musicClips[[targetClip,"Events"]],compactData];
			],
		{group,groups}];
		duration+=Max[sectionData[["Tracks",All,"Meta","Duration"]]],
	{sectionData,rawData}];
	Return[Total[Table[
		AudioFade[
			Sound@Flatten@musicClip[["Events"]],
		{musicClip[["FadeIn"]],musicClip[["FadeOut"]]}],
	{musicClip,musicClips}]]];
];



(* ::Input:: *)
(*DeleteObject[JS];*)


(* ::Input:: *)
(*data=Parse[localPath<>"src/test/test10.sml"];*)


(* ::Input:: *)
(*MIDIAdapt[data]*)


(* ::Subsubsection:: *)
(*MIDI*)


(* ::Input:: *)
(*MIDIStop[];*)


(* ::Input:: *)
(*MIDIStop[];EmitSound[#[[2]]]&@*)
(*EchoFunction["time: ",#[[1]]&]@*)
(*Timing[MIDIAdapt[Parse[localPath<>"src/test/test6.sml"]]];*)


(* ::Subsubsection:: *)
(*Audio*)


(* ::Input:: *)
(*AudioStop[];*)


(* ::Input:: *)
(*AudioStop[];AudioPlay[#[[2]]]&@*)
(*EchoFunction["time: ",#[[1]]&]@*)
(*Timing[AudioAdapt[Parse[localPath<>"src/test/test10.sml"]]];*)


(* ::Subsubsection:: *)
(*Test output*)


(* ::Input:: *)
(*testfile=localPath<>"src/test/test7.";*)
(*testjson=tokenize[testfile<>"sml"][["Tokenizer"]];*)
(*(*Export[testfile<>"tokenizer.json",testjson];*)*)
(*testdata=Parse[testfile<>"json"];*)
(*Export[testfile<>"output.mid",MIDIAdapt[testdata]];*)
(*(*Export[testfile<>"output.mp3",AudioAdapt[testdata]];*)*)
