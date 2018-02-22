(* ::Package:: *)

TempPath[]:=userPath<>"tmp$"<>ToString[Floor@SessionTime[]]<>".json";
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
1. positive number \!\(\*StyleBox[\"n\",\"TI\"]\): parse the first n sections.
2. negative number -n: parse the last n sections.
3. nonzero number {n}: parse the nth section.
4. nonzero number {m,n}: parse from mth section to nth section.
The default value of partspec is {1,-1}.";

Parse::nfound = "Cannot find file `1`.";
Parse::ext1 = "Cannot Parse file with extension `1`.";
Parse::ext2 = "Cannot Parse file with no extension.";
Parse::failure = "A failure occured in the process.";
Parse::invspec = "Part specification `1` is invalid.";
Parse::nosect = "No section was found through part specification `1`.";

Parse[filepath_String]:=Parse[filepath,{1,-1}];
Parse[filepath_String,partspec_]:=Block[
	{
		tempFile,rawData,
		sectCount,abspec,
		startSect,endSect
	},
	
	If[!FileExistsQ[filepath],
		Message[Parse::nfound,filepath];
		Return[];
	];
	
	Switch[ToLowerCase@FileExtension[filepath],
		"json",
			rawData=ExternalEvaluate[JS,"Parse('"<>filepath<>"')"],
		"sml",
			tempFile=TempPath[];
			Export[tempFile,Tokenize[filepath][["Tokenizer"]]];
			rawData=ExternalEvaluate[JS,"Parse('"<>tempFile<>"')"];
			DeleteFile[tempFile],
		"",
			Message[Parse::ext2];
			Return[],
		_,
			Message[Parse::ext1,FileExtension[filepath]];
			Return[];
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


EventConstruct[trackData_,startTime_]:=If[MemberQ[instList,trackData[["Instrument"]]],
	<|
		"Inst"->instDict[[trackData[["Instrument"]]]],
		"Note"->#Pitch,
		"Start"->startTime+#StartTime,
		"End"->startTime+#StartTime+#Duration,
		"Vol"->#Volume
	|>&,
	<|
		"Inst"->128,
		"Note"->percDict[[trackData[["Instrument"]]]],
		"Start"->startTime+#StartTime,
		"End"->startTime+#StartTime+#Duration,
		"Vol"->#Volume
	|>&
]/@trackData[["Content"]];

TrackConstruct[inst_,chan_,events_]:=Sound`MIDITrack[{
	Sound`MIDIEvent[0,"SetTempo","Tempo"->1000000],
	Sound`MIDIEvent[0,"ProgramCommand","Channel"->chan,"Value"->inst],
	Sequence@@MapThread[Sound`MIDIEvent[##,"Channel"->chan]&][Transpose[
		SortBy[events,{First,If[#[[2]]=="NoteOff",0,1]&}]
	]]
}];

$Resolution = 48;
$BaseLine = 60;
MIDIConstruct::channel = "The amount of channels exceeds 16. Some channels may be lost when exporting to a MIDI file.";
MIDIConstruct::instid = "The sound consists of instrument with ID exceeding 128, thus cannot be transferred to MIDI.";

MIDIConstruct[musicClip_]:=Block[
	{
		channelData,channelMap=<||>
	},
	channelData=GroupBy[musicClip,#Inst&->({
		{Floor[$Resolution*#Start],"NoteOn","Note"->#Note+$BaseLine,"Velocity"->Floor[127*#Vol]},
		{Floor[$Resolution*#End],"NoteOff","Note"->#Note+$BaseLine,"Velocity"->0}
	}&)];
	Do[
		If[instID==128,
			AppendTo[channelMap,instID->9],
			AppendTo[channelMap,instID->LengthWhile[
				Range[0,Length@channelData],MemberQ[Values@channelMap,#]&
			]];
		],
	{instID,Keys@channelData}];	
	Return[Sound@Sound`MIDISequence[
		KeyValueMap[TrackConstruct[#1,#2,Flatten[channelData[[Key[#1]]],1]]&,channelMap],
		"DivisionType"->"PPQ",
		"Resolution"->$Resolution
	]];
];


MIDIAdapt[rawData_]:=Block[
    {
		duration=0,musicClip={}
    },
	If[!ListQ[rawData],Return[]];
	Do[
		AppendTo[musicClip,Table[
			EventConstruct[trackData,duration],
		{trackData,sectionData[["Tracks"]]}]];
		duration+=Max[sectionData[["Tracks",All,"Meta","Duration"]]],
	{sectionData,rawData}];
	Return[MIDIConstruct@Flatten@musicClip];
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
				EventConstruct[trackData,duration],
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
			MIDIConstruct@Flatten@musicClip[["Events"]],
		{musicClip[["FadeIn"]],musicClip[["FadeOut"]]}],
	{musicClip,musicClips}]]];
];


(* ::Input:: *)
(*DeleteObject[JS];*)


(* ::Input:: *)
(*With[{testfile=localPath<>"src/test/test"},*)
(*Export[testfile<>".json",Tokenize[testfile<>".sml"][["Tokenizer"]]]*)
(*];*)


(* ::Input:: *)
(*data=Parse[localPath<>"src/test/test.sml",1];*)


(* ::Input:: *)
(*MIDIAdapt[Parse[localPath<>"src/test/test.sml"]]*)


(* ::Input:: *)
(*EmitSound@Sound@SoundNote[-25,1,"SlapBass"]*)


(* ::Input:: *)
(*EmitSound@Sound@SoundNote["RideBell",1]*)


(* ::Subsubsection:: *)
(*MIDI*)


(* ::Input:: *)
(*MIDIStop[];*)


(* ::Input:: *)
(*MIDIStop[];EmitSound[#[[2]]]&@*)
(*EchoFunction["time: ",#[[1]]&]@*)
(*Timing[MIDIAdapt[Parse[localPath<>"Songs/Touhou/TH11-Chireiden/Heartfelt_Fancy.sml"]]];*)


(* ::Input:: *)
(*MIDIStop[];EmitSound[#[[2]]]&@*)
(*EchoFunction["time: ",#[[1]]&]@*)
(*Timing[MIDIAdapt[Parse[localPath<>"src/test/test.sml"]]];*)


(* ::Subsubsection:: *)
(*Audio*)


(* ::Input:: *)
(*AudioStop[];*)


(* ::Input:: *)
(*AudioStop[];AudioPlay[#[[2]]]&@*)
(*EchoFunction["time: ",#[[1]]&]@*)
(*Timing[AudioAdapt[Parse[localPath<>"src/test/test.sml"]]];*)


(* ::Subsubsection:: *)
(*Test output*)


(* ::Input:: *)
(*Export[localPath<>"src/test.mid",MIDIAdapt[Parse[localPath<>"Songs/Touhou/TH11-Chireiden/Heartfelt_Fancy.sml",2]]];*)


(* ::Input:: *)
(*Export[localPath<>"src/test.mid",Sound[SoundNote["BassDrum"]]];*)
