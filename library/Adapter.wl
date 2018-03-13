(* ::Package:: *)

(* ::Subsubsection::Closed:: *)
(*Parser*)


TempPath[]:=userPath<>"tmp$"<>ToString[Floor@SessionTime[]]<>".json";
MIDIStop=Sound`StopMIDI;
MIDIPlay=Sound`EmitMIDI;

Parse::usage = "\!\(\*RowBox[{\"Parse\",\"[\",RowBox[{
StyleBox[\"filepath\",\"TI\"],\",\",StyleBox[\"partspec\",\"TI\"]
}],\"]\"}]\)\n\!\(\*RowBox[{\"Parse\",\"[\",RowBox[{
StyleBox[\"tokenizer\",\"TI\"],\",\",StyleBox[\"partspec\",\"TI\"]
}],\"]\"}]\)

Valid file format include:
1. SML file: tokenize and parse the SML file.
2. JSON file: parse the tokenized SML file.

Valid part specification include: 
1. Positive number \!\(\*StyleBox[\"n\",\"TI\"]\): \
parse the first \!\(\*StyleBox[\"n\",\"TI\"]\) sections.
2. Negative number -\!\(\*StyleBox[\"n\",\"TI\"]\): \
parse the last \!\(\*StyleBox[\"n\",\"TI\"]\) sections.
3. Nonzero number \!\(\*RowBox[{\"{\",StyleBox[\"n\",\"TI\"],\"}\"}]\): \
parse the \!\(\*StyleBox[\"n\",\"TI\"]\)\!\(\*SuperscriptBox[\"\[Null]\", \"th\"]\) section.
4. Nonzero number \!\(\*RowBox[{\"{\",StyleBox[\"m\",\"TI\"],\",\",StyleBox[\"n\",\"TI\"],\"}\"}]\): \
parse from \!\(\*StyleBox[\"m\",\"TI\"]\)\!\(\*SuperscriptBox[\"\[Null]\", \"th\"]\) section \
to \!\(\*StyleBox[\"n\",\"TI\"]\)\!\(\*SuperscriptBox[\"\[Null]\", \"th\"]\) section.
The default value of partspec is \!\(\*RowBox[{\"{\",\"1\",\",\",\"-1\",\"}\"}]\).";

Parse::type = "The first argument should be a filename or a tokenizer.";
Parse::nfound = "Cannot find file `1`.";
Parse::ext1 = "Cannot Parse file with extension `1`.";
Parse::ext2 = "Cannot Parse file with no extension.";
Parse::failure = "A failure occured in the process.";
Parse::invspec = "Part specification `1` is invalid.";
Parse::nosect = "No section was found through part specification `1`.";

Parse[origin_]:=Parse[origin,{1,-1}];
Parse[origin_,partspec_]:=Block[
	{
		filepath,tempFile,rawData,
		sectCount,abspec,startSect,endSect
	},
	
	Switch[origin,
		_String,
			If[!FileExistsQ[origin],
				Message[Parse::nfound,origin];Return[];
			];
			Switch[ToLowerCase@FileExtension[origin],
				"json",
					rawData=ExternalEvaluate[System`JS,"Parse('"<>origin<>"')"],
				"tm",
					tempFile=TempPath[];
					Export[tempFile,Tokenize[origin][["Tokenizer"]]];
					rawData=ExternalEvaluate[System`JS,"Parse('"<>tempFile<>"')"];
					DeleteFile[tempFile],
				"",
					Message[Parse::ext2];Return[],
				_,
					Message[Parse::ext1,FileExtension[origin]];Return[];
			],
		_Association,
			tempFile=TempPath[];
			Export[tempFile,origin];
			rawData=ExternalEvaluate[System`JS,"Parse('"<>tempFile<>"')"];
			DeleteFile[tempFile],
		_,
			Message[Parse::type];Return[];
	];
	
	If[FailureQ[rawData],
		Message[Parse::failure];
		Echo[Level[Level[rawData,1][[2,"StackTrace"]],1][[1]]];
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


(* ::Subsubsection::Closed:: *)
(*Constructor*)


EventConstruct[trackData_,startTime_]:=If[MemberQ[instList,trackData[["Instrument"]]],
	<|
		"Inst"->instDict[[trackData[["Instrument"]]]],
		"Note"->#Pitch+60,
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
	Sound`MIDIEvent[0,"ProgramCommand","Channel"->chan,"Value"->If[inst==128,0,inst]],
	Sequence@@(Sound`MIDIEvent[
		Floor[#Time],
		If[#Event==1,"NoteOn","NoteOff"],
		"Note"->#Note,
		"Channel"->chan,
		"Velocity"->#Velocity
	]&/@SortBy[events,{#Time&,#Event&}])
}];

$Resolution = 48;
MIDIConstruct::channel = "The amount of channels exceeds 16. Some channels may be lost when exporting to a MIDI file.";
MIDIConstruct::instid = "The sound consists of instrument with ID exceeding 128, thus cannot be transferred to MIDI.";

MIDIConstruct[musicClip_,rate_]:=Block[
	{
		channelData,channelMap=<||>,
		duration
	},
	If[rate>0,
		channelData=GroupBy[musicClip,#Inst&->({
			<|"Time"->$Resolution*#Start/rate,"Note"->#Note,"Velocity"->Floor[127*#Vol],"Event"->1|>,
			<|"Time"->$Resolution*#End/rate,"Note"->#Note,"Velocity"->0,"Event"->0|>
		}&)],
		(* upend *)
		duration=Max[#End&/@musicClip];
		channelData=GroupBy[musicClip,#Inst&->({
			<|"Time"->$Resolution*(duration+#End/rate),"Note"->#Note,"Velocity"->Floor[127*#Vol],"Event"->1|>,
			<|"Time"->$Resolution*(duration+#Start/rate),"Note"->#Note,"Velocity"->0,"Event"->0|>
		}&)];
	];
	Do[
		If[instID==128,
			AppendTo[channelMap,instID->9],
			AppendTo[channelMap,instID->LengthWhile[
				Range[0,Length@channelData],MemberQ[Append[Values@channelMap,9],#]&
			]];
		],
	{instID,Keys@channelData}];	
	Return[Sound`MIDISequence[
		KeyValueMap[TrackConstruct[#1,#2,Flatten[channelData[[Key[#1]]],1]]&,channelMap],
		"DivisionType"->"PPQ",
		"Resolution"->$Resolution
	]];
];


(* ::Subsubsection::Closed:: *)
(*Adapter*)


AdaptingOptions = {"Rate"->1.0,"Format"->"Audio"};
Adapt::format = "`1` is not a adaptable format.";
Adapt::invrate = "`1` is not a valid rate specification.";
Adapt::usage = "\
\!\(\*RowBox[{\"Adapt\",\"[\",RowBox[{StyleBox[\"data\",\"TI\"],\",\",StyleBox[\"options\",\"TI\"]}],\"]\"}]\)\n
The contents of \!\(\*StyleBox[\"options\",\"TI\"]\) can be the following: 
\!\(\*RowBox[{\"\t\",\"   Rate \",\"\t\",\"\t\",\"       1.0\",\"\t\",\"\t\",\"     Speed rate\"}]\)
\!\(\*RowBox[{\"\t\",\"Format\",\"\t\",\"\t\",\"Audio\",\"\t\",\"\t\",\"Adapting format\"}]\)";

Adapt[rawData_,OptionsPattern[AdaptingOptions]]:=Switch[OptionValue["Format"],
	"MIDI",MIDIAdapt[rawData,OptionValue[Keys@AdaptingOptions]],
	"Audio",AudioAdapt[rawData,OptionValue[Keys@AdaptingOptions]],
	_,Message[Adapt::format,OptionValue["Format"]];Return[];
];

MIDIAdapt[rawData_,OptionsPattern[AdaptingOptions]]:=Block[
    {
		duration=0,musicClip={}
    },
	If[!ListQ[rawData],Return[]];
	If[!Through[(Positive||Negative)@OptionValue["Rate"]],
		Message[Adapt::invrate,OptionValue["Rate"]];Return[];
	];
	Do[
		AppendTo[musicClip,Table[
			EventConstruct[trackData,duration],
		{trackData,sectionData[["Tracks"]]}]];
		duration+=Max[sectionData[["Tracks",All,"Meta","Duration"]]],
	{sectionData,rawData}];
	Return[MIDIConstruct[Flatten@musicClip,OptionValue["Rate"]]];
];

AudioAdapt[rawData_,OptionsPattern[AdaptingOptions]]:=Block[
    {
		duration=0,groups,
		musicClips={},targetClip,
		compactData,clipUsed,
		output=0,clipCount,musicClip
    },
	If[!ListQ[rawData],Return[]];
	If[!Through[(Positive||Negative)@OptionValue["Rate"]],
		Message[Adapt::invrate,OptionValue["Rate"]];Return[];
	];
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
	
	output=Total@Table[
		AudioFade[
			Sound@MIDIConstruct[Flatten@musicClip[["Events"]],OptionValue["Rate"]],
			If[OptionValue["Rate"]>0,
				{musicClip[["FadeIn"]],musicClip[["FadeOut"]]}/OptionValue["Rate"],
				{musicClip[["FadeOut"]],musicClip[["FadeIn"]]}/(-OptionValue["Rate"])
			]
		],
	{musicClip,musicClips}];
	
	Return[output];
];


(* ::Subsubsection:: *)
(*Debug*)


(* ::Input:: *)
(*Parse[Tokenize[localPath<>"src/test/test.tm"][["Tokenizer"]]]*)


(* ::Input:: *)
(*With[{testfile=localPath<>"Songs/PVZ/Watery_Grave"},*)
(*Export[testfile<>".json",Parse[testfile<>".tm"]]*)
(*];*)


(* ::Input:: *)
(*With[{testfile=localPath<>"Songs/PVZ/Watery_Grave"},*)
(*Export[testfile<>".json",Tokenize[testfile<>".tm"][["Tokenizer"]]]*)
(*];*)


(* ::Input:: *)
(*Tokenize[localPath<>"Research/test.tm"][["Tokenizer"]]*)


(* ::Input:: *)
(*Parse[localPath<>"Research/test.tm"]*)


(* ::Input:: *)
(*data=Parse[localPath<>"Research/test.tm"];Diagnose[data]*)


(* ::Input:: *)
(*data=Parse[localPath<>"Songs/Touhou/TH11-Chireiden/3rd_Eye.tm"];*)
(*Diagnose[data]*)


(* ::Input:: *)
(*MIDIPlay@MIDIAdapt[Parse[localPath<>"src/test/test.tm"]]*)


(* ::Input:: *)
(*EmitSound@Sound@SoundNote[5,1,"ElectricBass"]*)


(* ::Input:: *)
(*EmitSound@Sound@SoundNote["VoiceAahs",1]*)


(* ::Subsubsection:: *)
(*MIDI*)


(* ::Input:: *)
(*MIDIStop[];*)


(* ::Input:: *)
(*MIDIStop[];MIDIPlay[#[[2]]]&@*)
(*EchoFunction["time: ",#[[1]]&]@*)
(*Timing[MIDIAdapt[Parse[localPath<>"Songs/Touhou/TH11-Chireiden/3rd_Eye.tm"],"Rate"->1.1]];*)


(* ::Input:: *)
(*MIDIStop[];MIDIPlay[#[[2]]]&@*)
(*EchoFunction["time: ",#[[1]]&]@*)
(*Timing[MIDIAdapt[Parse[localPath<>"Research/test.tm"]]];*)


(* ::Subsubsection:: *)
(*Audio*)


(* ::Input:: *)
(*AudioStop[];*)


(* ::Input:: *)
(*AudioStop[];AudioPlay[#[[2]]]&@*)
(*EchoFunction["time: ",#[[1]]&]@*)
(*Timing[AudioAdapt[Parse[localPath<>"Songs/Touhou/TH11-Chireiden/3rd_Eye.tm",{5,6}],"Rate"->1.1]];*)


(* ::Input:: *)
(*AudioStop[];AudioPlay[#[[2]]]&@*)
(*EchoFunction["time: ",#[[1]]&]@*)
(*Timing[AudioAdapt[Parse[localPath<>"Research/test.tm"]]];*)


(* ::Input:: *)
(*Export[localPath<>"test.mp3",AudioAdapt[Parse[localPath<>"Songs/Touhou/TH11-Chireiden/3rd_Eye.tm",1]]];*)


(* ::Subsubsection:: *)
(*Interpretation*)


(* ::Input:: *)
(*CurrentValue[{StyleDefinitions,"Quantity"}]*)


(* ::Input:: *)
(*CurrentValue[{StyleDefinitions,"QuantityPanel"}]*)
