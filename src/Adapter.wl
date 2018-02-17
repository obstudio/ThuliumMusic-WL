(* ::Package:: *)

(* ::Input:: *)
(*DeleteObject[JS];*)


kernelPath=StringReplace[NotebookDirectory[],"\\"->"/"];
temp[]:=kernelPath<>"tmp$"<>ToString[Floor@SessionTime[]]<>".json";
Get[kernelPath<>"Tokenizer.wl"];
MIDIStop=Sound`StopMIDI;
JS=StartExternalSession["NodeJS"];
ExternalEvaluate[JS,File[kernelPath<>"SMML.js"]]
ExternalEvaluate[JS,"const fs = require('fs')"]
ExternalEvaluate[JS,"
	function parse(filePath) {
	    const data = JSON.parse(fs.readFileSync(filePath, 'utf8'))
	    return new SMML.Parser(data).parse()
	}
"]


Begin["SMML`"];

parse::usage = "parse [filepath, partspec]\n
Valid part specification include: 
1. positive number n: parse the first n sections.
2. negative number -n: parse the last n sections.
3. nonzero number {n}: parse the nth section.
4. nonzero number {m,n}: parse from mth section to nth section.
The default value of partspec is {1,-1}.";

parse::nfound = "Cannot find file `1`.";
parse::suffix = "Cannot parse file with suffix `1`.";
parse::nsuffix = "Cannot parse file with no suffix.";
parse::failure = "A failure occured in the process.";
parse::invspec = "Part specification `1` is invalid.";
parse::nosect = "No section was found through part specification `1`.";

parse[filepath_]:=parse[filepath,{1,-1}];
parse[filepath_,partspec_]:=Block[
	{
		tempFile,rawData,
		sectCount,abspec,
		startSect,endSect
	},
	Switch[filepath,
		_?(!FileExistsQ@#&),                               (* file not found *)
			Message[parse::nfound,filepath];Return[],
		_?(StringEndsQ[".json"]),                           (* json *)
			rawData=ExternalEvaluate[JS,"parse('"<>filepath<>"')"],
		_?(StringEndsQ[".sml"]),                            (* sml *)
			tempFile=temp[];
			Export[tempFile,tokenize[filepath][["Tokenizer"]]];
			rawData=ExternalEvaluate[JS,"parse('"<>tempFile<>"')"];
			DeleteFile[tempFile],
		_?(StringEndsQ["."~~WordCharacter..]),              (* other files *)
			Message[parse::suffix,
				StringCases[filepath,"."~~sfx:WordCharacter..:>sfx][[-1]]
			];Return[],
		_,
			Message[parse::nsuffix];Return[];
	];
	If[FailureQ[rawData],
		Message[parse::failure];
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
            Message[parse::invspec,partspec]
    ];
    If[startSect>endSect,
		Message[parse::nosect,partspec];Return[],
		Return[rawData[[startSect;;endSect]]];
    ];
	
];

midiAdapt[rawData_]:=Block[
    {
		duration=0,
		musicClip={}
    },
	If[!ListQ[rawData],Return[]];
	Do[
		AppendTo[musicClip,Table[
			SoundNote[
				#Pitch,
				duration+#StartTime+{0,#Duration},
				trackData[["Instrument"]],
				SoundVolume->#Volume
			]&/@trackData[["Content"]],
		{trackData,sectionData[["Tracks"]]}]];
		duration+=Max[sectionData[["Tracks",All,"Meta","Duration"]]],
	{sectionData,rawData}];
	Return[Sound@Flatten@musicClip];
];

audioAdapt[rawData_]:=Block[
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
				SoundNote[
					#Pitch,
					duration+#StartTime+{0,#Duration},
					trackData[["Instrument"]],
					SoundVolume->#Volume
				]&/@trackData[["Content"]],
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

End[];


(* ::Input:: *)
(*data=parse[kernelPath<>"test/test6.sml"];*)


(* ::Input:: *)
(*audioAdapt[data]*)


(* ::Subsubsection:: *)
(*MIDI*)


(* ::Input:: *)
(*MIDIStop[];*)


(* ::Input:: *)
(*MIDIStop[];EmitSound[#[[2]]]&@*)
(*EchoFunction["time: ",#[[1]]&]@*)
(*Timing[midiAdapt[parse[kernelPath<>"test/test3.sml"]]];*)


(* ::Subsubsection:: *)
(*Audio*)


(* ::Input:: *)
(*AudioStop[];*)


(* ::Input:: *)
(*AudioStop[];AudioPlay[#[[2]]]&@*)
(*EchoFunction["time: ",#[[1]]&]@*)
(*Timing[audioAdapt[parse[kernelPath<>"test/test6.sml"]]];*)


(* ::Subsubsection:: *)
(*Test output*)


(* ::Input:: *)
(*testfile=kernelPath<>"test/test6.";*)
(*testjson=tokenize[testfile<>"sml"][["Tokenizer"]];*)
(*(*Export[testfile<>"tokenizer.json",testjson];*)*)
(*testdata=parse[testfile<>"json"];*)
(*Export[testfile<>"output.mid",midiAdapt[testdata]];*)
(*(*Export[testfile<>"output.mp3",audioAdapt[testdata]];*)*)
