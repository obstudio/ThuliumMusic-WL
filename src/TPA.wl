(* ::Package:: *)

(* ::Input:: *)
(*DeleteObject[JS];*)


kernelPath=StringReplace[NotebookDirectory[],"\\"->"/"];
temp[]:=kernelPath<>"tmp$"<>ToString[Floor@SessionTime[]]<>".json";
Get[kernelPath<>"Tokenizer.wl"];
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
4. nonzero number {m,n}: parse from mth section to nth section.";

parse::invspec = "Part specification `1` is invalid.";
parse::nosect = "No section was found through part specification `1`.";

parse[filepath_]:=parse[filepath,{1,-1}];
parse[filepath_,partspec_]:=Block[
	{
		tempFile=temp[],
		startSect,endSect,
		rawData,sectCount,
		data={},abspec
	},
	Export[tempFile,tokenize[filepath][["Tokenizer"]]];
	rawData=ExternalEvaluate[JS,"parse('"<>tempFile<>"')"];
	DeleteFile[tempFile];
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
	If[startSect<=endSect,
		Echo@{startSect,endSect},
		Message[parse::nosect,partspec];
		Return[];
	];
	
];

End[];
