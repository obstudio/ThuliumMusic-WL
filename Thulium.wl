(* ::Package:: *)

(* ::Text:: *)
(*Thulium Music Player*)


localPath=NotebookDirectory[];
Needs["graphics`",localPath<>"dist\\Graphics.mx"];

<<(localPath<>"Assets\\assets.wl")         (* graphics *)
<<(localPath<>"Assets\\uiControls.wl")     (* controls *)
<<(localPath<>"Assets\\uiUser.wl")         (* UI for common users *)
<<(localPath<>"Assets\\uiDeveloper.wl")    (* UI for developers *)

<<(localPath<>"src\\library.wl")           (* library *)
<<(localPath<>"src\\Adapter.wl")           (* adapter *)
<<(localPath<>"src\\Syntax.wl")            (* syntax *)
<<(localPath<>"src\\Tokenizer.wl")         (* tokenizer *)
<<(localPath<>"ide\\Diagnostor.wl")        (* diagnoser *)

<<(localPath<>"package\\Standard\\.init.wl")


If[Length@FindExternalEvaluators["NodeJS"]==0,
	CreateDialog[{
		TextCell["Thulium Music Player requires Node.js as external evaluator. Please follow the guide to install Node.js and Zeromq first."],
		DefaultButton[]
	}];
	Abort[];
];
JS=StartExternalSession["NodeJS"];
ExternalEvaluate[JS,File[localPath<>"src\\SMML.js"]]
ExternalEvaluate[JS,"const fs = require('fs')"]
ExternalEvaluate[JS,"
	function Parse(filePath) {
	    const data = JSON.parse(fs.readFileSync(filePath, 'utf8'))
	    return new SMML.Parser(data).parse()
	}
"]
DeleteObject[Drop[ExternalSessions[],-1]]


(* ::Subsubsection:: *)
(*Packages Initialization*)


(* local data *)
colorData=Association@Import[localPath<>"color.json"];                               (* colors *)
styleColor=RGBColor/@Association@colorData[["StyleColor"]];
buttonColor=RGBColor/@#&/@Association/@Association@colorData[["ButtonColor"]];
pageSelectorColor=RGBColor/@#&/@Association/@Association@colorData[["PageSelectorColor"]];
styleData=Association/@Association@Import[localPath<>"style.json"];                  (* styles *)
styleDict=Normal@Module[{outcome={}},
	If[KeyExistsQ[#,"Size"],AppendTo[outcome,FontSize->#[["Size"]]]];
	If[KeyExistsQ[#,"Family"],AppendTo[outcome,FontFamily->#[["Family"]]]];
	If[KeyExistsQ[#,"Weight"],AppendTo[outcome,FontWeight->ToExpression@#[["Weight"]]]];
	If[KeyExistsQ[#,"Color"],AppendTo[outcome,FontColor->styleColor[[#[["Color"]]]]]];
outcome]&/@styleData;
langDict=Association@Import[localPath<>"Lang\\Languages.json"];                      (* languages *)
tagDict=Association/@Association@Import[localPath<>"Tags.json"];


(* user data *)
If[!DirectoryQ[userPath],
	CreateDirectory[userPath];
	userInfo=userTemplate;
	refreshLanguage;
	uiSetPath;
	Export[userPath<>"Default.json",userInfo],
	userInfo=Association@Import[userPath<>"Default.json"];
	refreshLanguage;
	If[userInfo[["Version"]]<version,
		Do[
			If[!KeyExistsQ[userInfo,tag],AppendTo[userInfo,tag->userTemplate[[tag]]]],
		{tag,Keys@userTemplate}];
		userInfo[["Version"]]=version;
		Export[userPath<>"Default.json",userInfo];
	];
];
If[!FileExistsQ[userPath<>"Favorite.json"],Export[userPath<>"Favorite.json",{}]];
favorite=Import[userPath<>"Favorite.json"];


(* program data *)
dataPath=userInfo[["DataPath"]];
If[!DirectoryQ[dataPath],CreateDirectory[dataPath]];
If[!DirectoryQ[dataPath<>"buffer\\"],CreateDirectory[dataPath<>"buffer\\"]];
If[!DirectoryQ[dataPath<>"images\\"],CreateDirectory[dataPath<>"images\\"]];
If[!FileExistsQ[dataPath<>"Buffer.json"],Export[dataPath<>"Buffer.json",{}]];
bufferHash=Association@Import[dataPath<>"Buffer.json"];
If[!FileExistsQ[dataPath<>"Image.json"],Export[dataPath<>"Image.json",{}]];
imageData=Association/@Association@Import[dataPath<>"image.json"];


(* ::Subsubsection::Closed:: *)
(*Test Code*)


(* ::Input:: *)
(*AudioStop[];*)


(* ::Subsubsection:: *)
(*Engine Start*)


(* ::Input::Initialization:: *)
Main;
