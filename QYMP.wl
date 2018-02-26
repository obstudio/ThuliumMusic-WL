(* ::Package:: *)

(* ::Text:: *)
(*Qingyun Music Player*)


localPath=NotebookDirectory[];
<<(localPath<>"Assets\\assets.wl")         (* graphics *)
<<(localPath<>"Assets\\uiControls.wl")     (* controls *)
<<(localPath<>"Assets\\uiUser.wl")         (* UI for common users *)
<<(localPath<>"Assets\\uiDeveloper.wl")    (* UI for developers *)
<<(localPath<>"Lib\\library.wl")           (* library *)

<<(localPath<>"src\\Adapter.wl")
<<(localPath<>"src\\Syntax.wl")
<<(localPath<>"src\\Tokenizer.wl")
<<(localPath<>"src\\Diagnostor.wl")

<<(localPath<>"package\\Standard\\.init.wl")


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


(* ::Subsubsection:: *)
(*Engine Start*)


(* ::Input::Initialization:: *)
refresh;updateImage;updateBuffer;


(* ::Input::Initialization:: *)
QYMP;


(* ::Subsubsection:: *)
(*Test Code*)


(* ::Input:: *)
(*AudioStop[];*)


(* ::Input:: *)
(*AudioStop[];AudioPlay@QYSParse[localPath<>"Songs\\Touhou\\Houkainohi.qys"];*)


(* ::Input:: *)
(*AudioStop[];AudioPlay@QYSParse[localPath<>"Songs\\test.qys"];*)


(* ::Input:: *)
(*Options[QYSParse[localPath<>"Songs\\Touhou\\Houkainohi.qys"]]*)


(* ::Input:: *)
(*Print[index["Touhou\\Hartmann_No_Youkai_Otome","Comment"]];*)


(* ::Input:: *)
(*EmitSound@Sound@SoundNote[23,.5,"Halo"]*)


(* ::Input:: *)
(*EmitSound@Sound@SoundNote["PanFlute"]*)
