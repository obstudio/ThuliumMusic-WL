(* ::Package:: *)

(* ::Text:: *)
(*Qingyun Music Player*)


localPath=NotebookDirectory[];
<<(localPath<>"Assets\\assets.wl")    (* graphics *)
<<(localPath<>"Lib\\library.wl")      (* library *)
<<(localPath<>"Lib\\uiControls.wl")   (* controls *)
<<(localPath<>"Lib\\uiUser.wl")       (* UI for common users *)
<<(localPath<>"Lib\\uiDeveloper.wl")  (* UI for developers *)
<<(localPath<>"Lib\\qymToken.wl")     (* QYM tokenizer *)
<<(localPath<>"Lib\\qysToken.wl")     (* QYS tokenizer *)
<<(localPath<>"Lib\\parser.wl")       (* parser *)


(* local data *)
instrData=Association@Import[localPath<>"instr.json"];                               (* instruments *)
colorData=Association@Import[localPath<>"color.json"];                               (* colors *)
styleColor=RGBColor/@Association@colorData[["StyleColor"]];
buttonColor=RGBColor/@#&/@Association/@Association@colorData[["ButtonColor"]];
pageSelectorColor=RGBColor/@#&/@Association/@Association@colorData[["PageSelectorColor"]];
styleData=Association/@Association@Import[localPath<>"style.json"];                  (* styles *)
styleDict=Normal@Module[{outcome={}},
	If[KeyExistsQ[#,"FontSize"],AppendTo[outcome,FontSize->#[["FontSize"]]]];
	If[KeyExistsQ[#,"FontFamily"],AppendTo[outcome,FontFamily->#[["FontFamily"]]]];
	If[KeyExistsQ[#,"FontWeight"],AppendTo[outcome,FontWeight->ToExpression@#[["FontWeight"]]]];
	If[KeyExistsQ[#,"FontColor"],AppendTo[outcome,FontColor->styleColor[[#[["FontColor"]]]]]];
outcome]&/@styleData;
langDict=Association@Import[localPath<>"Lang\\Languages.json"];                      (* languages *)


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


(* ::Input::Initialization:: *)
refresh;updateImage;updateBuffer;


(* ::Input::Initialization:: *)
QYMP;


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
