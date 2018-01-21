(* ::Package:: *)

(* ::Text:: *)
(*Qingyun Music Player*)


path=NotebookDirectory[];
<<(path<>"Lib\\library.wl")      (* library *)
<<(path<>"Lib\\assets.wl")       (* graphics *)
<<(path<>"Lib\\uiControls.wl")   (* controls *)
<<(path<>"Lib\\uiUser.wl")       (* UI for common users *)
<<(path<>"Lib\\uiDeveloper.wl")  (* UI for developers *)
<<(path<>"Lib\\qymToken.wl")     (* QYM tokenizer *)
<<(path<>"Lib\\qysToken.wl")     (* QYS tokenizer *)
<<(path<>"Lib\\parser.wl")       (* parser *)


(* temporary function *)
QYMParse[filename_]:=integrate[#MusicClips,#Effects]&@parse[QYM`tokenizer[filename]];
QYSParse[filename_]:=integrate[#MusicClips,#Effects]&@parse[QYS`Tokenize[filename]];


(* user data *)
version=201;
userPath=$HomeDirectory<>"\\AppData\\Local\\QYMP\\";
cloudPath="http://qymp.ob-studio.cn/assets/";
If[!DirectoryQ[userPath],CreateDirectory[userPath]];
If[!DirectoryQ[userPath<>"buffer\\"],CreateDirectory[userPath<>"buffer\\"]];
If[!DirectoryQ[userPath<>"images\\"],CreateDirectory[userPath<>"images\\"]];
template=<|"Version"->version,"Language"->"chs","Developer"->False,"Player"->"Old"|>;
If[!FileExistsQ[userPath<>"Default.json"],Export[userPath<>"Default.json",template]];
userInfo=Association@Import[userPath<>"Default.json"];
If[userInfo[["Version"]]<version,
	Do[
		If[!KeyExistsQ[userInfo,tag],AppendTo[userInfo,tag->template[[tag]]]],
	{tag,Keys@template}];
	userInfo[["Version"]]=version;
	Export[userPath<>"Default.json",userInfo];
];
If[!FileExistsQ[userPath<>"Buffer.json"],Export[userPath<>"Buffer.json",{}]];
bufferHash=Association@Import[userPath<>"Buffer.json"];
If[!FileExistsQ[userPath<>"Favorite.json"],Export[userPath<>"Favorite.json",{}]];
favorite=Import[userPath<>"Favorite.json"];
If[!FileExistsQ[userPath<>"Image.json"],Export[userPath<>"Image.json",{}]];
imageData=Association/@Association@Import[userPath<>"image.json"];


(* local data *)
instrData=Association@Import[path<>"instr.json"];                               (* instruments *)
colorData=Association@Import[path<>"color.json"];                               (* colors *)
styleColor=RGBColor/@Association@colorData[["StyleColor"]];
buttonColor=RGBColor/@#&/@Association/@Association@colorData[["ButtonColor"]];
pageSelectorColor=RGBColor/@#&/@Association/@Association@colorData[["PageSelectorColor"]];
styleData=Association/@Association@Import[path<>"style.json"];                  (* styles *)
styleDict=Normal@Module[{outcome={}},
	If[KeyExistsQ[#,"FontSize"],AppendTo[outcome,FontSize->#[["FontSize"]]]];
	If[KeyExistsQ[#,"FontFamily"],AppendTo[outcome,FontFamily->#[["FontFamily"]]]];
	If[KeyExistsQ[#,"FontWeight"],AppendTo[outcome,FontWeight->ToExpression@#[["FontWeight"]]]];
	If[KeyExistsQ[#,"FontColor"],AppendTo[outcome,FontColor->styleColor[[#[["FontColor"]]]]]];
outcome]&/@styleData;
dictionary=Association/@AssociationMap[Import[path<>"Lang\\"<>#<>".json"]&,langList];       (* languages *)
refreshLanguage;


(* ::Input::Initialization:: *)
refresh;updateImage;updateBuffer;


(* ::Input::Initialization:: *)
QYMP;


(* ::Input:: *)
(*AudioStop[];*)


(* ::Input:: *)
(*AudioStop[];AudioPlay@QYSParse[path<>"Songs\\Touhou\\Houkainohi.qys"];*)


(* ::Input:: *)
(*AudioStop[];AudioPlay@QYSParse[path<>"Songs\\test.qys"];*)


(* ::Input:: *)
(*Options[QYSParse[path<>"Songs\\Touhou\\Houkainohi.qys"]]*)


(* ::Input:: *)
(*Print[index["Touhou\\Hartmann_No_Youkai_Otome","Comment"]];*)


(* ::Input:: *)
(*EmitSound@Sound@SoundNote[23,.5,"Halo"]*)


(* ::Input:: *)
(*EmitSound@Sound@SoundNote["PanFlute"]*)
