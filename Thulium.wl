(* ::Package:: *)

(* ::Title:: *)
(*Thulium Music Player v2.1*)


(* ::Text:: *)
(*Please click the "Run All Code" button at the upper-right corner to run Thulium Music Player.*)


System`localPath = StringReplace[NotebookDirectory[],"\\"->"/"];
SetDirectory[localPath];
<< (localPath <> "Preload.wl");
Scan[Get, FileNames["*.wl", "library", Infinity]];
Scan[Get, FileNames["*.wl", "package", Infinity]];
Scan[Get, FileNames["*.wl", "assets", Infinity]];


InitializeParser


Main


(* ::Input:: *)
(*Assistant*)


(* ::Input:: *)
(*RenderTMD[localPath<>"docs/test.tmd"]*)
