(* ::Package:: *)

(* ::Title:: *)
(*Thulium Music Player v2.1*)


(* ::Text:: *)
(*Please click the "Run All Code" button at the upper-right corner to run Thulium Music Player.*)


System`localPath = StringReplace[NotebookDirectory[],"\\"->"/"];
SetDirectory[localPath];
<< (localPath <> "init.wl");
Scan[Get, StringReplace["\\"->"/"]/@FileNames["*.wl", "library", Infinity]];
Scan[Get, StringReplace["\\"->"/"]/@FileNames["*.wl", "package", Infinity]];
Scan[Get, StringReplace["\\"->"/"]/@FileNames["*.wl", "assets", Infinity]];


initJS


Main


(* ::Input:: *)
(*Assistant*)


(* ::Input:: *)
(*RenderTMD[localPath<>"docs/test.tmd"]*)
