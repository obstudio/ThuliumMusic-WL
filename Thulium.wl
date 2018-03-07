(* ::Package:: *)

(* ::Subsubsection:: *)
(*Thulium Music Player v2.1*)


System`localPath = NotebookDirectory[];
SetDirectory[localPath];
<< (localPath <> "init.wl");
Scan[Get, FileNames["*.wl", "library", Infinity]];
Scan[Get, FileNames["*.wl", "package", Infinity]];
Scan[Get, FileNames["*.wl", "assets", Infinity]];


initJS


Main


(* ::Input:: *)
(*Assistant*)
