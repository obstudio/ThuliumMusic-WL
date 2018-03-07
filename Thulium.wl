(* ::Package:: *)

(* ::Text:: *)
(*Thulium Music Player v2.1*)


System`localPath=NotebookDirectory[];
<<(localPath<>"init.wl");
<<(localPath<>"source\\Graphics.wl");

<<(localPath<>"src\\Tokenizer.wl");
<<(localPath<>"src\\Adapter.wl")           (* adapter *)
<<(localPath<>"src\\Syntax.wl");
<<(localPath<>"package\\Standard\\.init.wl");

<<(localPath<>"Assets\\assets.wl")         (* graphics *)
<<(localPath<>"Assets\\uiControls.wl")     (* controls *)
<<(localPath<>"Assets\\uiUser.wl")         (* UI for common users *)
<<(localPath<>"Assets\\uiDeveloper.wl")    (* UI for developers *)

<<(localPath<>"ide\\Diagnostor.wl")        (* diagnoser *)


initJS


(* ::Code::Initialization:: *)
Main
