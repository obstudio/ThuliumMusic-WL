(* ::Package:: *)

instrDict=Association@Import[NotebookDirectory[]<>"instrument.json"];
instrRequired=instrDict[["Required"]];
instrLocal=instrDict[["Local"]];
