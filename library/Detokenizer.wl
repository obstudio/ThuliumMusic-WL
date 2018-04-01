(* ::Package:: *)

Detoken[json_] :=
 Block[{Type, Syntax, Element, output, Print},
  Syntax = <|
    "Vol" -> "(!1\\%)",
    "Spd" -> "(!1)",
    "Oct" -> "(Oct:!1)",
    "KeyOct" -> "(1=$1)",
    "BarBeat" -> "(!1/!2)",
    "Dur" -> "(Dur:%1)",
    "Appo" -> "(Appo:%1)",
    "Port" -> "(Port:%1)",
    "Trace" -> "(Trace:!1)",
    "FadeIn" -> "(FadeIn:!1)",
    "FadeOut" -> "(FadeOut:!1)",
    "Rev" -> "(Rev:!1)",
    "Stac" -> "(Stac:%1)",
    "Tremolo1" -> "(%1-)&2",
    "Tremolo2" -> "&2(%1=)&3",
    "Tuplet" -> "(!1~)&2",
    "Portamento" -> "&1~&2",
    "GraceNote" -> "(@1^)&2",
    "Appoggiatura" -> "&1(^@2)",
    "Arpeggio" -> "\\$&1",
    "Fermata" -> "(.)&1"
    |>;
  Type =
   Association[
    "Number" -> (ToString["Content" /. #] &),
    "Expression" -> (ToString["Content" /. #] &),
    "String" -> ("\"" <> ("Content" /. #) <> "\"" &),
    "Package" -> Function[{package},
      AssociateTo[output, ("Path" /. package) ->
        StringDelete[
         StringRiffle[Element /@ ("Content" /. package), "\n"] <> 
          If[Count["Type" /. package, "Package"] < Length[package], 
           "\n\n#End", ""], StartOfString ~~ Whitespace]];
      "# Include \"" <> ("Path" /. package) <> "\""],
    "Function" -> Function[{function},
      Block[{EachData},
       EachData =
        ((AssociateTo[Syntax, ("Name" /. #) -> ("Syntax" /. #)];
           "Code" /. #) &);
       "\n# Function\n\n" <> 
        StringRiffle[EachData /@ ("Data" /. function), "\n\n"]]],
    "Chord" -> Function[{function},
      Block[{EachData},
       EachData[data_] :=
        Block[{Notation, Comment, Pitches},
         Notation = # &;
         Comment = # &;
         Pitches[pitches_List] :=
          Block[{EachPitch},
           EachPitch = If[#1 == "1" && #2 == "1", #3,
              "[" <> Which[
                #1 == "1" && #2 == "-1", "",
                #1 == #2, #1,
                #1 == "1", ";" <> #2,
                #2 == "-1", #1 <> ";",
                True, #1 <> ";" <> #2]
               <> "]" <> Which[
                #3 == "0", "",
                StringTake[#3, 1] == "-", #3,
                True, "+" <> #3]] &;
           
           StringRiffle[EachPitch @@@ Map[ToString, pitches, {2}], 
            ", "]];
         StringRiffle[ToExpression[#][# /. data] & /@
           {"Notation",
            "Comment",
            "Pitches"
            }, "\t\t"]];
       "\n# Chord\n\n" <> 
        StringRiffle[EachData /@ ("Data" /. function), "\n\n"]]],
    "Track" -> Function[{track},
      Block[{EachData},
       EachData[data_] := 
        "<*" <> ("Name" /. data) <> "*>" <> 
         StringJoin[Element /@ ("Content" /. data)];
       "\n# Track\n\n" <> 
        StringRiffle[EachData /@ ("Data" /. track), "\n\n"]]],
    
    "Function" -> Function[{function},
      If["Simplified" /. function,
       Evaluate@Quiet@StringReplace[Syntax["Name" /. function],
            {"\\!" -> "!",
             "\\%" -> "%",
             "\\$" -> "$",
             "\\@" -> "@",
             "\\&" -> "&",
             
             "!" | "%" ~~ n : DigitCharacter .. :> 
              Slot@ToExpression[n],
             
             "$" | "@" | "&" ~~ n : DigitCharacter .. :> 
              StringTake[Slot@ToExpression[n], {2, -2}]}] & @@ 
        Element /@ ("Argument" /. function),
       ("Name" /. function) <> "(" <> 
        StringRiffle[Element /@ ("Argument" /. function), ","] <> 
        ")"]],
    "RepeatBegin" -> ("||:" &),
    "RepeatEnd" -> (":||" &),
    "Volta" -> ("[" <> StringRiffle[If[Length[#] > 2,
            ToString@First[#] <> "~" <> ToString@Last[#],
            StringRiffle[ToString /@ #, "."]] & /@ 
          Split["Order" /. #, #1 + 1 == #2 &], "."] <> ".]" &),
    "Macrotrack" -> ("@" <> ("Name" /. #) &),
    
    "Note" -> Function[{note},
      Block[{Pitches, PitOp, DurOp, VolOp, Staccato},
       Pitches = 
        If[Length[#] == 1, StringJoin[#], "[" <> StringJoin[#] <> "]"]
           &[{"Degree", "PitOp", "Chord"} /. #] &;
       
       PitOp = # &;
       DurOp = # &;
       VolOp = # &;
       Staccato = If[# == 0, "", StringRepeat["`", #]] &;
       StringJoin[ToExpression[#1][#1 /. note] & /@
         {
          "Pitches",
          "PitOp",
          "DurOp",
          "VolOp",
          "Staccato"
          }]]],
    "PedalPress" -> ("&" &),
    "PedalRelease" -> ("*" &),
    "Tie" -> ("^" &),
    "Coda" -> ("+" &),
    "Segno" -> ("s" &),
    "DaCapo" -> ("DC" &),
    "DaSegno" -> ("DS" &),
    "BarLine" -> (If["Skip" /. #, "\\", 
        If[("Order" /. #) === {0}, "|", 
         "\\" <> StringTake[ToString["Order" /. #], {2, -2}] <> 
          ":"]] &),(**)
    "Subtrack" -> ("{" <>
        If[("Repeat" /. #) < -1, ToString[-("Repeat" /. #)] <> "*", 
         ""] <> Element /@ ("Content" /. #) <> "}" &),
    "Whitespace" -> ("Content" /. # &),
    "Undefined" -> ("Content" /. # &)];
  
  Element = Type["Type" /. #][#] &;
  
  Print[input_] := Block[{Comments, Library, Settings, Sections},
    Comments[comments_] := StringRiffle[comments, "\n"];
    Library[library_List] := 
     StringDelete[
      StringRiffle[Element /@ library, "\n"] <> 
       If[Count["Type" /. library, "Package"] < Length[library], 
        "\n\n#End", ""], StartOfString ~~ Whitespace];
    (*Settings[settings_]:=settings;*)
    
    Sections[sections_List] :=
     Block[{EachSection},
      EachSection[eachsection_] :=
       Block[{ID, Comments, Settings, Tracks},
        (*ID[id_]:=id;*)
        
        Comments[comments_] := comments <> "\n";
        
        Settings[settings_List] :=
         If[settings === {}, "\n", Element /@ settings <> "\n\n"];
        
        Tracks[tracks_List] :=
         Block[{EachTrack},
          EachTrack[track_] :=
           Block[{Id, Instruments, Content},
            Id = If[# === Null, "<", "<" <> # <> ":"] &;
            
            Instruments[instruments_List] :=
             Block[{EachInstrument},
              EachInstrument[instrument_] :=
               Block[{Instrument, Proportion},
                Instrument = # &;
                
                Proportion = 
                 If[# === Null, "", 
                   "(" <> 
                    StringDelete[ToString[100 #], 
                    "." ~~ EndOfString] <> "%)"] &;
                StringJoin[ToExpression[#1][#1 /. instrument] & /@
                  {
                   "Instrument",
                   "Proportion"
                   }]
                ];
              StringRiffle[EachInstrument /@ instruments, ","] <> ">"];
            
            Content[contents_List] := Element /@ contents;
            
            
            StringDelete[
             StringJoin[ToExpression[#1][#1 /. track] & /@
               {
                "Id",
                "Instruments",
                "Content"
                }], StartOfString ~~ "<>"]];
          StringRiffle[EachTrack /@ tracks, "\n\n"]];
        
        StringReplace[
         StringJoin[ToExpression[#1][#1 /. eachsection] & /@
           {
            (*"ID",*)
            "Comments",
            "Settings",
            "Tracks"
            }], StartOfString ~~ "\n\n" -> "\n"]];
      StringDelete[StringRiffle[EachSection /@ sections, "\n\n"], 
       StartOfString ~~ Whitespace]];
    AssociateTo[output, 
     "main" -> StringRiffle[ToExpression[#1][#1 /. input] & /@
         {
          "Comments",
          (*"Undefined",*)
          "Library",
          (*"Settings",*)
          "Sections"
          } /. "" -> Nothing, "\n\n"]]];
  output = <||>;
  
  Print[json]]



(* ::Input:: *)
(*input=Import[NotebookDirectory[]<>"test.json"];*)


(* ::Input:: *)
(*Detoken@input*)
