(* ::Package:: *)

BeginPackage["Thulium`Interface`About`", {
  "Thulium`System`",
  "Thulium`Assets`"
}];

About::usage = "Thulium Music About Interface";

Begin["`Private`"];

About[] := CreateDialog[Column[{Spacer[{40,40}],
  Caption[TextDict["About"],"Title"],
  Spacer[{20,20}],
  Row[{Spacer[60],Column[{
    Caption[TextDict["Thulium"],"Subtitle"],
    Spacer[4],
    Grid[{
      {Caption[TagName["Version"],"Text"],Caption[Thulium`System`$$Version,"Text"]},
      {Caption[TagName["Producer"],"Text"],Caption[TextDict["Obstudio"],"Text"]},
      {Caption[TagName["Website"],"Text"],Caption["qymp.ob-studio.cn","Text"]}
    },Alignment->Left]
  },Alignment->Left,ItemSize->Full],Spacer[60]}],
  Spacer[{20,20}],
  Button[TextDict["Return"],DialogReturn[Thulium`Homepage[]],ImageSize->100],
Spacer[{40,40}]},Center,ItemSize->Full],
WindowTitle->TextDict["About"],Background->WindowBackground];

End[];

EndPackage[];

Thulium`About = Thulium`Interface`About`About;


(* ::Input:: *)
(*Thulium`About[];*)
