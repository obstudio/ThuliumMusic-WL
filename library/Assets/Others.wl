(* ::Package:: *)

BeginPackage["Thulium`Interface`Settings`", {
  "Thulium`System`",
  "Thulium`Assets`"
}];

Settings::usage = "Thulium Music Settings Interface";

Begin["`Private`"];

Settings[] := Module[{choices = UserInfo},
  CreateDialog[Column[{Spacer[{40,40}],
    Caption[TextDict["Settings"],"Title"],Spacer[1],
    Row[{Spacer[40],Grid[{
      {Caption[TextDict["ChooseIdentity"],"Text"],
        RadioButtonBar[Dynamic @ choices["Developer"],{
          False->Caption[TextDict["NormalUser"],"Text"],
          True->Caption[TextDict["Developer"],"Text"]
        }]
      },
      {Caption[TextDict["ChooseLanguage"],"Text"],
        RadioButtonBar[Dynamic@choices["Language"],LangDict]}
      }
    ],Spacer[40]}],Spacer[1],
    Row[{
      Button[TextDict["Save"],
        UserInfo=choices;
        Export[$UserPath<>"Default.json",UserInfo];
        RefreshLanguage;
        DialogReturn[Thulium`homepage],
      ImageSize->150],
      Spacer[10],
      Button[TextDict["Return"],DialogReturn[Thulium`homepage],ImageSize->150]
    }],Spacer[{40,40}]
  },Center,ItemSize->Full],
  Background->WindowBackground,WindowTitle->TextDict["Settings"]]
];

End[];

EndPackage[];

Thulium`Settings = Thulium`Interface`Settings`Settings;


(* ::Input:: *)
(*Thulium`Settings[];*)


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
  Button[TextDict["Return"],DialogReturn[Thulium`homepage],ImageSize->100],
Spacer[{40,40}]},Center,ItemSize->Full],
WindowTitle->TextDict["About"],Background->WindowBackground];

End[];

EndPackage[];

Thulium`About = Thulium`Interface`About`About;


(* ::Input:: *)
(*Thulium`About[];*)
