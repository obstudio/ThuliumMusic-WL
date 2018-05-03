(* ::Package:: *)

uiPlayerControlsOld := With[{StatusAlias = StatusAlias}, {
	Row[{
		Dynamic[Style[timeDisplay[$CurrentStream["Position"]],20]],
		Spacer[8],
		ProgressIndicator[Dynamic[$CurrentStream["Position"]/$CurrentDuration],ImageSize->{240,16}],
		Spacer[8],
		Style[timeDisplay[$CurrentDuration],20]
	}],Spacer[1],
	Row[{Button[
		Dynamic[Switch[$CurrentStream[StatusAlias],
			"Playing",text[["Pause"]],
			"Paused"|"Stopped",text[["Play"]]
		]],
		Switch[$CurrentStream[StatusAlias],
			"Playing",$CurrentStream[StatusAlias]="Paused",
			"Paused"|"Stopped",$CurrentStream[StatusAlias]="Playing"
		],
		ImageSize->80],
		Spacer[20],
		Button[text[["Stop"]],$CurrentStream[StatusAlias]="Stopped",ImageSize->80],
		Spacer[20],
		Button[text[["Return"]],AudioStop[];DialogReturn[uiPlaylist[currentPlaylist]],ImageSize->80]			
	}]
}];


uiPlayerControlsNew := With[{StatusAlias = StatusAlias}, {
	Row[{
		Column[{Style[Dynamic[timeDisplay[$CurrentStream["Position"]]],20],Spacer[1]}],
		Spacer[8],
		Magnify[
			EventHandler[Dynamic@Graphics[{
				progressBar[$CurrentStream["Position"]/$CurrentDuration,16],
				progressBlock[$CurrentStream["Position"]/$CurrentDuration,16]
			}],
			{"MouseDragged":>(
				$CurrentStream["Position"]=$CurrentDuration*progressLocate[CurrentValue[{"MousePosition","Graphics"}][[1]],16]
			)}],
		3.6],
		Spacer[8],
		Column[{Style[timeDisplay[$CurrentDuration],20],Spacer[1]}]
	},ImageSize->Full,Alignment->Center],
	Row[{
		DynamicModule[{style="Default"},
			Dynamic@Switch[$CurrentStream[StatusAlias],
				"Playing",EventHandler[button["Pause",style],{
					"MouseDown":>(style="Clicked"),
					"MouseUp":>(style="Default";$CurrentStream[StatusAlias]="Paused")
				}],
				"Paused"|"Stopped",EventHandler[button["Play",style],{
					"MouseDown":>(style="Clicked"),
					"MouseUp":>(style="Default";$CurrentStream[StatusAlias]="Playing")
				}]
			]
		],
		Spacer[20],
		DynamicModule[{style="Default"},
			EventHandler[Dynamic@button["Stop",style],{
				"MouseDown":>(style="Clicked"),
				"MouseUp":>(style="Default";$CurrentStream[StatusAlias]="Stopped";$CurrentStream["Position"]=0)
			}]
		],
		Spacer[20],
		DynamicModule[{style="Default"},
			EventHandler[Dynamic@button["ArrowL",style],{
				"MouseDown":>(style="Clicked";),
				"MouseUp":>(style="Default";
					AudioStop[];
					DialogReturn[uiPlaylist[currentPlaylist]];
				)
			}]
		]		
	},ImageSize->{300,60},Alignment->Center]
}];


uiPageSelector[Dynamic[page_], pageCount_] := Row[{
	Dynamic@If[page<=1,pageSelectorDisplay["Prev","Disabled"],
	DynamicModule[{style="Default"},
		EventHandler[Dynamic@pageSelectorDisplay["Prev",style],{
			"MouseDown":>(style="Clicked"),
			"MouseUp":>(style="Default";page--;)
		}]
	]],
	Spacer[20],
	Row[Flatten@Array[{
		Dynamic@If[page==#,pageSelectorDisplay[#,"Current",32],
		DynamicModule[{style="Default"},
			EventHandler[Dynamic@pageSelectorDisplay[#,style,32],{
				"MouseDown":>(style="Clicked"),
				"MouseUp":>(style="Default";page=#;)
			}]
		]
	],Spacer[6]}&,pageCount]],
	Spacer[14],
	Dynamic@If[page>=pageCount,pageSelectorDisplay["Next","Disabled"],
	DynamicModule[{style="Default"},
		EventHandler[Dynamic@pageSelectorDisplay["Next",style],{
			"MouseDown":>(style="Clicked"),
			"MouseUp":>(style="Default";page++;)
		}]
	]]
}, ImageSize->{500,60}, Alignment -> Center];


SetAttributes[button,HoldRest];
button[buttonName_,action_]:=DynamicModule[{style="Default"},
	EventHandler[Dynamic@buttonDisplay[buttonName,style],{
		"MouseDown":>(style="Clicked"),
		"MouseUp":>(style="Default";action;)
	}]
]
