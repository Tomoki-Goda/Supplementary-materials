(* ::Package:: *)

BeginPackage["ExpandPowerV2`",{"Global`"}];
ExpandPower::usage="";
ExpandLog::usage="";
CheckSqrt::usage="";
BreakAbs::usage = "";
PowerFactor::usage = "";
exclusions::usage = "Option for ExpandPower default is {ap,ep}";
Begin["`Private`"]


ExpandPower::expandpower="You are using expand power";


$ExpandPower=True;


ExpandPower::off="$ExpandPower==False";


Options[ExpandPower]={abs->False,warning->False,exclusions->{(*ep,ap*)},
Assumptions->$Assumption(*s&&0<=zeta<=1&&0<=x1<=1&&0<=x2<=1&&0<=x3<=1&&0<=x4<=1&&0<=x5<=1&&0<=x6<=1&&0<=x7<=1&&0<=x8<=1&&0<=x9<=1&&
0<=y1<=1&&0<=y2<=1&&0<=y3<=1&&0<=y4<=1&&0<=y5<=1&&0<=y6<=1&&0<=y7<=1&&0<=y8<=1&&0<=y9<=1*),
(*If[CheckExpandSqrt===True,Safety\[Rule]True,*)Safety->False(*]*)};
ExpandPower[input_,OptionsPattern[]]:=Module[{list1,list2,arg,newarg,sign,pow,res,PMCondition,out,replac,i,j,uncertain={},test},(*If[OptionValue[warning]===True,ExpandPower::warning="you are using ExpandPower.";Message[ExpandPower::warning]];*)ExpandPower::warning="Complex I appears in the expression.";
If[$ExpandPower===False,Message[ExpandPower::off];Return[input]];
If[!FreeQ[input,I],Message[ExpandPower::warning]];

list1=Cases[input,((Power[a_,b_]/;!MatchQ[b,_Integer]&&FreeQ[b,Alternatives@@OptionValue[exclusions]])|Sqrt[_]),{0,Infinity}]//SortBy[#,ByteCount]&//DeleteDuplicates;
list1=list1//DeleteCases[#,Power[a_,b_]/;FreeQ[Factor[a],Times,1]]&;
list1=list1//DeleteCases[#,Sqrt[a_]/;FreeQ[Factor[a],Times,1]]&;
(*Print[list1];*)
If[list1==={},If[EPVerboseFunctions===True,Print["ExpandPower done"]];
(*Print[input];*)
Return[input]];

list1=list1//.Sqrt[a_]:>Power[a,1/2];
(*list1=list0;*)list2={};
For[i=1,i<=Length[list1],i++,
arg=Cases[list1[[i]],Power[a_,b_]:>{a,b},{0}]//First;
pow=arg[[2]];
arg=arg[[1]]//Factor;
If[Head[arg]=!=Times,res=Power[arg,pow],
res=PositiveDecompose[arg,Assumptions->OptionValue[Assumptions]];
(*Print[res];*)
res=Times@@Map[Simplify[Power[#,pow],Assumptions->OptionValue[Assumptions]]&,res];
];
res=res/.Power[Power[a_,b_],c_]/;b<0:>Power[Power[Power[a,-b],c],-1];
list2=AppendTo[list2,Rule[list1[[i]],ReplaceAll[res,list2]]]
];
replac=list2(*MapThread[#1\[Rule]#2&,{list1,list2}]*);
list2=list2[[All,2]];
replac=DeleteCases[replac,Rule[a_,a_]];
list1=replac[[All,1]];



If[replac==={},Return[input]];
If[OptionValue[warning]===True,Message[ExpandPower::expandpower]];
If[EPVerboseFunctions>=3,Print[replac]];

out=input//.replac;
out=out//.Abs[a_]:>Simplify[Abs[a],Assumptions->OptionValue[Assumptions]];
out=out//.replac;
(*Print[replac];*)

If[!FreeQ[out,Alternatives@@list1],Print["remaining,  ",Cases[out,Alternatives@@list1,{0,Infinity}]]];

If[OptionValue[Safety]===True,Print[MapThread[Simplify[#1==#2]&,{list1,list2}]]];
If[!FreeQ[out,I],Print["Potential error, complex"]];
Return[ExpandPower[out]]]


ExpandLog::warning="Input contains complex number"


Options[ExpandLog]={abs->False,warning->False,exclusions->{(*ep,ap*)},Assumptions->$Assumptions
(*&&0<=zeta<=1&&0<=x1<=1&&0<=x2<=1&&0<=x3<=1&&0<=x4<=1&&0<=x5<=1&&0<=x6<=1&&0<=x7<=1&&0<=x8<=1&&0<=x9<=1&&0<=y1<=1&&0<=y2<=1&&0<=y3<=1&&0<=y4<=1&&0<=y5<=1&&0<=y6<=1&&0<=y7<=1&&0<=y8<=1&&0<=y9<=1*),
(*If[CheckExpandSqrt===True,Safety\[Rule]True,*)Safety->False(*]*)};
ExpandLog[input_,OptionsPattern[]]:=Module[{list1,list2,arg,newarg,sign,pow,res,PMCondition,out,replac,i,j,uncertain={},test},(*If[OptionValue[warning]===True,ExpandPower::warning="you are using ExpandPower.";Message[ExpandPower::warning]];*)ExpandPower::warning="Complex I appears in the expression.";
If[$ExpandLog===False,Message[ExpandLog::off];Return[input]];
If[!FreeQ[input,I],Message[ExpandLog::warning]];

list1=DeleteDuplicates[Cases[input,Log[a_],{0,Infinity}]];
(*Print[list1];*)
If[list1==={},If[EPVerboseFunctions===True,Print["ExpandLog done"]];
Return[input//.{Log[1/a_]:>-Log[a],Log[- 1/a_]:>-Log[-a]}]];

list2={};
For[i=1,i<=Length[list1],i++,
arg=list1[[i]]/.Log[a_]:>Factor[a];

If[Head[arg]=!=Times,res=Simplify[Log[arg]],
res=PositiveDecompose[arg];
res=Plus@@Map[Simplify[Log[#]]&,res]//.{Log[1/a_]:>-Log[a],Log[- 1/a_]:>-Log[-a]};
];


list2=AppendTo[list2,Rule[list1[[i]],ReplaceAll[res,list2]]]
];
replac=list2(*MapThread[#1\[Rule]#2&,{list1,list2}]*);
list2=list2[[All,2]];
replac=DeleteCases[replac,Rule[a_,a_]];
list1=replac[[All,1]];


If[replac==={},Return[input]];
If[OptionValue[warning]===True,Message[ExpandLog::expandlog]];
If[EPVerboseFunctions>=3,Print[replac]];

(*Print[replac];*)
out=input//.replac;

If[!FreeQ[out,Alternatives@@list1],Print["remaining,  ",Cases[out,Alternatives@@list1,{0,Infinity}]]];
If[OptionValue[Safety]===True,Print[MapThread[Simplify[#1==#2,Assumptions->OptionValue[Assumptions]]&,{list1,list2}]]];
If[!FreeQ[out,I],Print["Potential error, complex"]];
Return[ExpandLog[out]]]


Options[PositiveDecompose]={Assumptions->True};


PositiveDecompose[expr_,OptionsPattern[]]:=Module[{arglist,tf,neglist,poslist,indlist,leftneg,list},
(*take an expression, return a list of positive factors. Overall sign of the input has to be + *)
arglist=Power@@@FactorList[expr];
tf=Map[Simplify[#>0,Assumptions->$Assumptions&&OptionValue[Assumptions]]&,arglist];
tf=Replace[tf,a_/;FreeQ[a,True|False]->"Indetermined",1];
poslist=Pick[arglist,tf,True];
neglist=Pick[arglist,tf,False];
indlist=Pick[arglist,tf,"Indetermined"];
leftneg=(-1)^(Length[neglist]);
list=Join[poslist,Map[-1*#&,neglist],{leftneg*Times@@indlist}];
If[indlist==={}&&leftneg==-1,
PositiveDecompose::negative="Something mayhave gone wrong. Does the product of `1` make a positive number?,  Make sure you are not using PowerExpand or anything similar. ";
Message[PositiveDecompose::negative, arglist];
(*Print[Style["negative argument",Red]]*)];
(*Print[list];*)
Return[list]
]


ClearAll[CheckSqrt];
Options[CheckSqrt]={Assumptions->(*$Assumptions*)0<=zeta<=1&&0<=x1<=1&&0<=x2<=1&&0<=x3<=1&&0<=x4<=1&&0<=x5<=1&&0<=x6<=1&&0<=x7<=1&&0<=x8<=1&&0<=x9<=1&&0<=y1<=1&&0<=y2<=1&&0<=y3<=1&&0<=y4<=1&&0<=y5<=1&&0<=y6<=1&&0<=y7<=1&&0<=y8<=1&&0<=y9<=1};
CheckSqrt[input_,OptionsPattern[]]:=Module[{list1,arg,newarg,sign,pow,res,PMCondition,i,j},
PrintTemporary[Style["Checking sqrt",Blue]];
If[!FreeQ[input,I],Print[Style["Complex",Red]]];
list1=Cases[input,((Power[a_,b_]/;!MatchQ[b,_Integer]&&FreeQ[b,ap|ep])|Sqrt[_]),{0,Infinity}]//SortBy[#,ByteCount]&//DeleteDuplicates;

list1=list1//.Sqrt[a_]:>Power[a,1/2];
(*list1=list0;*)
PrintTemporary[Length[list1], "  sqrt to check"];
(*Print[list1];*)
For[i=1,i<=Length[list1],i++,
arg=Cases[list1[[i]],Power[a_,b_]:>{a,b},{0}]//First;
pow=arg[[2]];
arg=arg[[1]](*//Expand*)//Factor;
(*Print[arg,",  ",pow];*)
newarg={};
If[Head[arg]=!=Times,arg={arg},
arg=List@@arg];

sign=1;
For[j=1,j<=Length[arg],j++,

PMCondition=Simplify[Reduce[OptionValue[Assumptions]&&(arg[[j]]<0)]];
If[PMCondition =!=False&&PMCondition =!=True ,
PMCondition =(!Simplify[Reduce[OptionValue[Assumptions]&&(arg[[j]]>0)]]);
If[PMCondition =!=False&&PMCondition =!=True ,
Print["The condition to be negative is ",PMCondition]]];

If[PMCondition=!=False,
newarg=AppendTo[newarg,-arg[[j]]];sign*=-1,
newarg=AppendTo[newarg,arg[[j]]]];
];

If[ sign=!=1,Print[arg]
(*CheckSqrt::CheckSqrt="Something mayhave gone wrong. Does the product of `3` make a positive number?,   Power: `1` . Sign: `2` . Make sure you are not using PowerExpand or anything similar. ";
Message[CheckSqrt::CheckSqrt,pow,sign,arg]; newarg=AppendTo[newarg,sign]*)];
];
If[EPVerboseFunctions==2,Print[Style["CheckSqrt Done",Green]]]
]


(*Expandsr[expr_]:=Module[{srcases,srcasesexp,repl},
srcases=DeleteDuplicates[Cases[expr,sr[_],{0,Infinity}]];
srcasesexp=srcases/.sr[a_]:>Sqrt[a]/.Reply//ExpandPower;
repl=MapThread[#1->#2&,{srcases,srcasesexp}];
Return[expr//.repl]
];*)


BreakAbs[in_]:=Module[{abs,absexpr,PM,repl,out},
abs=Cases[in,Abs[a_],-1]//DeleteDuplicates;
absexpr=Map[Power@@@FactorList@@#&,abs];
PM=Map[Simplify[#>0]&,absexpr,{2}];
PM=Map[If[#,1,-1,Abort[];Print[Style["Abs error",Red]]]&,PM,{2}];
repl=MapThread[Inner[Times,#1, #2,Times]&,{absexpr,PM}];
repl=MapThread[#1->#2&,{abs,repl}];
If[VerboseFunctions>=2,Print[repl]];
out=in//.repl;
If[!FreeQ[out,Abs],Print[Style["Abs error",Red]]];
Return[out]
];


PowerFactor[expr_]:=Module[{xi,in,in0,base,pow,baseout,baserem,out},
xi={x1,x2,x3,x4,x5,x6,x7,x8,x9};
xi=Join[xi,Map[Sqrt,xi]];
in0=expr//Factor;
in=DeleteDuplicates[Cases[in0,Power[a_,b_]:>{a,b},{0,Infinity}]];
{base,pow}=Transpose[in];
base=Map[FactorList[#]&,base];
baseout=Map[Cases[#,{x:(Alternatives@@xi),pow_},1]&,base];
(*Print[baseout];*)
baserem=Map[DeleteCases[#,{x:(Alternatives@@xi),pow_},1]&,base];
baserem=MapThread[Power[Times@@Power@@@#1,#2]&,{baserem,pow}];

(*baseout=MapThread[Power[Times@@Power@@@#1,#2]&,{baseout,pow}];*)

baseout=Table[Times@@Map[(#[[1]])^(#[[2]]*pow[[i]])&,baseout[[i]]],{i,1,Length[baseout]}];

out=MapThread[{#1,#2 #3}&,{Power@@@in,baseout,baserem}];
out=DeleteCases[out,{a_,a_}];
out=Rule@@@out;

Return[(in0)//.out]
]


End[]
EndPackage[]
