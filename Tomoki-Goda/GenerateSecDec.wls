#!/usr/bin/env wolframscript
(* ::Package:: *)

Quit[]


(* ::Text:: *)
(*This file can be used to generate splitting patterns for The SecDec function in SecDecV4.wl. *)
(*The resulting lists contain pairs of variables which need to be decomposed.*)
(**)


(* ::Section:: *)
(*Setup*)


SetDirectory[NotebookDirectory[]]
$Path=Join[$Path,{NotebookDirectory[]<>"Files"}];
positions=All;(*Choose primary sectors *)

name="n3lo";(* Gve a name*)

order=3;(* oder in alpha *)
numberofparticles=3; (* number of real emission*)
zval=1/3; (* vale of v *)
deltaterm=False; (* True if z\[Equal]1*)
If[deltaterm,zval=1];
factor={{CF,2},{TF,1}}; (* Clour factor for which we compute the coefficients.*)
epspow=0; (*order of epsilon already present in the matrix element *)


If[deltaterm,name=name<>"del"];
name=name<>StringJoin@@Map[ToString,Flatten[factor]]<>"n"<>ToString[order]<>"lo"<>"r"<>ToString[numberofparticles]<>"e"<>ToString[If[epspow<0,"m",""]]<>ToString[Abs[epspow]]<>"z"<>ToString[Numerator[zval]]<>"-"<>ToString[Denominator[zval]]
(*bar=True;*)
bar=False;





<<Initialize`
<<InputSetup`


RandomNum=Join[Map[#->RandomInteger[{1,99}]/100&,{x1,x2,x3,x4,x5,x6,x7,x8,x9}],Map[#->Pi RandomInteger[{1,99}]/100&,angles]]


limits=Subsets[activevariables,{1,Length[activevariables]}]


(* ::Section:: *)
(*Code*)


ZeroCond[expr_]:=Module[{lim,pos},
lim=Map[expr/.Map[#->0&,#]&,limits];
lim=lim/.RandomNum//N;
pos=lim//Position[#,0.|0,1]&;
(*Print[pos];*)
If[pos==={},Return[{}]];
lim=limits[[Flatten[pos]]];
lim=Intersection@@lim;
Return[lim]]



GenerateSplitPattern[denlist_]:=Module[{list},
list=Map[ZeroCond,denlist,{2}];
list=Map[If[Length[#]<=1,{},Take[#,2]]&,list,{2}];
list=DeleteCases[list,{},{2}];
list=Map[If[Length[#]<1,{},First[#]]&,list];
Return[list]
];



SplitRecursive[den_]:=Module[{safetycounter=1,denlist,pattern,patternlist={}},
(*denlist=denlist0;*)
denlist=Map[Map[First,FactorList[#]]&,den,{3}];
denlist=Map[DeleteDuplicates[Flatten[#]]&,denlist];
denlist=DeleteCases[denlist,a_/;NumberQ[a],{2}];
pattern=GenerateSplitPattern[denlist];
If[Flatten[pattern]=!={},
Print[pattern];
AppendTo[patternlist,pattern];
denlist=TriangleSplit[denlist,pattern];
];

While[Flatten[pattern]=!={},
If[safetycounter>50,Break[]];
safetycounter+=1;
denlist=Map[Map[First,FactorList[#]]&,denlist,{2}];
denlist=Map[DeleteDuplicates[Flatten[#]]&,denlist];
denlist=DeleteCases[denlist,a_/;NumberQ[a],{2}];

pattern=GenerateSplitPattern[denlist];

If[Flatten[pattern]=!={},
Print[pattern];
AppendTo[patternlist,pattern];
denlist=TriangleSplit[denlist,pattern];
];

];
Return[patternlist]
];


(* ::Section:: *)
(*Generate Possible denominators*)


sums=Subsets[li,{1,numberofparticles}];
sums=Plus@@@sums;
all={Thread[sp[sums,sums]],Thread[sp[p-sums,p-sums]],sp[#,n2]&/@sums}//Flatten
all=all//DeleteCases[#,Alternatives@@Map[sp[#,#]&,li]]&


all=ME@@all
all=all//.ReplFuncArgSum


(* ::Section:: *)
(*Remove cases related by permutations*)


Permutations[li]
Map[Thread[li->#]&,%]
Table[Map[input[[i]]/.#&,%],{i,Length[input]}];
Replace[%,{a_,b_,c_,d_}:>{Sort[a],Sort[b],Sort[c],d},3];
Sort/@%;
DeleteDuplicates[%];
Map[Intersection[input,#]&,%]
%//TableForm[#,TableDepth->1]&
(*%//Length*)
positions=Position[input,#]&/@Map[First,%]//Flatten
(*positions=*)inputraw[[%]]
types/@inputraw[[positions]]


(* ::Section:: *)
(*Parametrize, Find overlaps and decompose*)



appeartrig=DeleteDuplicates[Cases[#,Cos[_]|Sin[_]|Csc[_]|Sec[_],-1]]&/@prepared;
appearvars=Union[Cases[#,Alternatives@@Union[li0,liT,lim],-1]]&/@parametrizations;
appearvars=MapThread[Union[#1,#2]&,{appearvars,appeartrig}];
replvars=ME0@@@appearvars;


positions


patternassoc=<||>
SplitPattern=patternassoc;
For[i=1,i<=Length[positions],i++,
pos=positions[[i]];

prepared=PrepareIntegral[all,parametrizations[[pos]]];
prepared=prepared/.pm->1;
(*Print[prepared];*)
(*appeartrig=DeleteDuplicates[Cases[prepared,Cos[_]|Sin[_]|Csc[_]|Sec[_],-1]];
appearvars=Union[Cases[parametrizations[[positions[[i]]]],Alternatives@@Union[li0,liT,lim],-1]];
appearvars=Union[appearvars,appeartrig];
replvars=ME0@@appearvars;
Print[replvars];*)
secdec=SecDec[prepared,inputraw[[pos]],False];
(*Print["secdec" ,secdec];*)
den=Cases[#,ME[a__]:>Map[Numerator[Cancel[Together[#]]]&,{a}],-1]&/@secdec;
pattern=SplitRecursive[den];
name=types[inputraw[[pos]]];
AssociateTo[patternassoc,name->pattern];
(*fi=OpenWrite["./splitpattern"<>name<>".m"];
Write[fi, pattern];
Close[fi]*)
]


patternassoc


fi=OpenWrite["./splitpattern3.m"];
Write[fi, patternassoc];
Close[fi]




