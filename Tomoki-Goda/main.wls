#!/usr/bin/env wolframscript
(* ::Package:: *)

Quit[]


(* ::Text:: *)
(**)


(* ::Section::Closed:: *)
(*Set up *)


(* ::Text:: *)
(*Specify  what you want to run*)


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





Import["Initialize.wl"]


Protect[name,numberofparticles,deltaterm,order,zval,factor,epspow,bar];


(* ::Section:: *)
(*Pre-run set up *)


LaunchKernels[8]
SetSharedVariable[ShareAssum]
ShareAssum=$Assumptions
ParallelEvaluate[$Assumptions=ShareAssum];
<<InputSetup`


If[deltaterm,
measureorig=measureorig/.ScaleZ//ExpandPower[#,exclusions->{}]&;
measureorig=measureorig//.Power[a_,b_]:>Simplify[Power[a,b]];
measureorig=measureorig/.TEST[a_][b_ sp[c__]^powe_.]/;FreeQ[b,sp]:>b TEST[a][sp[c]^powe];
measureorig=measureorig/.delta[a_]:>deltaScale[delta[a]];
measureorig=measureorig/.{(1-z)^pow_.:>ZPow[pow]};
]
Protect[measureorig];


If[deltaterm,
myme=myme/.ScaleZ//Cancel];
myme=myme/.SCALE[a_]:>SCALE[Simplify[a]];
(*Print[myme];*)
myme=J3[1]SCALE[1] myme;
(*myme=myme/.z->zval;
Print[Style["z set to "<>ToString[zval//N],Red]]*)


(* ::Section::Closed:: *)
(*Generate Selector*)


(* ::Text:: *)
(*Selector Function*)


selectors/.ReplT2Sp/.ReplEn2Sp//.Replp;
tselectors=sel/@%;


(* ::Section:: *)
(*Prepare*)


(* ::Input:: *)
(*(*<<SecDecV4`*)
(*<<FunctionsV4`*)*)


(* ::Text:: *)
(*Determine Parametrization*)


variables=ReplaceAll[fullvariables,ReplspT]//.Replp/.sp[a_,a_]->0;
prepared=ParallelMap[PrepareIntegral[ME[Sequence@@variables],#]&,parametrizations,DistributedContexts->All];
appeartrig=DeleteDuplicates[Cases[#,Cos[_]|Sin[_]|Csc[_]|Sec[_],-1]]&/@prepared;


(* ::Text:: *)
(*Sector decomposition*)


<<SecDecV4`
<<ExpandPowerV2`
If[Decomposition===True,
(*appearvars=Union[Cases[#,Alternatives@@Union[li0,liT,lim],-1],Flatten[Map[{Cos[#],Sin[#]}&,Cases[#,Alternatives@@angles,-1]]]]&/@parametrizations;*)
appearvars=Union[Cases[#,Alternatives@@Union[li0,liT,lim],-1]]&/@parametrizations;
appearvars=MapThread[Union[#1,#2]&,{appearvars,appeartrig}];

replvars=ME0@@@appearvars;
decomposed0=Parallelize[MapThread[SecDec[#1,#2]&,{replvars,inputraw}],DistributedContexts->All];

decomposed={};
Do[
res=SecDec2[decomposed0[[i]],appearvars[[i]]];
AppendTo[decomposed,MapThread[#1(prepared[[i]]//.#2)&,res]]
,{i,Length[decomposed0]}
];

(**************************************************************************)

Put[{decomposed0,appearvars},logdir<>"/decomposed0"<>name<>".m"];
Print["Decomposed"];

Put[decomposed,logdir<>"/decomposed"<>name<>".m"];
Print["Decomposed"];
];


(* ::Section::Closed:: *)
(*Solve delta function*)


processed0=Map[Process[#,x5]&,decomposed,{2}];


(* ::Section:: *)
(*Extract Scaling*)


processed=ParallelMap[ProcessJacobian[#]&,processed0,{2},DistributedContexts->All];
(*<<SecDecV4`
processed=Map[ProcessJacobian[#]&,processed0,{2}];*)


(* ::Input:: *)
(*(*name="1L"*)*)


Do[
fi=OpenWrite[logdir<>"/processed"<>name<>ToString[primarysector]<>".m"];
Write[fi,processed[[primarysector]]];
Close[fi]
,{primarysector,1,Length[processed]}
];


replspc=Parallelize[Map[spToc[#,fullvariables]&,processed[[All,All,2]],{2}],DistributedContexts->All];
replsp=replspc[[All,All,1]];
replc=replspc[[All,All,2]];

Do[fi=OpenWrite[logdir<> "/repl"<>name<>ToString[primarysector]<>".m"];
	Write[fi,{replsp[[primarysector]],replc[[primarysector]]}];
	Close[fi];
	,{primarysector,1,Length[replsp]}
];





(* ::Section:: *)
(*Additional scaling (for loops)*)


<<FunctionsV4`


For[i=1,i<=Length[decomposed],i++,
{replsp,replc}=Import[logdir<>"/repl"<>name<>ToString[i]<>".m"];
processedi=Import[logdir<>"/processed"<>name<>ToString[i]<>".m"]/.ME[__]->1;

monJ3=Map[First[Cases[myme,J3[a_]:>Factor[a/.i0->0/.#],{0,Infinity}]]&,replsp];
monJ3=monJ3/.Power[a_,b_]:>Power[Factor[a],b]//.Power[(xi:Alternatives@@activevariables )^powx_. fac_,power_]:>Power[xi^powx,power] Power[fac,power];
monJ3=Map[ExtractMonomial,monJ3];
monJ3=Map[{#[[1]],J3[ #[[2]] ]}&,monJ3];
Print[monJ3];

mymedecomposed=SplitScaling[myme,#]&/@replsp;
mymedecomposed=mymedecomposed/.SCALE[_]->0/.J3[_]->1;
mymedecomposed=Table[Map[Inner[Times,processedi[[jj]],#,List]&,mymedecomposed[[jj]]],{jj,Length[mymedecomposed]}];
mymedecomposed=Table[Map[Inner[Times,monJ3[[jj]],#,List]&,mymedecomposed[[jj]]],{jj,Length[mymedecomposed]}];

mymedecomposed=Transpose[mymedecomposed,1<->2];
(*Print[mymedecomposed];*)
Do[
Put[mymedecomposed[[ii]],logdir<>"/mymedecomposed"<>name<>ToString[i]<>"-"<>ToString[ii]<>".m"],
{ii,Length[mymedecomposed]}]
]


(* ::Section::Closed:: *)
(*Expand Integrals in alpha and epsilon*)


countall=0;
For[i=1,i<=Length[decomposed],i++,
j=1;
While[FileExistsQ[logdir<>"/mymedecomposed"<>name<>ToString[i]<>"-"<>ToString[j]<>".m"],
mymedecomposed=Import[logdir<>"/mymedecomposed"<>name<>ToString[i]<>"-"<>ToString[j]<>".m"];
regpowers=Wtemp*mymedecomposed[[All,1]];
Print[regpowers];
skeleton=regpowers/.Wtemp-> W@@activevariables/.ReplLaurent;
Print[skeleton];
integrals=Map[ReplaceRepeated[Expand[Normal[Series[#,{ap,0,0},{ep,0,1}]]],delta[arg_]W[args1___,arg_,args2___]:>  W[args1,0,args2]]&,skeleton,{1}];
(*Print[integrals];*)
	fi=OpenWrite[logdir<>"/integrals"<>name<>ToString[i]<>"-"<>ToString[j]<>".m"];
	Write[fi,integrals];
	Close[fi];
	
j+=1;
countall+=Length[mymedecomposed];
(*Print[integrals];
Print["Next"];*)
];
Print[countall];
]



(* ::Section:: *)
(*remove spurious poles*)


(*CloseKernels[]*)
(*LaunchKernels[8]*)


sample=Table[i,{i,1,Length[tselectors]}]
(*sample={37}*)


<<FunctionsV4`
<<SecDecV4`
<<ExpandPowerV2`
<<SimpleRelations`


(*{replsp,replc}=Import[logdir<>"/repl"<>name<>ToString[1]<>".m"];*)


startnumber=1
For[smplno=startnumber,smplno<=Length[sample],smplno++,(*Loop over primary sector*)
	primarysector=sample[[smplno]];
	Put[smplno,logdir<>"/startnumber.m"];
	
	{replsp,replc}=Import[logdir<>"/repl"<>name<>ToString[primarysector]<>".m"];
	For[subsector=1,FileExistsQ[logdir<>"/mymedecomposed"<>name<>ToString[primarysector]<>"-"<>ToString[subsector]<>".m"],subsector++, (*loop over secondary sector*)
		(*************************************************)
		(*  Prepare replacements for the scalar products  *)
		(*************************************************)
		Print[Date[]];
		Print[Style["Primary sector ",Bold], primarysector,"-",subsector];
		(*processed=Import[logdir<>"/processed"<>name<>ToString[primarysector]<>".m"];*)
		processed=Import[logdir<>"/mymedecomposed"<>name<>ToString[primarysector]<>"-"<>ToString[subsector]<>".m"];
		processed=processed[[All,2]];
		Print[Length[processed]," sectors"];
		(*{replspi,replci}={Table[replsp[[subsector]],{i,Length[processed]}],Table[replc[[subsector]],{i,Length[processed]}]};*)
		Print[logdir<>"/processed"<>name<>ToString[primarysector]<>".m  ","Imported"];
		i1=ParallelMap[Together[(tselectors[[primarysector]] #)//.{z->zval,Wtemp->1,ME[__]->1,pm->1,sp[n1,a_]:>2 sp[p,a]}//.Replp]&,processed];
		i1=i1/.combine[a__]:>Cancel[Together[Times[a]]];
		Print["i1 done"];
		(*************************************************)
		(*  Prepare replacements for     W(x1,x2,...)     *)
		(*************************************************)
		weights=MapThread[ReplaceAll,{i1,replsp}];
		(*weights=weights/.Log[a_]:>Log[Factor[a]]//.Log[a_ b_]:>Log[a//Cancel]+Log[b//Cancel]/;And[Or[!NumberQ[a],(a>0)],Or[!NumberQ[b],(b>0)]];*)
		weights=weights//.Log[c[0][a__]^pow_. b_]:>pow Log[c[0][a]]+Log[b];
		weights=weights//.Log[1/a_]:>-Log[a];
		weights=weights/.Log[a_]:>Simplify[Log[a]];
		weights=weights//ExpandLog;
		weights=weights//.Map[Log[#]->xlog[#](*/(#^logpow)*)&,activevariables];
		(*weights=Weights//.xlog[x_,pow_]^power_\[RuleDelayed]x^(power-pow) xlog[x,pow*power];*)
		(*Print[weights];
		*)
		$Assumptions=Simplify[$Assumptions&&(And@@Map[0<#&,DeleteDuplicates[Cases[weights,c[0][_,_],-1]]]),Assumptions->True];
		assoc = Association[activevariables->Transpose[{weights/.z->zval,replc/.z->zval}]];
		file=OpenWrite["/tmp/assoc"<>StringJoin[Map[ToString,activevariables]]<>".m"];
		Write[file,assoc];
		Close[file];

		test=Cases[replc,Rule[a_,b_]:>b,{0,Infinity}]/.#&/@Permutations[Map[#->0&,activevariables],{3}]//Simplify;
		Print[And@@(FreeQ[#,0]&/@test)];
		Print[FreeQ[#,sp[__]]&/@assoc];
		Off[Solve::svars ];
		Off[ExpandPower::expandpower];
		ParallelEvaluate[
			Off[Solve::svars ];
			Off[ExpandPower::expandpower];
		];
		ClearAll[assoc];

		tuples=Sort/@Subsets[activevariables,{1,Length[activevariables]}]//DeleteDuplicates;

		keys=Prepend[Map[activevariables/.Map[#->0&,#]&,tuples],activevariables];
	
		(*Print[Style["FOI Primary sector ",Bold], primarysector];*)
		ClearSystemCache[];
		ParallelEvaluate[ClearSystemCache[]];

(**************************************************************************************************************************************)
(*                                                 Main part                                                                          *)
(**************************************************************************************************************************************)
		Do[
			newassoc=<||>;
			vars=Through[{activevariables/.Thread[#->0]&,Last[#,0]&}[tuples[[i]]]];
			thisvar=Last[vars];
			prev=ReplacePart[First[vars],ToExpression[StringTake[ToString[thisvar],-1]]->thisvar,Heads->False];
			Print[Style[vars,Bold,Blue]," ",prev];
			prevassoc=Import["/tmp/assoc"<>StringJoin[Map[ToString,prev]]<>".m"];
			thisexpr=Parallelize[Map[FactorOutImplicit[#[[1]],thisvar,#[[2]]]&,prevassoc[prev]],Method->"FinestGrained",DistributedContexts->All];
			ClearAll[prevassoc];
			
			exprsimple=Parallelize[Map[RemoveDuplicates[Cancel[#[[1]]/.thisvar->0],Cancel[#[[2]]/.thisvar->0]]&,thisexpr],Method->"FinestGrained",DistributedContexts->All];
			
			Print[Style["FOI ok",Green]];
			exprsimple=ParallelMap[{Collect[#[[1]],xlog[0],Quiet[Simplify[#,TimeConstraint->0.5]]&],#[[2]]}&,exprsimple];
			(*exprsimple=exprsimple/.xlog[0]^p_. simp[a_]\[RuleDelayed]xlog[0]^p Simplify[a]/.simp[a_]\[RuleDelayed]a;*)
	
			AssociateTo[newassoc,First[vars]->exprsimple];

			file=OpenWrite["/tmp/assoc"<>StringJoin[Map[ToString,First[vars]]]<>".m"];
	 		Write[file,newassoc];
			Close[file];
			ClearAll[newassoc];
	
			ClearSystemCache[];
			ParallelEvaluate[ClearSystemCache[]];
		,{i,1,Length[tuples]}];

		ClearAll[thisexpr,exprsimple,assoc,weights,i1,processed,replspi,replci];

		assoc=Merge[Map[Import["/tmp/assoc"<>StringJoin[Map[ToString,#]]<>".m"]&,keys],First];
		
		fi=OpenWrite[logdir<>"/assoc"<>name<>ToString[primarysector]<>"-"<>ToString[subsector]<>".m"];
		Write[fi,assoc];
		Close[fi]
	];
];




(* ::Title:: *)
(*Prepare C++ codes for integrals*)


epspow


apmax=0;
epmax=0-epspow;
(*Protect[apmax,epmax];*)


(*list={13,26,39,7,9,20,22,33,35,11,12,24,25,37,38,8,10,21,23,34,36,5,6,18,19,31,32,2,16,30,3,4,15,17,28,29,1,14,27};*)
list=Table[i,{i,1,Length[tselectors]}]
(*list={3};*)


<<Integrate`
If[PrepInt===True,
For[num=1,num<=Length[list],num++,
Print[Style[ToString[num],Red]];
i=list[[num]];
Print[Style[ToString[i],Blue]];

For[sub=1,FileExistsQ[logdir<>"/assoc"<>name<>ToString[i]<>"-"<>ToString[sub]<>".m"],sub++,


assoc=Import[logdir<>"/assoc"<>name<>ToString[i]<>"-"<>ToString[sub]<>".m"];
(*assoc=Import[logdir<>"/assoc"<>ToString[i]<>".m"];*)
integ=Import[logdir<>"/integrals"<>name<>ToString[i]<>"-"<>ToString[sub]<>".m"];


(********************************************************)
int=IntPrep[assoc,integ,apmax,epmax,ExpCompactForm->True];
(********************************************************)


op=OpenWrite[logdir<>"/intprep"<>name<>ToString[i]<>"-"<>ToString[sub]<>".m"];
Write[op,int];
Close[op];
ClearAll[assoc,integ,int,op];
ClearSystemCache[];
ParallelEvaluate[ClearSystemCache[]];

];
];
];


(* ::Section:: *)
(*all*)


ClearSystemCache[]


ParallelEvaluate[ClearSystemCache[]]


(*apmax=0;
epmax=-3;*)


epmax


logdir


<<FunctionsV4`


(*list={13,26,39,7,9,20,22,33,35,11,12,24,25,37,38,8,10,21,23,34,36,5,6,18,19,31,32,2,16,30,3,4,15,17,28,29,1,14,27};*)
list=Table[i,{i,1,Length[tselectors]}];
res=0;
<<Integrate`
<<FunctionsV4`
If[GenerateCpp===True,
Do[
Print[Style[ToString[i],Red]];
For[sub=1,FileExistsQ[logdir<>"/intprep"<>name<>ToString[i]<>"-"<>ToString[sub]<>".m"],sub++,
{assoc,integ}=Import[logdir<>"/intprep"<>name<>ToString[i]<>"-"<>ToString[sub]<>".m"];
res+=CInt[integ,assoc(*//SeparateFraction*),"int"<>ToString[i]<>"ss"<>ToString[sub],apmax,epmax];

ClearSystemCache[];
ParallelEvaluate[ClearSystemCache[]];
];
,{i,list}];
];



res


Directory[]


Put[res,logdir<>"/res.m"];


Directory[]


DeleteFile["../Parni/include.hh"];
CreateFile["../Parni/include.hh"];
Do[
Print[Style[ToString[i],Red]];
For[sub=1,FileExistsQ["../beamcalc/integrals/integral"<>ToString[i]<>"ss"<>ToString[sub]<>".hh"],sub++,
f=OpenAppend["../Parni/include.hh"];
WriteString[f,"#include \"/home/tomoki/beam-functions/n3lo-TG/beamcalc/integrals/integral"<>ToString[i]<>"ss"<>ToString[sub]<>".hh\" \n"];
WriteString[f,"#include \"/home/tomoki/beam-functions/n3lo-TG/beamcalc/integrals/integral"<>ToString[i]<>"ss"<>ToString[sub]<>"names.hh\" \n"];
Close[f];
ClearSystemCache[];
ParallelEvaluate[ClearSystemCache[]];
];
,{i,list}];


f=OpenWrite["../Parni/fillmap.hh"];
WriteString[f,"inline void fill_map() {\n"];
Close[f];

Do[
For[sub=1,FileExistsQ["../beamcalc/integrals/integral"<>ToString[i]<>"ss"<>ToString[sub]<>".hh"],sub++,
Print[Style[ToString[i]<>"-"<>ToString[sub],Red]];

f=OpenAppend["../Parni/fillmap.hh"];
WriteString[f,"fill_map_integral"<>ToString[i]<>"ss"<>ToString[sub]<>"();\n"];
Close[f];
ClearSystemCache[];
ParallelEvaluate[ClearSystemCache[]];
];

,{i,list}];

f=OpenAppend["../Parni/fillmap.hh"];
WriteString[f,"};"];
Close[f];


Print["End All " ,Date[]];


logdir


(* ::Title::Closed:: *)
(*Results of integrals*)


(* ::Input:: *)
(*res=List@@Import[logdir<>"/res.m"]*)


(* ::Input:: *)
(*Import[logdir<>"/repl"]*)


(* ::Input:: *)
(*ReplInts=Apply[Rule,Import[logdir<>"/repl"],{1}];*)
(*resParni=res/.ReplInts*)
(*Plus@@%*)
(*intres=%//Expand*)
(**)


(* ::Input:: *)
(*names=(List@@Expand[res])/.{ap->1,ep->1,Lmu->1};*)
(*errrepl=Map[#->ToString[#]<>"_error"&,names];*)
(*errParni=res/.errrepl/.ReplInts;*)
(*Plus@@%*)
(**)


(* ::Input:: *)
(*resParni*)
(*Map[Coefficient[#,ep,0]&,%]//DeleteCases[#,0]&//SortBy[#,Abs]&*)


(* ::Input:: *)
(*xia=((\[Mu]^2 Exp[EulerGamma])/(4 Pi))^ep*)
(**)


(* ::Input:: *)
(*<<Prefactor`*)
(*ReplConst=numconst->2 E^- EulerGamma;*)
(*ReplLa={La-> -Lq2+Lc+Log[z1]+Log[z2]};*)
(*ReplQ=Q->(numconst E^(Lq2/2))/xT;*)
(*ReplxT={xT->(numconst E^(Lp/2))/\[Mu]};*)


(* ::Input:: *)
(*Prefactor[xT,order,numberofparticles,If[bar,0,ap],ep]pm^(If[bar,-1,1]*order ap) \[Nu]^(order ap );*)
(*xia^ numberofparticles(4Pi)^(2*numberofparticles) (2Pi)^(-numberofparticles(3-2ep))*%;*)
(*%/.ReplxT/.ReplConst//Simplify[#,Assumptions->And[\[Mu]>0,Lp>0]]&*)
(*prefac=%//Series[#,{ap,0,0},{ep,0,1}]&//Normal*)


(* ::Input:: *)
(*result=ep^epspow intres*prefac//Collect[#,ep,Simplify]&*)
(*resfile=OpenWrite[logdir<>"/result.m"]*)
(*Write[resfile,result];*)
(*Close[resfile]*)
(**)
(**)
