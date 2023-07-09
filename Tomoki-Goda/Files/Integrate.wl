(* ::Package:: *)

(*$Path=DeleteDuplicates[Join[$Path,{"~/.Mathematica/Applications/Cuba-4.2","~/beam/n3lo","~/beam/quark-loop","~/beam/selector-tomoki","~/Tools/Cuba-4.2-m","~/Tools/Cuba-4.2-l"}]];
Install["Cuhrel"];
Install["Vegasl"];
Install["Suavel"];
Install["Divonnel"];
<<ExpandPower`*)


Off[General::munfl ];


IntVerboseFunctions=1;


(*SetSharedFunction[AssocFullapep,AssocFullp];*)


(*BeginPackage["Integrate`",{"Global`","ExpandPower`"(*,"Replacements`"*)}]
IntPrep::usage="";
Begin["`Private`"];*)


(* ::Section:: *)
(*Master Function*)


(* ::Subsection::Closed:: *)
(*Old*)


(*Int[assoc_,int_,apmax_,epmax_]:=Module[{processed,assocprocessed,inteprocessed,i,j,intres,ready,epmin,apmin,res,inte,zerorep,tf},

inte=int;
processed=ProcessIntegrand[assoc, inte,apmax, epmax];
processed=ApplyTrivial[processed[[1]],processed[[2]]];
processed=processed/.{exp[a_]:> Exp[a]}/.{sel[a_]:>Cancel[a]};
processed=processed/.{J[a_]:>Cancel[a]};

PrintTemporary["Partial Simplify"];
{assocprocessed,inteprocessed}={Map[Apply[ReplaceSomec[#1,#2,0.03(3+epmax)]&,#]&,processed[[1]],{2}],processed[[2]]}//Normal;
PrintTemporary["Done"];
inteprocessed=DeleteCases[inteprocessed,0,{2}];

intres=0;
epmin=First[Sort[DeleteDuplicates[Join[{epmax},Cases[inteprocessed,ep^pow_.:>pow,-1]]]]];
apmin=First[Sort[DeleteDuplicates[Join[{apmax},Cases[inteprocessed,ep^pow_.:>pow,-1]]]]];
PrintTemporary["Start Integrating"];

For[i=apmin,i<=apmax,i++,
	For[j=epmin,j<=epmax,j++,
		ready={assocprocessed,Map[Coefficient[Coefficient[#,ap,i],ep,j]&,inteprocessed]};
		ready={ready[[1]],DeleteCases[ready[[2]],0,{2}]};
		ready=ready[[All,{35}]];
		Print[ready[[2]]];
		
		
       
		(*res=ap^i ep ^j Plus@@Parallelize[MapThread[LoopIntegrate[#2,#1,3+j]&,ready]];*)
		res=ap^i ep ^j Plus@@MapThread[LoopIntegrate[#2,#1,3+j]&,ready];
		
		If[VerboseFunctions>=2&&res=!=0,Print[res]];
		intres+=res;
	]
];
(*Print[intres];*)
Return[intres]
];*)


(* ::Subsection:: *)
(*New*)


Int[assoc_,int_,apmax_,epmax_]:=Module[{assocprocessed,inteprocessed,intres},
{assocprocessed,inteprocessed}=IntPrep[assoc,int,apmax,epmax];
intres=ParallelIntegrate[assocprocessed,inteprocessed,apmax,epmax];
Return[intres]
];



Options[IntPrep]={ExpCompactForm->True,simplify->False,IsolateSelector->True};


IntPrep[asso_,int_,apmax_,epmax_,OptionsPattern[]]:=Module[{assoc,processed,assocprocessed,inteprocessed,i,j,intres,ready,epmin,apmin,res,inte,zerorep,tf},

inte=int;

assoc=asso/.{sel[0]:>0,J[a_]:>a/.ZPow[a_]->1};
processed=AbsoluteTiming[ProcessIntegrand[assoc, inte,apmax, epmax]];
(*Return[processed];*)
Print[processed[[1]]];
processed=processed[[2]];
(*Print[processed[[1]]];*)
processed=ApplyTrivial[processed[[1]],processed[[2]]];
(*Print[processed[[2,22]]];*)
(*Print[processed];*)
If[OptionValue[ExpCompactForm]===True,
Print["ExpCompactForm"];
(*Print[Cases[processed,exp[a_]:>a Exp[a],-1]];*)
processed=processed/.{exp[a_]:>a Exp[a]}
(*kT^2 is with - sign in exp but it is taken care of in qlstartup (i.e. doesn't need to be -a Exp[a] because the expression is already multiplied by -1)*),
processed=processed/.{exp[a_]:> Exp[a]}
];
processed=processed//.{xlog[x_]:>Log[x],\[Delta]RS->1,\[Mu]->1,i0->0};
If[!OptionValue[IsolateSelector],
processed=processed/.{sel[a_]:>a}];

If[OptionValue[simplify]===True,
PrintTemporary["Partial Simplify"];
{assocprocessed,inteprocessed}=Normal[{ParallelMap[Map[Apply[ReplaceSomec[#1,#2,0.05(*(3+epmax)*)]&,#]&,#]&,processed[[1]]],processed[[2]]}],
{assocprocessed,inteprocessed}=processed;
assocprocessed=Normal/@assocprocessed;
];
PrintTemporary["Done"];
inteprocessed=DeleteCases[inteprocessed,0,{2}];
Return[{assocprocessed,inteprocessed}]
];


(* ::Subsection::Closed:: *)
(*integrate*)


Options[ParallelIntegrate]={Parallel->True,MinEpsilon->-3,MinAlpha->-1};
ParallelIntegrate[assocprocessed_,inteprocessed_,apmax_,epmax_,sector_:All,OptionsPattern[]]:=Module[{intres,epmin,apmin,i,j,k,ready,res},


intres=0;
epmin=First[Sort[DeleteDuplicates[Join[{epmax},Cases[inteprocessed,ep^pow_.:>pow,-1]]]]];
apmin=First[Sort[DeleteDuplicates[Join[{apmax},Cases[inteprocessed,ep^pow_.:>pow,-1]]]]];

If[epmin<OptionValue[MinEpsilon],Print[Style["ep, "<>ToString[epmin]<>"->"<>ToString[OptionValue[MinEpsilon]],Blue]];epmin=OptionValue[MinEpsilon]];
If[apmin<OptionValue[MinAlpha],Print[Style["ap, "<>ToString[apmin]<>"->"<>ToString[OptionValue[MinAlpha]],Blue]];apmin=OptionValue[MinAlpha]];

PrintTemporary["Start Integrating"];

For[i=apmin,i<=apmax,i++,
	For[j=epmin,j<=epmax,j++,
		ready={assocprocessed,Map[Coefficient[Coefficient[#,ap,i],ep,j]&,inteprocessed]};
		ready={ready[[1]],DeleteCases[ready[[2]],0,{2}]};
		ready=ready[[All,sector]];
		If[IntVerboseFunctions>=2,Print[ready[[2]]]];

		(*res=ap^i ep ^j Plus@@Parallelize[MapThread[LoopIntegrate[#2,#1,3+j]&,ready]];*)
		If[OptionValue[Parallel]===True,
		res=ap^i ep ^j Plus@@Parallelize[Table[LoopIntegrate[ready[[2,k]],ready[[1,k]],2(*precision*),k],{k,Length[ready[[2]]]}]],
		res=ap^i ep ^j Plus@@Table[LoopIntegrate[ready[[2,k]],ready[[1,k]],2(*precision*),k],{k,Length[ready[[2]]]}]
		];
		(*res=ap^i ep ^j Plus@@Table[LoopIntegrate[ready[[2,k]],ready[[1,k]],3+j,k],{k,Length[ready[[2]]]}];*)
		
		If[IntVerboseFunctions>=1&&res=!=0,Print[res]];
		intres+=res;
	]
];
(*Print[intres];*)
Return[intres]
];


(* ::Section:: *)
(*ProcessIntegrand*)


ProcessIntegrand[assoc_, inte_, appow_, eppow_] := Module[{intkey, newinte, newassoc, ph, epow, apow,replLog},
 
  (*Expand Assoc. and put in the form {{W[__]\[Rule]{a_,{b_\[Rule]c_},W[__]\[Rule]...},...},{...}}*)
  newassoc = AbsoluteTiming[AssocExplicit[assoc]];
   
  If[IntVerboseFunctions>=1, Print["AssocExplicit ",newassoc[[1]]]];
  newassoc=newassoc[[2]];
  
  (*Remove W[__] that doesn't apear in integral*)
  intkey = Cases[#, W[___], {0,Infinity}] & /@ inte;
  newassoc = MapThread[KeyTake, {newassoc, intkey}];
  (********************************************)
  
  (*Expand integrals in ep ap and ph2/ph3 returns in terms of W[x1,x2,x3,x4,Cos[phi],appower,eppower]*)
  ph = DeleteDuplicates[Cases[assoc, Alternatives@@angles, {0,Infinity}]];
 
  newinte = AbsoluteTiming[ParallelMap[ExpandIntegralFull[#, ph, appow, eppow]&,inte,DistributedContexts->All]];
 If[IntVerboseFunctions>=1, Print["ExpandIntegralFull ",newinte[[1]]]];
  newinte=newinte[[2]];
  
  (*************************************************************************************************)
  
  (*Determin lowest power of ap ep*)
  epow = Last[Sort[DeleteDuplicates[Join[{0},Cases[newinte, W[a___, b_, c_] :> c,{0,Infinity}]]]]];
  apow = Last[Sort[DeleteDuplicates[Join[{0},Cases[newinte, W[a___, b_, c_] :> b,{0,Infinity}]]]]];
  (*******************************)
  
  intkey = DeleteDuplicates[Cases[#, W[___],  {0,Infinity}]]& /@ newinte;
  
  (*Expand assoc. and return assoc. of W[x1,x2,x3,x4,Cos[phi],appower,eppower]*)
  newassoc = AbsoluteTiming[Parallelize[MapThread[AssocExpandFull[#1,#2, apow, epow]&,{newassoc, intkey}],DistributedContexts->All]];
  
  If[IntVerboseFunctions>=1, Print["AssocExpandFull ",newassoc[[1]]]];
  newassoc=newassoc[[2]];
  
   newassoc =Parallelize[Map[AssocFullLog,newassoc],DistributedContexts->All];
   (*Print[newinte]; 
   Print[newassoc];*)
   replLog=newassoc[[All,2]];
   newassoc=newassoc[[All,1]];
   newinte=MapThread[#1/.#2&,{newinte,replLog}];

  (*intkey = Cases[#, W[x : __], -1] & /@ newinte;
  newassoc = MapThread[KeyTake, {newassoc, intkey}];*)
  (***************************************************************************)
  Return[{newassoc, newinte}]
  
  ]



(* ::Subsection::Closed:: *)
(*AssocFullLog*)


AssocFullLog[asso_]:=Module[{tup,ser,newassoc={},ser1,i,replmu,expr,replist={},rep},
tup=asso//Keys;
newassoc={};

For[i=1,i<=Length[tup],i++,
	If[FreeQ[asso[tup[[i]]],\[Mu]R|\[Mu]],
		(*Case where the W[___] doesnt depend on \[Mu]*)
		ser1=Join[{tup[[i]]/.{W[a___,b_,c_]:>W[a,0,b,c]}}
                 ];
		ser=Join[{asso[tup[[i]]]}/.{W[a___,b_,c_]:>W[a,0,b,c]}
                 ];
                 rep=tup[[i]] ->(tup[[i]]/.W[a___,b_,c_]:>W[a,0,b,c]);
                 ,
 (****************************************************************************************)                
        (*Case where the W[___]depend on \[Mu].*)   
        (*Print[Cases[asso[tup[[i]]],Log[a_]/;!FreeQ[a,\[Mu]|\[Mu]R],{0,Infinity}]//DeleteDuplicates];*)
        replmu={Rule[\[Mu]|\[Mu]R,Exp[Lmu]]};
        expr=asso[tup[[i]]]/.replmu/.Log[a_]:>Simplify[Log[a],Assumptions->$Assumptions&&(Lmu\[Element]Reals)];

       (* Print[Cases[asso[tup[[i]]],Log[a_]/;!FreeQ[a,\[Mu]|\[Mu]R],{0,Infinity}]//DeleteDuplicates];*)
        expr=Map[{#,expr[[2]]}&,CoefficientList[expr[[1]],Lmu]];

	ser1=Table[tup[[i]]/.W[a___,b_,c_]:>W[a,ii-1,b,c],{ii,Length[expr]}];
	ser=Table[expr[[ii]],{ii,Length[expr]}];
	rep=tup[[i]] ->Sum[Lmu^(ii-1)(tup[[i]]/.W[a___,b_,c_]:>W[a,ii-1,b,c]),{ii,Length[expr]}];

	];
	
	ser=MapThread[#1->#2&,{ser1,ser}];

	newassoc=Join[newassoc,ser];
	AppendTo[replist,rep];
];



AssocFullLog::message="something is fatally wrong.";
If[!FreeQ[newassoc,\[Mu]|\[Mu]R|Lmu],Message[AssocFullLog::message];Print[DeleteDuplicates[Cases[newassoc,ph2|ph3|ap|ep,-1]]];Abort[]];
Return[{Association[newassoc],Flatten[replist]}]]


(* ::Subsection::Closed:: *)
(*AssocExplicit*)


AssocExplicit[assoc_]:=Module[{tup,res,newassoc},
tup=Reverse[Tuples[Map[{0,#}&,activevariables]]];

res=Table[
Table[
 W@@tup[[i]]->(assoc[tup[[i]]][[j]]),
{j,1,Length[assoc[tup[[i]]]]}],
{i,1,Length[tup]}
];
res=Transpose[res];
newassoc=Map[Association,res];

Return[newassoc]
];


(* ::Subsection:: *)
(*ExpandIntegralFul*)


SetAttributes[W,Constant]


SetAttributes[exp,Constant]


SetAttributes[sel,Constant]


(* ::Subsubsection::Closed:: *)
(*old*)


(*ExpandIntegralFull[integral_,phi_,appower_,eppower_]:=Module[{int=integral,epow,apow,zerorep},

int=Map[Normal[Series[#,{ap,0,appower},{ep,0,eppower}]]&,int,{1}];

(*Apply decomposition in phi as in Czakon & Haymes 's paper*)
int=int/.W[a__]:>dW[-2 ep,phi]W[a,Cos[phi]]/.RepldW;
int=Map[Expand,int];

int=int/.delta[phi] fac_:>(fac/.Cos[phi]->1);
int=int/.delta[-phi+Pi] fac_:>(fac/.Cos[phi]->-1);
(************************************************)

int=Map[Expand,int];

int=If[Head[#]===Plus,List@@#,{#}]&/@int;

(*Determine the lowers power of ep and ap*)
epow=DeleteDuplicates[Join[{eppower},Cases[int,ep^pow_.:>pow,-1]]];
epow=epow//Sort//First;
apow=DeleteDuplicates[Join[{appower},Cases[int,ap^pow_.:>pow,-1]]];
apow=apow//Sort//First;
(****************************************)


(*Apply decomposition in phi as in Czakon & Haymes 's paper*)
int=int/.gwiazda[arg_]b_:>
 (-ReplaceAll[arg,{Cos[phi]->1-x9}] ReplaceAll[ b,{Cos[phi]->1}]
  -ReplaceAll[arg,{Cos[phi]->-1+x9}] ReplaceAll[ b,{Cos[phi]->-1}] 
  +ReplaceAll[arg b,{Cos[phi]->1-x9}]
  +ReplaceAll[arg  b,{Cos[phi]->-1+x9}]);
(************************************************)

(*Laurent expansion*)
int=Map[Factor,int,{2}];


int=int/.(star[arg_]b_)/;!FreeQ[arg,x1]:>arg (b-ReplaceAll[b,Map[#->0&,Cases[arg,x1,-1]]]);
int=Map[Factor[Cancel[#]]&,int,{2}];



int=int/.(star[arg_]b_)/;!FreeQ[arg,x2]:>arg (b-ReplaceAll[b,Map[#->0&,Cases[arg,x2,-1]]]);
int=Map[Factor[Cancel[#]]&,int,{2}];

int=int/.(star[arg_]b_)/;!FreeQ[arg,x3]:>arg (b-ReplaceAll[b,Map[#->0&,Cases[arg,x3,-1]]]);
int=Map[Factor[Cancel[#]]&,int,{2}];


int=int/.(star[arg_]b_)/;!FreeQ[arg,x4]:>arg (b-ReplaceAll[b,Map[#->0&,Cases[arg,x4,-1]]]);
int=Map[Factor[Cancel[#]]&,int,{2}];

(*****************)

int=int/.W[a__]:>Sum[W[a,i,j] ep^j ap^i,{i,0,appower-apow},{j,0,eppower-epow}];


int=ParallelMap[Normal[Series[#,{ap,0,appower},{ep,0,eppower}]]&,int,{2}];


int=Map[Collect[#,ep,Collect[#,ap]&]&,int,{2}];



Return[int]
];*)


(* ::Subsubsection:: *)
(*new*)


MaxLmuPow=3


starexpand[int0_,var_]:=Module[{int},
int=int0/.(star[arg_]b_)/;!FreeQ[arg,var]:>arg (b-ReplaceAll[b,Map[#->0&,Cases[arg,var,-1]]]);
int=Map[Factor[Cancel[#]]&,int,{1}];
Return[int];
];


ExpandIntegralFull[integral_,phi_,appower_,eppower_]:=Module[{int=integral,epow,apow,zerorep},

int=Normal[Series[int,{ap,0,appower},{ep,0,eppower}]];

(*Apply decomposition in phi as in Czakon & Haymes 's paper*)
Print[int];
Print[phi];
If[phi=!={},
int=int/.W[a___]:>dW[-2 ep,phi]W[a,Cos[phi]]/.RepldW,
int=int/.W[a___]:>W[a,1]
];
int=Expand[int];

int=int/.delta[phi] fac_:>(fac/.Cos[phi]->1);
int=int/.delta[-phi+Pi] fac_:>(fac/.Cos[phi]->-1);
(************************************************)

int=Expand[int];

int=If[Head[int]===Plus,List@@int,{int}];

(*Determine the lowers power of ep and ap*)
epow=DeleteDuplicates[Join[{eppower,0},Cases[int,ep^pow_.:>pow,{0,Infinity}]]];
epow=epow//Sort//First;
apow=DeleteDuplicates[Join[{appower,0},Cases[int,ap^pow_.:>pow,{0,Infinity}]]];
apow=apow//Sort//First;
(****************************************)


(*Apply decomposition in phi as in Czakon & Haymes 's paper*)
int=int/.gwiazda[arg_]b_:>
 (-ReplaceAll[arg,{Cos[phi]->1-x9}] ReplaceAll[ b,{Cos[phi]->1}]
  -ReplaceAll[arg,{Cos[phi]->-1+x9}] ReplaceAll[ b,{Cos[phi]->-1}] 
  +ReplaceAll[arg b,{Cos[phi]->1-x9}]
  +ReplaceAll[arg  b,{Cos[phi]->-1+x9}]);
(************************************************)

(*Laurent expansion*)
int=Map[Factor,int,{1}];

int=Fold[starexpand[#1,#2]&,int,activevariables];
(*****************)


int=int/.W[a___]:>Sum[W[a,i,j] ep^j ap^i,{i,0,appower-apow},{j,0,eppower-epow}];


int=Map[Normal[Series[#,{ap,0,appower},{ep,0,eppower}]]&,int,{1}];


int=Map[Collect[#,ep,Collect[#,ap]&]&,int,{1}];



Return[int]
];


(* ::Subsection::Closed:: *)
(*RepldW*)


RepldW=
{dW[-2ep,phi_]:>
 (delta[phi]+delta[-phi+Pi]+
 ep gwiazda[2/((-2+x9) x9)]
+ep^2 gwiazda[(Log[16]-2 Log[-(-2+x9) x9])/((-2+x9) x9)]
+ep^3 gwiazda[(Pi^2+12 Log[2]^2-12 Log[2] Log[-(-2+x9) x9]+3 Log[-(-2+x9) x9]^2)/(3 (-2+x9) x9)]
(*+ep^4 gwiazda[(8 Log[2]^3+Pi^2 Log[4]-(Pi^2+12 Log[2]^2) Log[-(-2+x9) x9]+Log[64] Log[-(-2+x9) x9]^2-Log[-(-2+x9) x9]^3-6 PolyGamma[2,1])/(3 (-2+x9) x9)]*))
};


(* ::Subsection::Closed:: *)
(*AssocExpandFull*)


AssocExpandFull[assoc_,keys_,appow_,eppow_]:=Module[{asso},
asso=AssocFullapep[assoc,appow,eppow];
asso=AssocFullph[asso];
asso=KeyTake[asso,keys];
Return[asso]];


(* ::Subsubsection::Closed:: *)
(*AssocFullapep*)


AssocFullapep::message="something is fatally wrong.";


AssocFullapep[asso_,appow_,eppow_]:=Module[{tup,ser,newassoc={},ser1,i,rest},
tup=asso//Keys;
For[i=1,i<=Length[tup],i++,
ser=asso[tup[[i]]][[1]];

If[ser===0,
rest=0;
ser=1,

(*rest=ser//.{J1[a_]:>a,J2[a_]:>1};*)
rest=1;
If[Cases[ser,J2[a_]:>a,-1]==={},Print[ser];Abort[]];

(*ser=First[Cases[ser,J2[a_]:>a,-1]]/.{dW[a__]:>S[-2ep]/2}/.ReplSdim;(*pick only the part relevant for the expansion*)*)
ser=ser/.{dW[a__]:>S[-2ep]/2}/.ReplSdim//.ZPow[_]->1(*//.{J1[a_]:>a(*,J2[a_]:>a,J3[a_]:>a*)}*);
];

(*ser=Normal[Series[ser,{ap,0,appow},{ep,0,eppow}]];*)
ser=ser/.Alternatives[J1,J2,J3,J4][a_]:>Normal[Series[a,{ap,0,appow},{ep,0,eppow}]];
ser1=Join@@Table[Replace[tup[[i]],{W[a___]:>W[a,jj,ii]},{0}],{jj,0,appow},{ii,0,eppow}];
ser=Join@@Table[{ rest Coefficient[Coefficient[ser,ap,jj],ep,ii],asso[tup[[i]]][[2]]},{jj,0,appow},{ii,0,eppow}];

ser=MapThread[#1->#2&,{ser1,ser}];
newassoc=Join[newassoc,ser];

If[!FreeQ[ser,ap|ep],Message[AssocFullapep::message];Print[DeleteDuplicates[Cases[ser,ap|ep,{0,Infinity}]]];Abort[]];
];

Return[Association[newassoc]]
]


(* ::Subsubsection::Closed:: *)
(*old*)


(*AssocFullapep[asso_,appow_,eppow_]:=Module[{tup,ser,newassoc={},ser1,i},
tup=asso//Keys;
For[i=1,i<=Length[tup],i++,
ser=asso[tup[[i]]][[1]]/.{dW[a__]:>S[-2ep]/2}/.ReplSdim;
ser=Normal[Series[ser,{ap,0,appow},{ep,0,eppow}]];
ser1=Join@@Table[Replace[tup[[i]],{W[a_,b_,c_,d_]:>W[a,b,c,d,jj,ii]},{0}],{jj,0,appow},{ii,0,eppow}];
ser=Join@@Table[{Coefficient[Coefficient[ser,ap,jj],ep,ii],asso[tup[[i]]][[2]]},{jj,0,appow},{ii,0,eppow}];
ser=MapThread[#1->#2&,{ser1,ser}];
newassoc=Join[newassoc,ser];

If[!FreeQ[ser,ap|ep],Message[AssocFullapep::message];Print[DeleteDuplicates[Cases[ser,ap|ep,-1]]];Abort[]];
];
Return[Association[newassoc]]
]*)


(* ::Subsubsection::Closed:: *)
(*AssocFullph*)


AssocFullph[asso_]:=Module[{tup,ser,newassoc={},ser1,i,ph},
tup=asso//Keys;
newassoc={};

For[i=1,i<=Length[tup],i++,
(*If[FreeQ[Quiet[Simplify[asso[tup[[i]]],TimeConstraint\[Rule]0.1]],ph2|ph3],*)
	If[FreeQ[asso[tup[[i]]],ph2|ph3],
		(*Case where the W[___] doesnt depend on phi. produce replacement W[__]\[Rule]W[__,1,__]*)
		ser1=Join[{tup[[i]]/.{W[a___,b_,c_]:>W[a,1,b,c]}},
                 Table[tup[[i]]/.{W[a___,b_,c_]:>W[a,ii,b,c]},{ii,{1-x9,-1+x9,-1}}]
                 ];
		ser=Join[{asso[tup[[i]]]},
                 Table[tup[[i]]/.{W[a___,b_,c_]:>{W[a,1,b,c],{}}},{ii,{1-x9,-1+x9,-1}}]
                 ],
 (****************************************************************************************)                
        (*Case where the W[___]depend on phi.*)   
		ph=Cases[asso[tup[[i]]],ph2|ph3,-1]//First;
		ser1=Table[tup[[i]]/.W[a___,b_,c_]:>W[a,ii,b,c],{ii,{1,1-x9,-1+x9,-1}}];
		ser=Table[(asso[tup[[i]]]/.Cosph2Repl[{Cos[ph]->ii}]),{ii,{1,1-x9,-1+x9,-1}}]
	];
	
	ser=MapThread[#1->#2&,{ser1,ser}];
	newassoc=Join[newassoc,ser];
];

AssocFullph::message="something is fatally wrong.";
If[!FreeQ[newassoc,ph2|ph3|ap|ep],Message[AssocFullph::message];Print[DeleteDuplicates[Cases[newassoc,ph2|ph3|ap|ep,-1]]];Abort[]];
Return[Association[newassoc]]]



Cosph2Repl[replacement_List]:=Module[{ph,res},
ph=Cases[replacement,ph2|ph3,-1]//First;
If[Abs[(Cos[ph]/.replacement)]=!=1,
res={Sin[ph]->Power[1-Cos[ph]^2,1/2]/.replacement,
Csc[ph]->Power[1-Cos[ph]^2,-1/2]/.replacement}//Simplify,
res={Sin[ph]->Power[1-Cos[ph]^2,1/2]/.replacement}//Simplify];
Return[Join[res,replacement]]
];



(* ::Subsection::Closed:: *)
(*FindZeroW*)


FindZeroW[assoc_]:=Module[{tup,keytup,repall,frame,rep,expr,pos,i},
tup=Subsets[activevariables];
keytup=activevariables/.Map[#->0&,#]&/@tup;
repall={};
For[i=1,i<=Length[keytup],i++,
expr=assoc[keytup[[i]]];
pos=Position[expr,{0,_}];
frame=Table[{},{j,Length[expr]}];
rep=ReplacePart[frame,Map[#:>{(W[Sequence@@keytup[[i]]]->0)}&,Flatten[pos]]];
AppendTo[repall,rep];
];
repall=repall//Transpose;
repall=Map[Flatten,repall];
Return[repall];
]


(* ::Subsection:: *)
(**)


(* ::Section::Closed:: *)
(*ApplyTrivial*)


ApplyTrivial[assoc_,inte_]:=Module[{newinte={},newassoc=assoc,repl,tup,i},
For[i=1,i<=Length[assoc],i++,
tup=Keys[assoc[[i]]];
repl={};
(*If[i\[Equal]22,Print[assoc]];*)
Do[If[assoc[[i]][tup[[j]]][[1]]===0||MatchQ[assoc[[i]][tup[[j]]][[1]],W[___]],

AppendTo[repl,tup[[j]]->assoc[[i]][tup[[j]]][[1]]];
newassoc[[i]]=KeyDrop[newassoc[[i]],{tup[[j]]}]
],{j,1,Length[tup]}];
(*Print[repl];*)
(*If[i==22,Print[repl]];*)
AppendTo[newinte,inte[[i]]//.repl];
];
Return[{newassoc,newinte}]];


(* ::Section::Closed:: *)
(*ReplaceSomec*)


ClearAll[ReplaceSomec]
ReplaceSomec[exp_,repl_,time_]:=Module[{subrepl,subreplnew,remrepl,subreplout,full,take,out},

(*subrepl=Cases[repl,re:((c[zero_][b__]->d_)/;(zero=!=0))];*)
subrepl=Cases[repl,re:((c[zero_][b__]->d_)/;(zero=!=0))]//Simplify;
If[!FreeQ[subrepl,c[0][__]],Print["Error"];
	subrepl=DeleteCases[repl, (c[0][__]->_)];
];

subreplout=Map[Factor[Numerator[#[[2]]]]&,subrepl];
subreplout=Map[If[Head[#]===Times,List@@#,{#}]&,subreplout];


full=Join@@subreplout;
take=Join@@Map[If[Count[full,#,1]>1,{#},{}]&,DeleteDuplicates[full]];
subreplout=Map[Times@@Cases[#,Alternatives@@take]&,subreplout];

subreplnew=MapThread[#1[[1]]->Cancel[#1[[2]]/#2]&,{subrepl,subreplout}];

subrepl=MapThread[#1[[1]]->#2 #1[[1]]&,{subrepl,subreplout}];

remrepl=Cases[repl,re:(c[a_][b__]->d_)/;a===0]//Simplify;

out={Evaluate[Quiet[Simplify[exp/.subrepl,TimeConstraint->time]]],Join[remrepl,subreplnew]};
out=RemoveSqrt[out[[1]],out[[2]],x7];
out=RemoveSqrt[out[[1]],out[[2]],x8];

Return[out]
];



(* ::Section::Closed:: *)
(*LoopIntegrate*)


LoopIntegrate[int_,repl_,prec_,k_:0]:=Module[{outint=0,
i,midint,inte,repla,pos,intevars,in, intvar,xi1,xi2,xi3,xi4,xi6,xi7,xi8,xi9,intei,intcomp,singvar,intevarsi,power},
(*pos=Table[i,{i,Length[int]}];
pos=Delete[pos,Position[int,0,1]];*)
If[int=!={},Print[int]];
(*inte=int[[{8}]];*)
inte=Delete[int,Position[int,0,1]];

(*inte={Plus@@inte};*)
(*inte=inte[[{8}]];*)
(*Print[inte];*)

intevars=Map[{
			If[!FreeQ[#,x1],{xi1,0,1},{}],
			If[!FreeQ[#,x2],{xi2,0,1},{}],
			If[!FreeQ[#,x3],{xi3,0,1},{}],
			If[!FreeQ[#,x4],{xi4,0,1},{}],
			If[!FreeQ[#,x9],{xi9,0,1},{}]
			}&,inte
			];
   
intevars=DeleteCases[intevars,{},{2}];
(*in=Map[Association,in];*)

If[inte==={}||inte==={0},Return[0]];
For[i=1,i<=Length[inte],i++,
intevarsi=intevars[[i]];
	
	singvar=DetectUncancelled[inte[[i]],repl];
	If[singvar=!={},
	singvar=singvar/.{x1->xi1,x2->xi2,x3->xi3,x4->xi4};
	intevarsi=Join[Cases[intevarsi,a_/;!FreeQ[a,Alternatives@@singvar]],Cases[intevarsi,a_/;FreeQ[a,Alternatives@@singvar]]];
	];

If[IntVerboseFunctions>=1,
	If[k=!=0,
		Print[Style[ToString[k]<>" ",Green],inte[[i]],
		"  ",
		intevarsi,
		"  ",
		Cases[Cases[inte[[i]],W[__],-1]/.repl,HeavisideTheta[a_]:>Simplify[HeavisideTheta[a]],-1]
		],
		Print[inte[[i]],"  ",intevars[[i]]]
	];
];		
	repla=KeyTake[Association[repl],Cases[inte[[i]],W[__],-1]];
	repla=repla//Normal;
	repla=repla/.HeavisideTheta[a_]:>Simplify[HeavisideTheta[a]];
	intei=inte[[i]];
	(*{intei,repla}=DivideSectors[inte[[i]],repla];*)
	
	
	
	If[inte[[i]]=!=0,
		If[FreeQ[inte[[i]],x1],xi1=0.1];
		If[FreeQ[inte[[i]],x2],xi2=0.1];
		If[FreeQ[inte[[i]],x3],xi3=0.1];
		If[FreeQ[inte[[i]],x4],xi4=0.1];
		If[FreeQ[inte[[i]],x9],xi9=0.1];
		
		(*If[!FreeQ[repla,HeavisideTheta[_]],
		in=Integrand4[1/5 inte[[i]]/.{x3->x3/5},repla/.{x3->x3/5},xi1,xi2,xi3,xi4,xi6,xi7,xi8,xi9],
		in=Integrand4[inte[[i]],repla,xi1,xi2,xi3,xi4,xi6,xi7,xi8,xi9]];*)
	in=Integrand4[intei,repla,xi1,xi2,xi3,xi4,xi6,xi7,xi8,xi9];
	(*intcomp=Compile[{xi1,xi2,xi3,xi4,xi6,xi7,xi8,xi9},Integrand4[intei,repla,xi1,xi2,xi3,xi4,xi6,xi7,xi8,xi9]];*)
	power=Length[DeleteCases[{xi1,xi2,xi3,xi4,xi6,xi7,xi8,xi9},a_/;NumberQ[a]]];
	Print[Style[power,Red], "variables"];
	midint=Vegas[If[xi1 xi2 xi3 xi4 >10^-7,in,0],
		Sequence@@(intevarsi),{xi6,0,1},{xi7,0,1},{xi8,0,1},
		NStart->100000, NIncrease->50000, NBatch->10000,MaxPoints->10000000, AccuracyGoal->0, PrecisionGoal->2, 
		Verbose->1, PseudoRandom->False,Compiled->True(*,SharpEdges\[Rule]True*)];
(*	midint=Suave[in,
		Sequence@@(intevars[[i]]),{xi6,0,1},{xi7,0,1},{xi8,0,1},
		MaxPoints->5000000, NNew\[Rule]10^(power+1),NMin\[Rule]5,Flatness\[Rule]10,AccuracyGoal\[Rule]0, PrecisionGoal\[Rule]3, Verbose->1, PseudoRandom->False];*)
(*	midint=Cuhre[in,
		Sequence@@(intevars[[i]]),{xi6,0,1},{xi7,0,1},{xi8,0,1},
		MaxPoints\[Rule]10000000, AccuracyGoal\[Rule]1, PrecisionGoal\[Rule]3,Verbose->1,
		Key\[Rule]0,MinPoints\[Rule]20];*)
		
	If[IntVerboseFunctions>=2,Print[midint]];
	midint=Flatten[midint];
	ClearAll[xi1,xi2,xi3,xi4,xi9],
	midint={0,0,0}];
	
	If[midint===$Failed||!FreeQ[midint,$Failed],
		op=OpenAppend["~/beam/quark-loop/failedintegrals.m"];
		Write[op,{inte[[i]],repla}];
		Close[op];
		Abort[]
	];
	outint=+midint
	
];
Return[outint]

];

$Assumptions=Simplify[$Assumptions&&(And@@((0<#<1)&/@{x1,x2,x3,x4,x5,x6,x7,x8}))&&-1/10<ap<1/10&&-1/10<ep<1/10,Assumptions->True];


DivideSectors[int_,repl_]:=Module[{ht,var,sol},
(*ht=DeleteDuplicates[Cases[int/.repl,HeavisideTheta[arg_]\[RuleDelayed]arg,-1]];*)
ht=DeleteDuplicates[Cases[Cases[int,W[__],-1]/.repl,HeavisideTheta[arg_]:>arg,-1]];
(*ht=Cases[Normal[KeyTake[repl,DeleteDuplicates[Cases[inte[[i]],W[__],-1]]]],HeavisideTheta[a_]:>Simplify[HeavisideTheta[a]],-1];*)
If[ht==={},Return[{int,repl}]];

Print[Simplify[Reduce[And@@Map[#<0&,ht]]]];

var=Map[Length[Variables[#]]&,ht];
ht=Pick[ht,var,1];

Print[ht];


If[ht==={},Return[{int,repl}]];
sol=Solve[#==0,Variables[#]]&/@ht;

sol=Map[Flatten,sol];


If[Times@@Map[Length,sol]=!=1,
Print[sol]];
sol=Map[Flatten,sol];


sol=ReplaceAll[Map[First,sol],{(Rule[a_,b_]):>({b,Rule[a,a b]})}];
sol=sol//Transpose;
sol={Times@@sol[[1]],sol[[2]]};
Print[sol];
Return[{sol[[1]] int/.sol[[2]],repl/.sol[[2]]}]
]


counter=1;
ClearAll[Integrand4]

Integrand4[integrals_,repl_,xi1_Real,xi2_Real,xi3_Real,xi4_Real,xi6_Real,xi7_Real,xi8_Real,xi9_Real]:=Module[
{num,replnum,rule},
rule=Rule@@@Transpose[{{x1,x2,x3,x4,x6,x7,x8,x9},{xi1,xi2,xi3,xi4,xi6,xi7,xi8,xi9}}];
replnum=Association[repl/.HeavisideTheta[a_]:>Simplify[HeavisideTheta[a/.rule]]];
replnum=Normal[Map[Apply[ReplaceAll[#1/.rule,#2/.rule]&,#]&,replnum]];

If[counter<=2,Print[replnum,{xi1,xi2,xi3,xi4,xi6,xi7,xi8,xi9}]];
counter+=1;
Return[integrals/.replnum/.rule]
]


(*RemoveSqrt[expr_,xi_]:=Module[{out},
out=expr/.{xi->(1-xi^2)^2};
out=4 xi (1-xi^2) out;
Return[ExpandPower[Cancel[ExpandPower[out]]]]
]*)


(* ::Section::Closed:: *)
(*others*)


RemoveSqrt[expr_,repl_,xi_]:=Module[{out1,out2},
out1=expr/.{xi->(1-xi^2)^2};
out2=repl/.{xi->(1-xi^2)^2};
out1=4 xi (1-xi^2) out1;
Return[{Cancel[ExpandPower[out1]],out2}]
]


DetectUncancelled[int_,repl_]:=Module[{rand={1,1},numrep,intI},
(*rand=Map[#->RandomInteger[{2,10}]&,{x6,x7,x8,x9}];*)
While[!DuplicateFreeQ[rand],
rand=Table[RandomInteger[{2,10}],{i,4}]
];

rand=MapThread[#1->#2&,{{x6,x7,x8,x9},rand}];


numrep=repl/.rand/.{Rule[W[a__],b_]:>Rule[W[a],ReplaceAll@@b]};
intI=int/.rand;
intI=intI/.numrep//Together//Cancel;
(*Print[intI];*)
intI=intI//FactorList//Chop;
intI=Cases[intI,{fac_. xi:(x1|x2|x3|x4),a_}/;(a<0),{1}];
If[intI=!={},
Print[Style["Uncancelled variable" <>ToString[intI],Red]];
Return[Cases[intI,x1|x2|x3|x4,-1]]
(*Return[First[Cases[intI,x1|x2|x3|x4,-1]]]*)
];
Return[{}]
]


SeparateFraction[Wrepl_]:=Module[{out},
(*produces n[_][__]/d[_][__] from c[_][__] so that n and d are not in the form of fraction.*)
out=Wrepl//ExpandPower;
out=out//.{(Rule[c[a_][b__],expr_]):>Sequence[Rule[n[a][b],Numerator[expr]],Rule[d[a][b],Denominator[expr]]]};
out=out//.{(Rule[cT[a_][b__],expr_]):>Sequence[Rule[nT[a][b],Numerator[expr]],Rule[dT[a][b],Denominator[expr]]]};
out=out/.{c[a_][b__]:>n[a][b]/d[a][b],cT[a_][b__]:>nT[a][b]/dT[a][b]};

out=out/.Rule[W[a__],{b_,c_}]:>Rule[W[a],RemoveDuplicates[b,c]];


out=out/.Rule[W[a__],{b_,c_}]:>Rule[W[a],{Cancel[b/.{Exp[ex_]:>Exp[Cancel[Together[ex]]]}],c}];
(*out=out/.Rule[W[a__],{b_,c_}]:>Rule[W[a],{b/.{Exp[ex_]:>Exp[Together[ex]]},c}];*)
out=out/.Rule[W[a__],b_]:>Rule[W[a],ApplyNumber[b]];
Return[out]
];


Options[ApplyNumber]={FactorOut->False}


ApplyNumber[list_,OptionsPattern[]]:=Module[{expr,funcs,repl,repln,replrem,subreplout,full,take,subreplnew,repldn,repln2},
(* pull out factors that occurs multiple times in the d, n or c functions*)
(*should be used like W[__]\[Rule]ApplyNumber[{_,{__}}]*)
expr=list[[1]];
funcs=DeleteDuplicates[Cases[expr,d[_][__]|n[_][__]|c[_][__]|cT[_][__],-1]];
repl=Normal[KeyTake[Association[list[[2]]],funcs]];


If[OptionValue[FactorOut]===True,
	(*Identical Factors*)
	subreplout=Map[Factor[#[[2]]]&,repl];
	(*subreplout=Map[If[Head[#]===Times,List@@#,{#}]&,subreplout];*)
	subreplout=Map[DeleteCases[(*Power@@@*)FactorList[#],{_Integer,1},{1}]&,subreplout];
	
	
	full=Join@@subreplout;
	take=Join@@Map[If[Count[full,#[[1]],2]>1,{#[[1]]},{}]&,DeleteDuplicates[full]];
	Print[take];
	subreplout=Map[Times@@Power@@@Cases[#,{Alternatives@@take,_}]&,subreplout];(*Factors to be pulled out*)

	(*Print[subreplout];*)
	subreplnew=MapThread[#1[[1]]->Cancel[#1[[2]]/#2]&,{repl,subreplout}];(*new replacement after the fators are pulled out*)
	repldn=MapThread[#1[[1]]->#2 #1[[1]]&,{repl,subreplout}];(*replacements to pull out the factors*),
	
	subreplnew=repl;
	repldn={};
];

repln=Cases[subreplnew,Rule[a_,b_]/;NumberQ[b],{1}];(*replacements only with number*)
If[IntVerboseFunctions>=2,Print[repln]];
replrem=Complement[subreplnew,repln];(*remaining*)

Return[{expr/.repldn/.repln/.Exp[a_]:>Exp[Cancel[a]]//Cancel,replrem}]
]


(* ::Section:: *)
(*End*)


(*End[];
EndPackage[];*)
