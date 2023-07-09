(* ::Package:: *)

(*name="test";
positions=All;*)


(*SetDirectory["~/beam-functions/n3lo-TG/Tomoki Minimum"];*)
(*SetDirectory["~/beam-functions/n3lo-TG/Tomoki Minimum"];*)


(*Import["./name-nnlo.wl"];*)
(*Import["./name.wl"];*)


(*numberofparticles=2;*)
activevariables=Table[ToExpression["x"<>ToString[i]],{i,2(numberofparticles-1)}];(*not at all sure*)


Decomposition=True;
GenerateIntegrals=True;
FOI=True;

(*bar=False;*)
(*bar=True;*)
IFOContinue=False;
PrepInt=True;
GenerateCpp=True;

(*logdir="../log2"*)
(*If[bar===True,
logdir="../"<>name<>"logbar",
logdir="../"<>name<>"log"];*)
If[bar===True,
logdir="./"<>name<>"blog",
logdir="./"<>name<>"log"
];


If[!DirectoryQ[logdir],CreateDirectory[logdir]];


intdir= logdir<>"/integrals"
If[!DirectoryQ[intdir],CreateDirectory[intdir]];
Print[Style["files for C++ are produced at "<>intdir,Cyan]];


(* ::Section:: *)
(*Definitions*)


(*SetDirectory["/home/tomoki/beam_git/beam-functions/n3lo-TG/Tomoki Minimum"];*)
(*SetDirectory["/media/tomoki/HAMA/beam-functions/n3lo-TG/Tomoki Minimum"];*)

Directory[]
SetSharedVariable[zval];

(*zval=1/2;*)

(*Unprotect[n1,n2,l1,l2,l3,p,pm,z,l10,l20,l30,l1T,l2T,l3T,l1m,l2m,l3m,th1,ch1,ch2,ph1,ph2,ph3,sel,exp,ap,ep,c,sp,n,d,S,dW,W,times,J,prop];
Unprotect[x1,x2,x3,x4,x5,x6,x7,x8,x9];*)
times::usage="";
sel::usage="";
exp::usage="";
ap::usage="";
ep::usage="";
sp::usage="";
spT::usage="";
sp1::usage="";
c1::usage="";
c::usage = "";
cT::usage = "";
n::usage = "";
d::usage = "";
nT::usage = "";
dT::usage = "";
x1::usage = "";
x2::usage = "";
x3::usage = "";
x4::usage = "";
x5::usage = "";
x6::usage = "";
x7::usage = "";
x8::usage = "";
x9::usage = "";
th1::usage = "";
ch1::usage = "";
ch2::usage = "";
ph1::usage = "";
ph2::usage = "";
ph3::usage = "";
W::usage = "";
dW::usage = "";
S::usage = "";
prop::usage = "";
J::usage = "";
J1::usage = "";
J2::usage = "";
J3::usage = "";
J4::usage = "";
l1::usage = "";
l2::usage = "";
l3::usage = "";
n1::usage = "";
n2::usage = "";
p::usage = "";
pm::usage = "";
z::usage = "";
zeta::usage=""
l1m::usage = "";
l2m::usage = "";
l3m::usage = "";
l1T::usage = "";
l2T::usage = "";
l3T::usage = "";
l10::usage = "";
l20::usage = "";
l30::usage = "";
li::usage = "";
liT::usage = "";
lim::usage = "";
li0::usage = "";
li0li::usage = "";
limli::usage = "";
liTli::usage = "";
ni::usage = "";
sr::usage = "";
ME::usage = "";
ME0::usage = "";
Wtemp::usage = "";
measureorig = "";
angles::usage= "";
cos::usage= "";
SimpleList::usage= "";
l::usage= "";
ZPow::usage= "";
\[Mu]::usage= "";
\[Mu]R::usage= "";
Lmu::usage= "";
SCALE::usage= "";


Protect[n1,n2,l1,l2,l3,p,pm,z,l10,l20,l30,l1T,l2T,l3T,l1m,l2m,l3m,th1,ch1,ch2,ph1,ph2,ph3,sel,exp,ap,ep,c,sp,n,d,S,dW,W,times,J,prop,l];
Protect[x1,x2,x3,x4,x5,x6,x7,x8,x9];


If[NumberQ[zval],Print[Style["z is set to "<>ToString[Numerator[zval]]<>"/"<>ToString[Denominator[zval]],Red]],
zval=1/2;Print[Style["z is set to"<>ToString[zval],Red]]];


li=Map[ToExpression["l"<>ToString[#]]&,Table[i,{i,1,numberofparticles}]];
ni={n1,n2};
liT=Map[ToExpression[ToString[#]<>"T"]&,li];
lim=Map[ToExpression[ToString[#]<>"m"]&,li];
li0=Map[ToExpression[ToString[#]<>"0"]&,li];

li0li=Association@@Join[MapThread[#1->#2&,{li0,li}],MapThread[#1->#2&,{li,li0}],Map[#->1&,ni],{p->pm/2}];
limli=Association@@Join[MapThread[#1->#2&,{lim,li}],MapThread[#1->#2&,{li,lim}]];
liTli=Association@@Join[MapThread[#1->#2&,{liT,li}],MapThread[#1->#2&,{li,liT}]];





angles={th1,ch1,ph1,ch2,ph2,ph3};
cos=Map[Cos,angles]


Protect[li,li0,liT,lim,ni,li0li,liTli,limli,angles,cos,activevariables];


$Assumptions=Simplify[$Assumptions&&(And@@Map[0<#&,Join[lim,liT,li0]]),Assumptions->True];


$Assumptions=Simplify[$Assumptions&&sp[__]>0,Assumptions->True];


SetAttributes[#, Orderless]&/@{sp,spT,c};
SetAttributes[sp1,Orderless]


$Assumptions=$Assumptions&&
And@@Map[0<#<1(*&&0<assocxy[#]<1*)&,{x1,x2,x3,x4,x5,x6,x7,x8,x9}]&&
And@@Map[0<#<Pi&,{th1,ch1,ch2,ph1,ph2,ph3}]&&
0<zeta<1&&0<z<1;
$Assumptions=$Assumptions//Simplify[#,Assumptions->True]&;


(* ::Section:: *)
(*Repl*)


ReplFuncArgSum = {sp[arg1_,arg2_+arg3_]:>sp[arg1,arg2]+sp[arg1,arg3],
                  spT[arg1_,arg2_+arg3_]:>spT[arg1,arg2]+spT[arg1,arg3],
                  sp[Times[-1, arg1_], arg2_] :> -sp[arg1, arg2],sp[lj_,lj_]/;MemberQ[li,lj]->0,
                  sp[p,p]->0,sp[n1,n1]->0,sp[n2,n2]->0
		  };
		  
ReplFuncspArgSum = {sp[arg1_,arg2_+arg3_]:>sp[arg1,arg2]+sp[arg1,arg3],
                  sp[Times[-1, arg1_], arg2_] :> -sp[arg1, arg2],sp[lj_,lj_]/;MemberQ[li,lj]->0,
                  sp[p,p]->0,sp[n1,n1]->0,sp[n2,n2]->0
		  };
ReplspTArgSum = { spT[arg1_,arg2_+arg3_]:>spT[arg1,arg2]+spT[arg1,arg3]};
                
		  
ReplspT = {spT[k1_, k2_] :> 
           1/2 sp[k1,n1] sp[k2,n2] + 1/2 sp[k1,n2] sp[k2,n1] - sp[k1,k2],sp[lj:Alternatives@@li,lj:Alternatives@@li]->0};


ReplSdim = {S[dim_] :> (2 Pi^((dim + 1)/2))/Gamma[1/2 (dim + 1)]};

(*
 * Scalar products (d-dim, d-2-dim and 3-dim)
 *)

Replsp = {sp[v1_, v2_] :> 
          v1[[1]] v2[[1]] - Sum[v1[[i]] v2[[i]], {i, 2, Length[v1]}], 
          spT[v1_, v2_] :> Sum[v1[[i]] v2[[i]], {i, 2, Length[v1]-1}]
};


Replp={sp[mom_,n1]:>2/pm sp[mom,p],sp[n2,p]->pm,pm->1};


ReplT2Sp={ljT:Alternatives@@liT:>Power[sp[liTli[ljT],n1]sp[liTli[ljT],n2],1/2]};
ReplEn2Sp={lj0:Alternatives@@li0:>(sp[li0li[lj0],n1]+sp[li0li[lj0],n2])/2};


ReplLaurent = {
Power[arg_, -1+fac_]/;!FreeQ[{x1, x2, x3, x4, x5, x6, x7, x8, x9}, arg] ->
               1/(fac) delta[arg] + star[Power[arg, -1]]
               + Power[fac, 1] star[Power[Log[arg],1] Power[arg, -1]]
               + 1/2 Power[fac, 2] star[Power[Log[arg],2] Power[arg, -1]]
};



ScaleZ={sp[l_,plus:Alternatives[p,n1]]:>sp[l,plus]/(1-z),sp[l_,n2]:>sp[l,n2](1-z)};


<<SecDecV4`
<<FunctionsV4`
<<SelectorV4`
