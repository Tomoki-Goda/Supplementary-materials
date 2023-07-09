(* ::Package:: *)

Print[Style["SimpleRelations",Red]];


BeginPackage["SimpleRelations`",{"Global`","ExpandPowerV2`","SecDecV4`"}]


ForceFindSimpleRelations::usage="";

Begin["`Private`"];


(* ::Section::Closed:: *)
(*Storage*)


(*SortOutDuplicates[in_,new_]:=Module[{list,elements,tf={False},numi=1,take,rem},
If[in==={},
If[VerboseFunctions>=1,
Print["Simple Expressions to solve ", new]];
Return[new]];
list=Map[FactorList,DeleteDuplicates[Simplify[in]]];
elements=Map[#[[1]]&,list,{2}];
elements=DeleteDuplicates[Flatten[elements]];
elements=DeleteCases[elements,a_/;NumberQ[a]]//Simplify;
While[Count[tf,True]<2&&numi<=Length[elements],
tf=Map[NumberQ[Cancel[elements[[numi]]/#]]&,elements];
(*tf=Map[NumberQ[Cancel[elements[[numi]]/#]]&,elements];*)
numi+=1;
];
take=Take[elements,First[Position[tf,True]]];
rem=Delete[elements,Position[tf,True]];
Return[SortOutDuplicates[rem,Join[new,take]]];
];*)


(*ListPower[expr_List,list_]:=Module[{i,out={},pow},
For[i=1,i<=Length[list],i++,
pow=Cases[expr,{list[[i]],a_}:>a];
pow=pow/.{}:>{0};
AppendTo[out,pow]
];
Return[out//Flatten]];*)


(*ClearAll[PullOutComFac]
Options[PullOutComFac]={NumericFactor->False};
PullOutComFac[list_,OptionsPattern[]]:=Module[{factlist,factorall,numfac=1},
factlist=FactorList/@list;
factorall=Cases[factlist,{a_,b_}:>a,{2}];
factorall=factorall//DeleteDuplicates//DeleteCases[#,_Integer]&//Sort;
factlist=ListPower[#,factorall]&/@factlist;
factlist=Map[First[Sort[#]]&,Transpose[factlist]];
If[OptionValue[NumericFactor]===True,numfac=First[FactorTermsList[Plus@@list]]];
Return[numfac Inner[#1^#2&,factorall,factlist,Times]]];
*)


(*MultiplyList[list_]:=Module[{fac,el,out={},i},
For[i=1,i<=Length[list],i++,
fac=FactorTermsList[list[[i]]];
fac=Abs[fac[[1]]];
(*Print[fac];*)
el=list[[i]]/fac;
out=Join[out,Table[el,{j,fac}]]
];
Return[out]
];*)


(*FindSetWithFactor[expr_,repl_,var_]:=Module[{loop,i,exprit,list1,found,tf,pos,comfac,cand,listset,listset2,retlist={}},
loop=1;
exprit=expr;
i=1;
While[exprit=!=0&&loop<=150,
If[i==(Length[list1]-1),Return[{exprit,retlist}];Break[]];
list1=exprit//Expand;
list1=List@@list1;
found=False;
(*Print[Length[list1]];*)
i=1;
While[found=!=True,
If[i==(Length[list1]-1),Break[]];
tf=Map[Cancel[list1[[i]]/#/.Simplify[repl/.var->0]/.var->0]&,Drop[list1,i]];
(*Print[tf];*)
tf=Map[NumberQ,tf];
If[Or@@tf,
pos=i+Flatten[Position[tf,True]];
(*Print[pos];*)
cand=list1[[pos]];
listset=Join[list1[[{i}]],cand];
comfac=PullOutComFac[listset,NumericFactor->True];
listset=comfac MultiplyList[Cancel[listset/comfac ]];
listset2=SelectMaxZeroSet[listset,repl,var];
If[listset2[[1]]==={},found=False;i+=1,
Print[Style["Found  ",Green],Factor[Plus@@(listset2[[1]])]];
found=True;
AppendTo[retlist,Factor[Plus@@(listset2[[1]])]];
exprit=Simplify[exprit-(Plus@@listset2[[1]])]
],
i+=1;
]
]
]
]
*)


(*SelectMaxZeroSet[list0_List,repl_,var_]:=Module[{i=1,found=False,cand,pos,comfac,list},
(*Print[SelectMaxZeroSet];*)
comfac=PullOutComFac[list0,NumericFactor->True];
list=list0/comfac//Cancel;
While[found===False,
If[i===Length[list],(*Print["not found"];*)Return[{{},list}]];
pos=Subsets[Table[i,{i,Length[list]}],{Length[list]+1-i}];
cand=Map[list[[#]]&,pos]/.Simplify[repl/.var->0]/.var->0;
cand=Map[Cancel[#/PullOutComFac[#,NumericFactor->True]]&,cand];
cand=Map[PossibleZeroQ,Simplify[ExpandPower[Plus@@@cand]]];
If[Or@@cand,
found=True;
cand=First[Flatten[Position[cand,True]]];
(*Print[cand];*)
pos=pos[[cand]],
i+=1
]
];
(*Print[pos];*)
(*Print[comfac];*)
SelectMaxZeroSet::message="Error. Original and new ones don't match";
If[Sort[Join[list0[[pos]],Delete[list0,Map[List,pos]]]]=!=list0,Message[SelectMaxZeroSet::message];Abort[]];

Return[{list0[[pos]],Delete[list0,Map[List,pos]]}]
]
*)


(*TakeMinimumComponent[exprorig_,repl_,var_]:=Module[{expr,list,pos,outlist={},i,j,repl0,sum,found=False},
expr=exprorig;
(*Print[expr];*)
i=1;
repl0=Simplify[repl/.var->0];
While[Simplify[expr]=!=0&&i<25,
list=expr//Expand;
If[Head[list]===Plus,list=List@@list,{list}];
(*Print[list,"  list"];*)
list=MultiplyList[list];
If[list==={},Print["TakeMinimumComponent",expr];Abort[]];
pos=Subsets[Table[i,{i,Length[list]}],{i+1}];
found=False;
j=1;
While[found===False,
If[j>Length[pos],i+=1;Break[]];
sum=Simplify[Plus@@(list[[pos[[j]]]]/.repl0)];
If[list==={},Print[2];Print["TakeMinimumComponent",expr];Print[expr];Abort[]];
If[PossibleZeroQ[sum],
found=True;
(*Print[Style["Found",Green]];*)
AppendTo[outlist,list[[pos[[j]]]]];
(*Print["partial zero",list[[pos[[j]]]]];*)
 expr=expr-(Plus@@(list[[pos[[j]]]]));
i=1,
j+=1];
];
];
If[Simplify[(Plus@@Plus@@@outlist)-exprorig]=!=0,Print["TakeMinimumComponent"];Print[outlist];Print[exprorig];Abort[]];
Return[Plus@@@outlist]
]
*)


(*Scaling[expr_,repl_]:=Module[{out},
out=Times@@(Cases[#, (List[v_,pow_]/;!FreeQ[{Sqrt[x1],Sqrt[x2],
             Sqrt[x3],Sqrt[x4],x1,x2,x3,x4,0},v]):>v^pow,-1])& /@
	     (FactorList[Cancel[ExpandPower[#]]]&/@{(expr/.repl)});
Return[First[out]]];*)


(*

ReduceRelations[set0_,repl_,var_]:=Module[{set,new,prim11,length,i,scval},
set=TakeZeroFactor[#,repl,var]&/@set0;
set=set//Flatten;
set=SortOutDuplicates[set,{}];
prim11=Cases[set0,c[0][a__]-c[0][b__]];
For[i=1,i<=Length[prim11],i++,
length=2;
While[length>1,
(*Print[prim11[[i]]];*)
scval=prim11[[i]]//Cases[#,c[0][a__],-1]&;
(*new=Cases[set,fac1_. scval[[1]]^pow1_. -fac2_. scval[[2]]^pow2_.];*)

new={
Cases[set,fac1_. scval[[1]]^pow1_. -fac2_. scval[[2]]^pow2_.],
Cases[set, -fac2_. scval[[2]]^pow2_. +fac1_. scval[[1]]^pow1_.],
Cases[set,fac1_. scval[[1]]^pow1_. -fac2_. scval[[2]]^pow2_.],
Cases[set, -fac2_. scval[[2]]^pow2_. +fac1_. scval[[1]]^pow1_.]};

new=DeleteDuplicates[new];
(*Print[new];*)
new=Cases[set,fac1_. scval[[1]]^pow1_. -fac2_. scval[[2]]^pow2_.| fac2_. scval[[2]]^pow2_. -fac1_. scval[[1]]^pow1_.];

new=Map[#/.{scval[[1]]^pow_.:>scval[[1]]^(pow-1),scval[[2]]^pow_.:>scval[[2]]^(pow-1)}&,new];
new=DeleteCases[new,0];
length=Length[new];
set=DeleteCases[set,fac1_. scval[[1]]^pow1_. -fac2_. scval[[2]]^pow2_.| fac2_. scval[[2]]^pow2_. -fac1_. scval[[1]]^pow1_.];
set=Join[set,new,{prim11[[i]]}];
set=SortOutDuplicates[DeleteDuplicates[set],{}];
];
];
(*Print["Done ",set0];
Print["--> ",set];*)
Return[set]];*)


(*
*)


(*PredictionAndDetection[expr_,repl_,var_]:=Module[{new,pred,sol,FL,lim,pos,tf,vari,assum,cond,sola,detect,detect0},
detect0=FindSetWithFactor[expr,repl,var];
detect=detect0[[2]];
detect=TakeZeroFactor[#,repl,var]&/@(detect)//Flatten;
detect=TakeMinimumComponent[#,repl,var]&/@detect//Flatten;
detect=TakeZeroFactor[#,repl,var]&/@(detect)//Flatten;
detect=SortOutDuplicates[detect,{}];
(*detect0={0,1};*)

If[detect0[[1]]===0,
Print["it's not too bad ", detect0];
vari=DeleteDuplicates[Cases[pred,c[_][__],-1]];
assum=And@@Map[0<#&,vari];
cond=And@@Map[#==0&,detect];
sola=Assuming[assum,Reduce[cond]];
sola=Simplify[sola,Assumptions->assum];
If[!TrueQ[sola/.repl/.var->0//Simplify],Print[1,sola,repl];Abort[]];

If[(Head[sola]===And)&&FreeQ[sola,Or],sola=List@@sola,Print[2,sola,repl];Abort[]];
sola=sola/.Equal[a_,b_]:>a-b;

Return[sola]];
pred=PredictRelations[repl,var,4];
(*Print["pred ",pred];
Print["detect ",detect];*)
pred=Join[pred,detect];
(***********************************)
vari=DeleteDuplicates[Cases[pred,c[_][__],-1]];
assum=And@@Map[0<#&,vari];
cond=And@@Map[#==0&,pred];
sola=Assuming[assum,Reduce[cond]];
sola=Simplify[sola,Assumptions->assum];
If[!TrueQ[sola/.repl/.var->0//Simplify],Print[3,sola,repl];Abort[]];
If[(Head[sola]===And)&&FreeQ[sola,Or],sola=List@@sola,Print[4,sola,repl];Abort[]];
sola=sola/.Equal[a_,b_]:>a-b;
(***********************************)
sol=And@@Map[#==0&,pred]//Solve;
tf=And@@@Map[!NumberQ[#[[2]]]&,sol,{2}];
sol=Delete[sol,Position[tf,False]];
(*Print[sol];*)
If[Length[sol]=!=1,Print["Error 5"];Abort[],
lim=expr/.First[sol]//Simplify
];
(*Print[lim];*)
FL=Map[First,FactorList[lim]];
pos=FL/.Simplify[repl/.var->0]//Simplify;
new=FL[[Flatten[Position[pos,0]]]];
(*Print[new];*)
new=TakeZeroFactor[#,repl,var]&/@Flatten[new];
new=SortOutDuplicates[Flatten[new],{}];
If[VerboseFunctions>=2,Print[Join[sola,Flatten[new]]]];
Return[Join[sola,Flatten[new]]];
]*)


(*Options[BreakDown]={print->False};
BreakDown[expr_,repl_,var_,OptionsPattern[]]:=Module[{listc,res},
listc=expr//Expand;

If[Head[listc]===Plus,
listc=List@@listc;
(*Print[listc];*)
res=FindSet[listc,repl,var,{},{},print->OptionValue[print]];
(*Print[res];*)
res=DeleteDuplicates[res];
,
res={expr}];

(*Print[res];
Abort[];*)
res=Map[TakeZeroFactor[#,repl,var]&,res];
res=Map[Take[SortBy[#,LeafCount],{1}]&,res];

Return[SortOutDuplicates[res,{}]]

];
*)


(*Options[FindSet]={print->False,ReturnList->False};
FindSet[listc_,repl_,var_,new1_,new2_,OptionsPattern[]]:=Module[{new,list0,tf={False},sum,rem,numi=1,zerofactor,leave=False,pred},

If[listc==={},
 If[Simplify[(Plus@@new2)/.Simplify[repl/.var->0]/.var->0]=!=0,Print["Error"];Abort[]];
 If[new2=!={},
 (*Print[new2];*)
 new=PredictionAndDetection[Plus@@new2,repl,var],
 new=new1
  ];
  (*Print[new];*)
 Return[new];
];

list0=listc/.Simplify[repl/.var->0]/.var->0;

While[Count[tf,True]<2&&numi<=Length[list0],
tf=Map[NumberQ[Cancel[list0[[numi]]/#]]&,list0];
numi+=1;
];

(*Print[Count[tf,True]];*)
sum=Plus@@Delete[listc,Position[tf,False]]//Simplify;
rem=Delete[listc,Position[tf,True]];

If[OptionValue[print],Print[Length[new1]+Length[new2],"th iteration. ",Count[tf,True]," terms to merge. ",Length[rem]," terms remaining. "]];

If[Simplify[sum/.Simplify[repl/.var->0]/.var->0]=!=0,
Return[FindSet[rem,repl,var,new1,Append[new2,sum],print->OptionValue[print],ReturnList->OptionValue[ReturnList]]];
leave=True;
];

If[leave,
Return[FindSet[rem,repl,var,new1,Append[new2,sum],print->OptionValue[print],ReturnList->OptionValue[ReturnList]]]
,
sum=TakeZeroFactor[sum,repl,var];
Return[FindSet[rem,repl,var,Append[new1,sum],new2,print->OptionValue[print],ReturnList->OptionValue[ReturnList]]]];
];
*)


(* ::Section:: *)
(*New*)


(*find pairs of expressions that produces numerical factor when one devided the other*)
findpair[list_,repl_,var_]:=Module[{pairlist={},pair,num,frac,pos,repl0,i,fracimp,posimp},
repl0=ExpandPower[Simplify[repl/.var->0]];
For[i=1,i<=Length[list],i++,
frac=Map[Cancel[list[[i]]/#/.repl0]&,Drop[list,i]];
pos=Map[NumberQ,frac];
pos=Position[pos,False];
num=Delete[frac,pos];
pair=MapThread[{list[[i]],#1,#2}&,{Delete[Drop[list,i],pos],num}];
pairlist=Join[pairlist,pair];
];
Return[pairlist]
]


TakeZeroFactor[sum_,repl_,var_]:=Module[{zerofactor,tf,sum2},
(************************************************************************)
(* Simple relations often comes out in the form  0 * "something non-zero"*)
(*                   this is to simply pickup zero factor                *)
(************************************************************************)
zerofactor=Power@@@FactorList[sum];
tf=zerofactor/.Simplify[repl/.var->0]/.var->0;
tf=PossibleZeroQ/@tf;
sum2=Delete[zerofactor,Position[tf,False]];
If[Simplify[(Times@@sum2)/.Simplify[repl/.var->0]/.var->0]=!=0,
Print[sum,repl,var];
TakeZeroFactor::message="Error";
Message[TakeZeroFactor::message];
If[VerboseFunctions>=2,Print[sum2]];
Abort[]];
If[Length[sum2]==2,Print[Style["Length "<>ToString[Length[sum2]],Red]]];
Return[sum2]
]



ForceFindSimpleRelations[expr_,repl_,var_]:=Module[{rel,sol,tf,rel2,lim,add,combl,replred},
(***************************************************************)
(*          This is the main function of this file              *)
(*expr is the large expression that is supposed to be 0 when x\[Rule]0*)
(*         repl is the replacements of c[_][__]                  *)
(*           var = variable that is sent to 0                    *)
(***************************************************************)
(*Print[expr,var];*)
If[Length[DeleteDuplicates[Cases[expr,c[_][__]|cT[_][__],-1]]]<=4,combl=Length[DeleteDuplicates[Cases[expr,c[_][__]|cT[_][__],-1]]],combl=4];

replred=Normal[KeyTake[Association[repl],DeleteDuplicates[Cases[expr,c[_][__],-1]]]];

rel=Timing[PredictRelations[replred,var,combl]];(*Generate simple ralations predictable easily*)

If[VerboseTiming>=2,Print["Generating relations took ", rel[[1]]]];(*generating relations may take a few seconds*)
rel=DeleteDuplicates[rel[[2]]];

rel=ReduceVariables[expr,rel];
(*it solves set of relations such that it reduces variables that do not appear in the problem we are dealing with*)

sol=Map[#==0&,rel];(*set that the relations are all 0. i.e. the variable is sent to 0*)
(*Print[sol];*)
sol=Timing[Solve[sol,DeleteDuplicates[Cases[sol,c[_][__]|cT[_][__]]]]];(*Solve to produce replacement rules that satisfies the relations*)
(*Since generated relations overlaps often it is better to use Solve[] than manually*)
(*i.e. A B - C D =x fac1 && A-C=x fac2 --> B-D=x fac3*) 
(*so A-C & B-D (or other appropriate perm.) overlaps with A B-C D *)
If[VerboseTiming>=2,Print["Solving ", sol[[1]]]];
sol=sol[[2]];

(*Remove solution that includes c[_][__]\[Rule]0 *)
tf=Map[MemberQ[#,_->0]&,sol];
If[VerboseFunctions>=2,Print[sol]];
sol=Delete[sol,Position[tf,True]];
If[Length[sol]>1,Print["more solutions"]];
sol=First[sol];(*there may be more than one ...*)
(***************************************)

rel2=sol//.{(a_->b_/c_):>a c-b}/.{(a_->b_):>a -b};(*So the solution is reversed back to the appropriate relations.*)
(*The resultant relations would be slightly different from the original rel, as Solve has merged or separated overlapping part appropriately*)
lim=Factor[expr/.sol];
If[!PossibleZeroQ[lim],(*if Expression will not go 0 when the soltion is applied*)
add=Times@@Map[First,lim//FactorList];(*make a list of factors without power*)
add=TakeZeroFactor[add,repl,var];(*Find a factor that is remaining finite but vanishes when the variable is sent to zero explicitly*)
If[VerboseFunctions>=2,Print["New ",add]];
rel=Join[rel2,add],
rel=rel2
];

Print[rel];
(*Testing*)
sol=Map[#==0&,rel];
sol=Solve[sol,DeleteDuplicates[Cases[sol,c[_][__]|cT[_][__]]]];
tf=Map[MemberQ[#,_->0]&,sol];
sol=Delete[sol,Position[tf,True]];
sol=First[sol];
If[!PossibleZeroQ[Factor[expr/.sol]],ForceFindSimpleRelations::message="NotFound";Message[ForceFindSimpleRelations::message];Abort[]];


If[VerboseFunctions>=2,Print["Simple  enough?", rel]];
Return[rel]
];


PredictRelations[repl_,var_,limit_:4]:=Module[{set,cs,remove,zeroset,add,int,setvar,zerosetvar,i},
(*predict simple relations that are simple enough to predict*)

cs=repl[[All,1]];
cs=Join[cs,{1(*,2*)(*,1/2*)}];(*cs is a list of c[_][__] and 1, *)
set=Times@@@Subsets[cs,Round[limit/2]]//DeleteDuplicates//Sort; (*take subsets containing less than limit/2 (limit is set to 4. i.e. relation will not involve more than 4 c[_][__]'s) *)
set=findpair[set,repl,var];
(*find pairs of sets of c[_][__] which produces number when one divive another in the limit var\[Rule]0*)
(*i.e. c[0][a__]/(c[0][b__] c[0][c__]) /. x\[Rule]0 \[Equal]2 
   --\[Rule] c[0][a__]-2 c[0][b__] c[0][c__] /. x\[Rule]0 ==0*)
(*this way the simple ralations contain numerical factors*)

set=Map[#[[1]]-#[[3]] #[[2]]&,set];
set=DeleteCases[set,0];
set=TakeZeroFactor[#,repl,var]&/@set;
set=Flatten[set]//DeleteDuplicates;
(*set=ReduceRelations[set,repl,var];*)

(*There may be soome relations that are already 0 before sending var\[Rule]0*)
remove=Map[PossibleZeroQ[Factor[#]]&,set/.repl];
remove=Position[remove,True];
If[VerboseFunctions>=2,Print[set[[Flatten[remove]]],"are already zero"]];
zeroset=set[[Flatten[remove]]];
set=Delete[set,remove];
(********************************************************************)


(*Such relations that are 0 on their own may be unimprotant but they are at least necessary if it includes c[_][__] that appears in the non-trivial relations*)
(*Including "zero relations" may change something but for now they are removed unless necessary*)
For[i=1,i<=Length[zeroset],i++,
setvar=DeleteDuplicates[Cases[set,c[_][__]|cT[_][__],-1]];
zerosetvar=DeleteDuplicates[Cases[zeroset,c[_][__],-1]];
If[!IntersectingQ[setvar,zerosetvar],Break[]];
int=Intersection[setvar,zerosetvar];
int=First[int];
add=Map[FreeQ[#,int]&,zeroset];
add=Position[add,False];

(*Print[zeroset[[Flatten[add]]]];*)

set=Join[set,zeroset[[Flatten[add]]]];
zeroset=Delete[zeroset,add];
];



(*Print[set];*)
If[!And@@Map[PossibleZeroQ,set/.Simplify[repl/.var->0]//Simplify],Print["Error, wrong relations"]];
Return[set]
]


Scaling[expr_List,repl_]:=Module[{out},

out=Timing[Times@@(Cases[#, (List[v_,pow_]/;!FreeQ[{Sqrt[x1],Sqrt[x2],
             Sqrt[x3],Sqrt[x4],x1,x2,x3,x4,0},v]):>v^pow,-1])& /@
	     (FactorList[Cancel[ExpandPower[#]]]&/@(expr/.repl))];
	     If[VerboseFunctions>=2,Print["Time for scaling ",out[[1]]]];
	    out=out[[2]];
Return[First[out]]];


TakeMinimum[expr_,simplist0_,var_]:=Module[{vari={},list,remove,pos,i,found,res,simplist,el,j},
el=Map[DeleteDuplicates[Cases[#,c[_][__],-1]]&,simplist0];
If[Length[simplist0]>Length[Union@@el],Print[Length[simplist0],Length[Union@@el]];Return[simplist0]];

el=Length/@el;
simplist=Transpose[{simplist0,el}];
simplist=SortBy[simplist,Last];
simplist=First/@simplist;


j=1;
While[Length[vari]=!=Length[simplist],
vari={};

If[j==25,Print["suitable variable set not found"];Return[simplist0]];

For[i=1,i<=Length[simplist],i++,

el=DeleteDuplicates[Cases[simplist[[i]],c[_][__]|cT[_][__],-1]];
el=Complement[el,vari];
If[el==={},Break[]];
el=RandomChoice[el];
AppendTo[vari,el]
];
j+=1;
];


list=Map[#==0&,simplist];

list=MapThread[Solve[#1,#2]&,{list,vari}];


remove=Map[PossibleZeroQ[#[[2]]]&,list,{3}];
remove=Map[Or@@@#&,remove];
list=Delete[list,Position[remove,True]];
If[!And@@Map[Length[#]==1&,list],TakeMinimum::message="solution is not unique";Message[TakeMinimum::message];Print[list];];
list=Map[First,list];
pos=Subsets[Table[i,{i,Length[list]}],{1,Length[list]}];
i=0;
found=False;
(*Print[list];*)

While[found=!=True,
If[i==Length[pos],Print[res];Print["not found"];Break[]];
i+=1;
(*Print[list[[pos[[i]]]]];*)
res=Together[expr//.(Join@@list[[pos[[i]]]])/.var->0];
found=PossibleZeroQ[res];
];

Return[simplist[[pos[[i]]]]]
];



(*Adjust the simple relations most suitable for the eqthis*)
(*it solves set of relations such that it reduces variables that do not appear in the problem we are dealing with*)
ReduceVariables[expr_,simple_]:=Module[{necessary,all,sol},
If[Length[simple]<=1,Return[simple]];

necessary=DeleteDuplicates[Cases[expr,c[_][__]|cT[_][__],-1]];
all=DeleteDuplicates[Cases[simple,c[_][__]|cT[_][__],-1]];


sol=Eliminate[Map[#==0&,simple],Complement[all,necessary]];
(*Print["sol1 ",sol];*)
If[!FreeQ[sol,Or],Print[sol];ReduceVariables::message="Insufficient input?";Message[ReduceVariables::message];Return[{simple}]];
If[(Head[LogicalExpand[sol]]=!=And)&&(!FreeQ[sol,Or|And]),Print[sol];Print[simple];Print[expr];ReduceVariables::message="Insufficient input?";Message[ReduceVariables::message];Return[{simple}]];


If[Head[sol]===And,sol=List@@sol,{sol}];

sol=sol/.Rule->Equal;
sol=sol/.{(a_==b_):>(a-b)};

Return[sol]
];






(* ::Section:: *)
(**)


End[]
EndPackage[]
