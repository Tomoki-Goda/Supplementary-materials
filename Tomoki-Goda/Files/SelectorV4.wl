(* ::Package:: *)

(*SetDirectory[NotebookDirectory[]]*)


(*<<Initialize`*)


BeginPackage["SelecorV4`", {"Global`","SecDecV4`"}];
Selector::usage="";
FreeParametrization::usage="";
InputFormat::usage="";
makeselector::usage="";
(*times::usage="";*)
InterpretSelector::usage = ""
ReturnDFunc::usage = "option for Selector[]"
TestSelecor::usage = "option for Selector[]"
SelectorPlus::usage = ""
Selector66::usage = ""
AngleSet::usage = ""
combine::usage = ""
ParallelExpand::usage = ""
GenerateD::usage = ""
setT::usage = ""
setE::usage = ""
DropSubsets::usage = ""
$SelectorTestMode=False;


Begin["`Private`"];


(* ::Section:: *)
(*Miscellaneous*)


(* ::Subsection:: *)
(*Miscellaneous*)


(*li={l1,l2,l3};*)

Print[Style["Loading "<>ToString[numberofparticles]<>" particle selector function.", Green]];
If[VerboseFunctions>=2,Print[Style["System is not well tested for cases where expression can not achieve maximal divergences.", Pink]]];


If[Length[li]=!=numberofparticles,
Print["Change in number of particles"];
Unprotect[li,li0,liT,lim,ni,li0li,liTli,limli];
li=Map[ToExpression["l"<>ToString[#]]&,Table[i,{i,1,numberofparticles}]];
ni={n1,n2};
liT=Map[ToExpression[ToString[#]<>"T"]&,li];
lim=Map[ToExpression[ToString[#]<>"m"]&,li];
li0=Map[ToExpression[ToString[#]<>"0"]&,li];

li0li=Association@@Join[MapThread[#1->#2&,{li0,li}],MapThread[#1->#2&,{li,li0}],Map[#->1&,ni]];
limli=Association@@Join[MapThread[#1->#2&,{lim,li}],MapThread[#1->#2&,{li,lim}]];
liTli=Association@@Join[MapThread[#1->#2&,{liT,li}],MapThread[#1->#2&,{li,liT}]];
Protect[li,li0,liT,lim,ni,li0li,liTli,limli];
]


splistfull=DeleteDuplicates[sp@@@Subsets[Join[li,ni],{2}]]//DeleteCases[#,Alternatives@@(sp@@@Subsets[ni,{2}])]&;
Print[splistfull];



(*SetAttributes[sp,Orderless]
*)


ParallelExpand[in_]:=Module[{list,out,tf,extra},
tf=Map[SameQ[Head[#],sp]&,in];
(*Print[tf];*)
list=Pick[in,tf,True];
extra=Pick[in,tf,False];
list=Subsets[list,{2}];
list=list/.sp->List;
list=Map[{Intersection@@#,Complement[Union@@#,Intersection@@#]}&,list];
list=Map[If[#[[1]]=!={}&&#[[2]]=!={},sp@@(#[[2]])]&,list];
out=DeleteCases[DeleteDuplicates[Join[list,in]],Null];
(*Print[list];*)

If[Length[out]==Length[in],
Return[out],
Return[ParallelExpand[out]]]
];



DropSubsets[in_List]:=Module[{tf,in2},
in2=DeleteDuplicates[Map[Sort,in]];
tf=Table[Or@@Map[SubsetQ[#,in2[[i]]]&,Drop[in2,{i}]],{i,1,Length[in2]}];
tf=Position[tf,True];
Return[Delete[in2,tf]]
]


Options[makeselector]={holdtimes->True,factorout->False};
makeselector[input_,OptionsPattern[]]:=Module[{D2,sel,spinput},
If[OptionValue[factorout]===True,spinput=selectorfactorout[input],spinput=input];
(*input is a list of lists of sp[]'s that go to zero simultaneously*)
D2=Sum[Cancel[1/(Times@@spinput[[numi]])],{numi,1,Length[spinput]}];
sel=Table[times[Cancel[1/D2],Cancel[1/Times@@spinput[[numi]]]],{numi,1,Length[spinput]}]//Simplify ;

If[OptionValue[holdtimes]===False,
sel=sel/.times->Times//Simplify;
If[(Plus@@sel//Together)===1,sel=sel//Expand//Factor,sel=Null],
If[(Plus@@ReplaceAll[sel,times->Times]//Together)===1,sel=sel//Simplify,sel=Null]
];
Return[sel]
]


InputFormat[n2_,n1_,parallel_,order_]:=Module[{n2out=n2,n1out=n1,parallelout=parallel,orderout=order},
parallelout=Complement[parallel,Union[n2,n1]];
If[Sort[Union[n2,n1]]=!=li,
(*If one particle doesnt have to be certain parametrization, use the same one as the ther particles*)
If[Length[n2]===0&&Length[n1]=!=0,n1out=Join[n1,Complement[li,n1]],
If[Length[n1]===0&&Length[n2]=!=0,n2out=Join[n2,Complement[li,n2]],
(*if there is no reason to choose one, use angle and energy*)
n1out=Join[n1,Complement[li,Join[n1,n2]]]
]]];
Return[{n2out,n1out,parallelout,orderout}]
];


(* ::Section:: *)
(*Selector NEW!*)


(* ::Subsection::Closed:: *)
(*Transverse & Energy*)


lTlist=Subsets[liT,{numberofparticles-1}];


EnergySet[blockedlT_]:=Module[{energylist,soft,allowedenergy},
energylist=Map[{#,1/#}&,li0];
energylist=Subsets[energylist,{numberofparticles-1}];
energylist=Join@@Map[Tuples,energylist];
soft=energylist/.lj0:Alternatives@@li0:>li0li[lj0];
soft=DeleteCases[soft,1/_,{2}];
allowedenergy=Map[MemberQ[#,blockedlT]&,soft];
allowedenergy=Pick[energylist,allowedenergy,False];
Return[allowedenergy]
]



(* ::Subsection::Closed:: *)
(*angle*)


AngleSet[splistfull_:splistfull,numberofparticles_:numberofparticles]:=Module[{sub,drop,suball},
sub=Subsets[splistfull,numberofparticles (numberofparticles+1)/(2)];
(*sub=Fold[DeleteCases[#1,a_/;SubsetQ[a,#2]]&,sub,Map[{sp[n1,#],sp[n2,#]}&,li]];
sub=Fold[DeleteCases[#1,a_/;SubsetQ[a,#2]]&,sub,Transpose[Map[{sp[n1,#],sp[n2,#]}&,li]]];*)
sub=Map[ParallelExpand,sub]//DeleteDuplicates;
sub=Fold[DeleteCases[#1,a_/;SubsetQ[a,#2]]&,sub,Map[{sp[n1,#],sp[n2,#]}&,li]];
sub=Fold[DeleteCases[#1,a_/;SubsetQ[a,#2]]&,sub,Transpose[Map[{sp[n1,#],sp[n2,#]}&,li]]];
drop=Join@@Map[{{sp[#[[1]],#[[2]]],sp[n1,#[[1]]],sp[n2,#[[2]]]},{sp[#[[1]],#[[2]]],sp[n2,#[[1]]],sp[n1,#[[2]]]}}&,Subsets[li,{2}]];
suball=Fold[DeleteCases[#1,a_/;SubsetQ[a,#2]]&,sub,drop];
Return[suball]];



(* ::Subsection:: *)
(*selector*)


GenerateD[(*OptionsPattern[]*)]:=Module[{
sum,angset,iT,TransverseD,
EnergyD,iE,(*tallowen,*)remsub,HEn,LEn,angsel,lTselector,allD,subD,blockedlT,tf},
(*******************************************************************)
(*******************************************************************)
sum=0;
allD={};
TransverseD=Subsets[liT,{numberofparticles-1}];(*D for lT*)
angset=AngleSet[]; 
(*******************************************************************************)
For[iT=1,iT<=Length[liT],iT++,(*start of loop in lt selector*)

blockedlT=First[Complement[li,Map[liTli,DeleteDuplicates[Cases[TransverseD[[iT]],Alternatives@@liT,-1]]]]];

EnergyD=EnergySet[blockedlT];(*Allowed energy config. in the given lT*)

If[$SelectorTestMode,Print["Allowed config. ", TransverseD[[iT]],EnergyD]];

subD={};
(*********************************************************************************)
For[iE=1,iE<=Length[EnergyD],iE++,(*start of loop in energy selector*)

HEn=Cases[EnergyD[[iE]],1/a_/;MemberQ[li0,a]:>li0li[a]]; (*High energy*)
LEn=Cases[EnergyD[[iE]],a_/;MemberQ[li0,a]:>li0li[a]];(*Low energy*)

(******************************** Collinear Always makes liT=0********************************)
remsub=DeleteCases[angset,a_/;MemberQ[a,sp[n1,blockedlT]]];
(*Print[DropSubsets[Cases[angset,a_/;MemberQ[a,sp[n1,blockedlT]]]]];*)
(********************************************************************************************)


(***************************Without sp[li, n2], inf energy will make inf lT***********************)
remsub=Pick[remsub,Map[SubsetQ[#,Map[sp[n2,#]&,HEn]]&,remsub],True];
(* Print[DropSubsets[Pick[remsub,Map[SubsetQ[#,Map[sp[n2,#]&,HEn]]&,remsub],False]]];*)
(**************************************************************************************************)
 
 
(************can not have all particles being col. or a.col. unless energy  is infinite**********)
tf=Map[Complement[DeleteDuplicates[Cases[#,sp[n1|n2,lj_]:>lj,{0,Infinity}]],HEn]&,remsub];
tf=Map[numberofparticles===Length[#]&,tf];
(*Print[Pick[remsub,tf,True]];*)
remsub=Pick[remsub,tf,False];
(**********************************************************************************************)


(********************can not have all particles being soft or a.col. unless km=0*******************)
tf=Map[Union[DeleteDuplicates[Cases[#,sp[n2,lj_]:>lj,{0,Infinity}]],LEn]&,remsub];
tf=Map[numberofparticles===Length[#]&,tf];
(*Print[Pick[remsub,tf,True]];*)
remsub=Pick[remsub,tf,False];
(**********************************************************************************************)


(*************reduce subsets************)
remsub=DropSubsets[remsub];
(*Print["Blocked Transverse ",blockedlT," Hard ",HEn, " Soft ",LEn];
Print[remsub];*)
(***************************************)

sum+=Length[remsub];
(******First arg of setE/setT is the d of parent selector*)
subD=Append[subD,setE[EnergyD[[iE]],remsub]];
];(*End of energy loop*)
allD=Append[allD,setT[TransverseD[[iT]],subD]];
];(* end of lT loop*)
Return[allD/.sp[a_,b_]:>sp[a,b]/(li0li[a]li0li[b])/.ljT:Alternatives@@liT:>ljT^2]
]



SetAttributes[combine,{Flat,Listable}]


Options[Selector66]={SeparationLevel->1};
Selector66[OptionsPattern[]]:=Module[{dfunc,selector,Tselector,Aselector,Eselector,AEselector,TEselector},
If[OptionValue[SeparationLevel]>2,Print["option should be 0,1 or 2 "];Abort[]];
If[OptionValue[SeparationLevel]=!=1,Print["Options 0, 2 may not work. 2 definitely doesn't work."]];
dfunc=GenerateD[];
If[OptionValue[SeparationLevel]<=1,
dfunc=dfunc//.setE[a_,b_]:>Map[Join[a,#]&,b];
If[OptionValue[SeparationLevel]<=0,
dfunc=dfunc//.setT[a_,b_]:>Map[Join[a,#]&,b,{2}];
Print["One layer"];
selector=makeselector[Flatten[dfunc,2]];
selector=combine/@selector,
(*****lT selector and AE are factorized******)
Tselector=makeselector[dfunc[[All,1]]];
AEselector=Map[makeselector[Flatten[#[[2]],1]]&,dfunc];
Print["Two layers"];
selector=MapThread[combine,{Tselector,AEselector},1];
],
(*****lT  A and E are all factorized******)
Tselector=makeselector[dfunc[[All,1]]];
Eselector=Map[makeselector[#[[2,All,1]]]&,dfunc];
Aselector=Map[Map[makeselector[#[[2]]]&,#[[2]]]&,dfunc];
TEselector=MapThread[combine,{Tselector,Eselector},1];
Print["Three layers"];
selector=MapThread[combine,{TEselector,Aselector},2];
];
selector=Flatten[selector];
selector=Map[times@@combine@@@Transpose[#//.times|combine->List]&,selector];
Return[selector]
]





InterpretSelector[sel_]:=Module[{in,n2mom,n1mom,parallel,hard,soft,flat,fulllist,outlist},

in=sel[[2]]/.{sp[lj_,lk_]:>(li0li[lk]li0li[lj]) sp[lj,lk]}/.combine[a__]:>Cancel[Times[a]];
If[in===1,
Return[{{},li,{},li}]
];
(*in=in/.combine\[Rule]Times;*)
parallel=DeleteDuplicates[Flatten[Cases[in,sp[lj:Alternatives@@li,lk:Alternatives@@li]:>{lj,lk},-1]]];
n2mom=Cases[in,sp[n2,lj_]:>lj,-1];
n1mom=Cases[in,sp[n1,lj_]:>lj,-1];
hard=DeleteCases[Map[First,FactorList[Numerator[in/.{sp[__]->1,(Alternatives@@liT):>1}]]],a_/;NumberQ[a]];
hard=Map[li0li,hard];
soft=DeleteCases[Map[First,FactorList[Denominator[in/.{sp[__]->1,(Alternatives@@liT):>1}]]],a_/;NumberQ[a]];
(*Print[Denominator[in/.{sp[__]\[Rule]1,(Alternatives@@liT)\[RuleDelayed]1}]];*)
soft=Map[li0li,soft];
flat=DeleteCases[Map[First,FactorList[Denominator[in/.{sp[__]->1,(Alternatives@@li0):>1}]]],a_/;NumberQ[a]];
flat=Map[liTli,flat];

(***Ordering******)
(*lT=0 come first*)
(*n1 comes first*)
(*soft comes first*)
parallel=Function[Join[Intersection[#1,#2,#3,#4],Intersection[#1,#2,Complement[#3,#4]],Intersection[#1,Complement[#2,#3]],Complement[#1,#2]]][parallel,flat,n1mom,soft];


outlist=Function[{#1,#2,#3,Join[#3,Join[Complement[#1,#3],Complement[#2,#3],Complement[li,Join[#1,#2,#3]]]]}][n2mom,n1mom,parallel];

Return[outlist]
]



Options[CombineSelector]={ReturnDFunc->False};


CombineSelector[sel_,OptionsPattern[]]:=Module[{seltype,uniquetypes,newsel,selector,pos,i,sector,dfunc,dfunction,listdfunc={}},
seltype=Map[InterpretSelector,sel];
uniquetypes=DeleteDuplicates[seltype];
(*Print[Length[uniquetypes]," Selectors"];*)
newsel={};

If[OptionValue[ReturnDFunc],dfunc=sel[[All,2]]];



selector=sel;

For[i=1,i<=Length[uniquetypes],i++,

pos=Flatten[Position[seltype,uniquetypes[[i]]]];

(*If[$SelectorTestMode,
Print[i,"  ",pos,"  ",sel[[pos,2]],"  ",uniquetypes[[i]]]
];*)

sector={Cancel[Together[Plus@@(selector[[pos]])]],uniquetypes[[i]]};

If[OptionValue[ReturnDFunc],dfunction=dfunc[[pos]];
AppendTo[listdfunc,dfunction];
];
AppendTo[newsel,sector]
];
Print[Length[newsel]," sectors"];
If[OptionValue[ReturnDFunc],Return[Append[Transpose[newsel],listdfunc]],Return[Transpose[newsel]]];
]




Options[Selector]={ReturnDFunc->False,TestSelector->False,SeparationLevel->1};


Selector[OptionsPattern[]]:=Module[{fullselector},
fullselector=CombineSelector[Selector66[SeparationLevel->OptionValue[SeparationLevel]],ReturnDFunc->OptionValue[ReturnDFunc]];
CombineSelector::warning="Selector does not add up to 1";
If[$SelectorTestMode,If[Factor[Together[Plus@@(fullselector[[1]]//.combine->Times/.times->Times)]]=!=1,Message[Selector66::warning]]];
Return[fullselector/.times[combine[a__],combine[b__]]:>Apply[combine,Thread[times[{a},{b}]]]/.times[a__]:>Cancel[Times[a]]]
]


(* ::Section:: *)
(*Parametrization*)


(*Parametrize[splist_]:=Module[{parallel,ln2,order},
(*splist=in/(Times@@Cases[in,sp[li_,lj_]\[RuleDelayed](li lj/.{lk:Alternatives@@li\[RuleDelayed]ToExpression[ToString[lk]<>"0"], nj:Alternatives@@ni\[RuleDelayed]1}),-1]);*)

parallel=Cases[splist,sp[lj:Alternatives@@li,lk:Alternatives@@li]:>{lj,lk},-1];
ln2=Cases[splist,sp[n2,lk:Alternatives@@li]:>lk,-1];

order=DeleteDuplicates[Flatten[parallel]];
order=Join[order,Complement[li,order]];

parallel=DeleteCases[parallel,Alternatives@@Cases[splist,sp[n2|n1,lk:Alternatives@@li]:>lk,-1],2];
parallel=Cases[parallel,{a_,b_},{1}];

If[Length[ln2]==2,ln2=Join[ln2,Complement[li,ln2,parallel,Cases[splist,sp[n1,lk:Alternatives@@li]:>lk,-1]]]];
FreeParametrization[ln2,Complement[li,ln2],parallel,order]
]*)


R[pos1_,pos2_,ang_,size_]:=Module[{mat},
mat=Table[Table[If[i==j,1,0],{i,size}],{j,size}];
If[!SubsetQ[Table[i,{i,size}],{pos1,pos2}]||pos1==pos2,Return[mat]];
mat=Fold[ReplacePart[#1,Sequence@@#2]&,mat,Transpose[{{Cos[ang],-Sin[ang],Sin[ang],Cos[ang]},{{pos1,pos1},{pos1,pos2},{pos2,pos1},{pos2,pos2}}}]]
]


Options[FreeParametrization]={\[Rho]->False,PrintMatrix->False,ReturnMatrix->False,(*PrioritizeParallel->False,*)l0lT->False,solvedelta->True};


FreeParametrization[LC_List,AE_List,paral_List,order_List,OptionsPattern[]]:=Module[
{tf,f,pa,lc,ae,n=Length[order]+1,\[Theta],mat,matAE,matLC,vectmat,pos,frame,fr,rot,i,repllT,out,parallel,repll0},
\[Theta][num_,type_]:=(*(-1)^(type+1)*)-1* ToExpression[{"th","ch","ph"}[[type]]<>ToString[n-num]];
(*parallel can be given in list of combinations*)
(*If[OptionValue[PrioritizeParallel],parallel=paral,parallel={}];*)
(*parallel=Complement[paral,Union[LC,AE]];*)
parallel=paral;
If[ArrayDepth[parallel]===2,
f=False;
pa=parallel;
While[f==False,
tf=Map[Apply[IntersectingQ,#]&,Subsets[pa,{2}]];
tf=Delete[Subsets[pa,{2,Length[pa]}],Position[tf,False]];
tf=DeleteDuplicates[Union@@@tf];
f=(tf=={});
pa=DeleteCases[Sort/@pa,Alternatives@@Join@@Map[Subsets,tf]];
pa=Join[tf,pa]],
(*Or just a list of all parallel momenta, but in this case only one parallel*)
pa={parallel},Print["something is wrong"]];
ae=AE;
lc=LC;
(*momenta which are parallel to each other should have same type of parametrization*)
For[i=1,i<=Length[pa],i++,
If[IntersectingQ[pa[[i]],ae],ae=Union[pa[[i]],ae]];
If[IntersectingQ[pa[[i]],lc],lc=Union[pa[[i]],lc]]
];
ae=SortBy[ae,Position[order,#]&];
lc=SortBy[lc,Position[order,#]&];
If[IntersectingQ[ae,lc],Print[AE,ae,LC,lc];Abort[]];

If[pa=!={{}},
pa=SortBy[#,Position[order,#]&]&/@pa;
pa=SortBy[pa,Position[order,First[#]]&]];

(*Print["AE ",ae," LC ",lc, " Pa ", pa];*)
(*parallel should come first in order inorder to simplify?*)
If[Join@@pa=!=Take[order,Length[Join@@pa]],Print[Join@@pa,order];Print["Choose Appropriate ordering"];Abort[]];


(*Produce frames*)
(*Print[ae,lc,order];*)
frame={};
For[i=1,i<=Length[order],i++,
If[MemberQ[ae,order[[i]]],

(*fr=order[[i]]->join@@(ToExpression[ToString[order[[i]]]<>"0"]{{1},rot[i]})];*)
fr=order[[i]]->join@@({{ToExpression[ToString[order[[i]]]<>"0"]},ToExpression[ToString[order[[i]]]<>"d"]rot[i]})];

If[MemberQ[lc,order[[i]]],

fr=order[[i]]->join@@{
{(ToExpression[ToString[order[[i]]]<>"m"]+ToExpression[ToString[order[[i]]]<>"p"])/2},
ToExpression[ToString[order[[i]]]<>"T"]rot[i],
{(ToExpression[ToString[order[[i]]]<>"m"]-ToExpression[ToString[order[[i]]]<>"p"])/2}}];

AppendTo[frame,fr]
];



(*Produce Rotation matrices in AE and LC*)
If[ae=!={},
matAE=Table[
Table[R[i-1,i,\[Theta][i-1,j],n],{i,n-j+1,n,1}]
,{j,1,n-1}];
(*Print[TableForm[Table[MatrixForm/@matAE[[i]],{i,1,Length[matAE]}]]]*)
];

If[lc=!={},
matLC=Table[
Table[R[i,i+1,\[Theta][i+1,j],n-1],{i,n-j,n-1}]
,{j,1,n-1}];
(*Print[TableForm[Table[MatrixForm/@matLC[[i]],{i,1,Length[matLC]}]]]*)
];


(*Produce Rotation matrices for parallels in AE or LC*)
(*This part may be a bit problematic!!*)
vectmat={};

If[pa=!={{}},(*Print[Length[pa]," Set/Sets of parallel momenta"];*)
For[i=1,i<=Length[pa],i++,

If[SubsetQ[ae,pa[[i]]],
pos=First[Flatten[Position[order,First[pa[[i]]]]]];
(*mat=Prepend[Table[Join[matAE[[pos]],matAE[[pos+k]]],{k,1,Length[pa[[i]]]-1}],matAE[[pos]]];*)
mat=Table[Join@@Table[
matAE[[i+j-1]]
,{j,1,k}],{k,1,Length[pa[[i]]]}];(*{k,1,Length[Join@@pa[[i]]]}];*)
vectmat=Join[vectmat,mat];
];

If[SubsetQ[lc,pa[[i]]],
pos=First[Flatten[Position[order,First[pa[[i]]]]]];
(*mat=Prepend[Table[Join[matLC[[pos]],matLC[[pos+k]]],{k,1,Length[pa[[i]]]-1}],matLC[[pos]]];*)
mat=Table[Join@@Table[
matLC[[i+j-1]]
,{j,1,k}],{k,1,Length[pa[[i]]]}];(*,{k,1,Length[Join@@pa[[i]]]}];*)
vectmat=Join[vectmat,mat];
];
]

];


(*non parallel*)
For[i=Length[Join@@pa]+1,i<=Length[order],i++,
If[MemberQ[lc,order[[i]]],
vectmat=Join[vectmat,{matLC[[i]]}]];
If[MemberQ[ae,order[[i]]],
vectmat=Join[vectmat,{matAE[[i]]}]];
];
If[OptionValue[PrintMatrix],Print["  "];Print[TableForm[Map[Map[MatrixForm,#]&,vectmat]]]];
(*If[OptionValue[ReturnMatrix],Return[{frame,Table[{rot[ii],vectmat[[ii]]},{ii,numberofparticles}]}]];*)
vectmat=Dot@@#&/@vectmat;

(*rot[i_,n_]:=vectmat[[i]].Join[Table[0,i-1],{1}];*)
If[OptionValue[ReturnMatrix],Return[{frame,Table[{rot[ii],vectmat[[ii]]},{ii,numberofparticles}]}]];
(*Print[frame];*)
vectmat=#[[All,Length[#]]]&/@vectmat;


frame=frame/.rot[ii_]:>vectmat[[ii]];
If[OptionValue[\[Rho]]===True,frame=frame/.{l1m->\[Rho]1/(1-\[Rho]1) l1T,l2m->\[Rho]2/(1-\[Rho]2) l2T,l3m->\[Rho]3/(1-\[Rho]3) l3T,l1p->(1-\[Rho]1)/\[Rho]1 l1T,l2p->(1-\[Rho]2)/\[Rho]2 l2T,l3p->(1-\[Rho]3)/\[Rho]3 l3T}];
If[OptionValue[solvedelta]===True,
frame=frame/.{l1p->l1T^2/l1m,l2p->l2T^2/l2m,l3p->l3T^2/l3m}/.{l1d->l10,l2d->l20,l3d->l30}];

out=Join[frame/.join->Join,{n1->Join[{1},Table[0,n-1],{1}],n2->Join[{1},Table[0,n-1],{-1}]}];
out=Append[out, p->pm/2 n1/.out];

If[OptionValue[l0lT],
repllT=Map[(#/.MapThread[Rule,{li,liT}])->Sqrt[sp[n1,#]sp[n2,#]]&,ae];
repll0=Map[(#/.MapThread[Rule,{li,li0}])->(limli[#]^2+liTli[#]^2)/(2 limli[#]) &,lc];
out=Join[out,repllT/.out/.Replsp,repll0]];
Return[out//Simplify]
(*Return[vectmat];*)
]



End[]
EndPackage[]
