(* ::Package:: *)

VerboseFunctions = 0;
VerboseTiming=0;
(*$Assumptions=Simplify[And@@Map[0<#<1&,{x1,x2,x3,x4,x5,x6,x7,x8,x9(*,y1,y2,y3,y4,y5,y6,y7,y8,y9*)}]&&$Assumptions,Assumptions->True];*)



$Assumptions=Simplify[And@@Map[0<#<1&,Table[ToExpression["x"<>ToString[i]],{i,1, numberofparticles (3+numberofparticles)/2}]]&&$Assumptions,Assumptions->True];






(*SetAttributes[dummy,Protected]*)


BeginPackage["FunctionsV4`",{"Global`","ExpandPowerV2`","SimpleRelations`","SecDecV4`","SymbolicC`"}]
CInt::usage="";
FactorOutImplicit::usage="";
(*ReplEn2Sp::usage="";*)
ExtractRegulators::usage="";
RemoveDuplicates::usage="";
SortJacobian::usage="";
dummy::usage="";
SimplifyJacobian::usage="";
Y9::usage = ""
mY9::usage = ""
X9::usage = ""
par::usage = ""
Breakc::usage = ""
W1 ::usage = ""
W2::usage = ""
c1::usage = ""
c2::usage = ""
cT1::usage = ""
cT2::usage = ""
ExpandList::usage = ""
SplitScaling::usage = ""
AbstractFactors::usage = ""
FindSolutions::usage = ""
deltaScale::usage = ""


Begin["`Private`"];


SetAttributes[dummy,Protected]


apmin=-1;
epmin=-3;


weighthead={W,W1,W2}


cfunchead={c,c1,c2,cT,cT1,cT2,n,d,n1,d1}


mathdir=NotebookDirectory[];


(* ::Section::Closed:: *)
(*C++*)


(* ::Subsection::Closed:: *)
(*C*)


(****************************************************************************** 
 *
 * Generate body of C++ functions. A simple function writing content of funcdata
 * in the C++ format.
 *
 ******************************************************************************)
GenFuncBody[funcdata_] := Module[{seclist={}, FF},

  seclist = Thread[FF[funcdata[[1,All]], funcdata[[2,All]]]];

  Return[seclist/.FF->CFuncString];
]



(****************************************************************************** 
 *
 * Functions for writing integrands into C++ files
 *
 ******************************************************************************)



ExtractHeavisideTheta[expr_]:=Module[{list},
list=FactorList[expr];
list=Cases[list,HeavisideTheta[a_]:>a,{2}];
list=And@@Map[#>0&,list];
Return[list]
];


StringArgument[func_]:=Module[{var},
var=Sort[DeleteDuplicates[Cases[func,x1|x2|x3|x4|x6|x7|x8|x9,{0,Infinity}]]];
var=Join[var,Table[ToExpression["dummy"<>ToString[i]],{i,8}]]//Take[#,8]&;
(*Print[var];*)
var="("<>StringJoin[Map["VARPREC "<>ToString[#]<>", "&,var]]<>"VARPREC* par )";

(*Print[var,"  " ,func];*)
Return[var]
];


CFuncString[name_, func_] := Module[{code = "",cond},

   cond=Simplify[ExtractHeavisideTheta[func]];
   If[cond=!=True,If[VerboseFunctions>=2,Print[cond]];
     code = "VARPREC " <> code  <> ToString[name] <>
    StringArgument[func]
    <>"{\ \n"<>
    "if("<> ToString[CForm[cond]] <>"){\n "<>
    "  return " <> ToString[CForm[func]] <> ";}\n" <>
    "else {return 0;}"<>
    "}\n\n", 
   
  code = "VARPREC " <> code  <> ToString[name] <>
    StringArgument[func]
    <>"{\ \n"<>
    "  return " <> ToString[CForm[func]] <> ";\n" <>
    "}\n\n";
    ];
 (* ];*)

  Return[ToCCodeString[code]];
];


CMapString[namelist_] := Module[{code},
  code = "inline void fill_map() {\n  " <> StringRiffle[
     Table["functions[\"" <> namelist[[i]] <> "\"] = " <> 
       namelist[[i]] <> ";\n", {i, 1, Length[namelist]}], "  "] <> "}\n\n";
  Return[code];
];


CFileString[fname_, body_, funcnames_] := Module[{f1, preprocname},
  f1 = OpenWrite[fname];
  preprocname = 
   ToUpperCase[
    StringReplace[StringSplit[fname, "/"][[-1]], ".hh" -> "_HH__"]];
  WriteString[f1, "#ifndef __" <> preprocname <> "\n"]; 
  WriteString[f1, "#define __" <> preprocname <> "\n\n"];
  WriteString[f1, "#include \""<>mathdir<>"beamcalc/defs.hh\"\n\n"];
  WriteString[f1, #] & /@ body;
  WriteString[f1, "\n\n"];
  (*
  WriteString[f1, CMapString[funcnames]];
  *)
  WriteString[f1, "#endif/*__" <> preprocname <> " */\n"];
  Close[f1];
]



MathematicaForm[weight_,sector_] := Module[{},

 Return[weight/. sector[[2]] /. Replsp /. ReplSin /.  sectors[[4]] // Simplify];
]


(*******************************************)
(*change names of functions suitable for C++*)
(*******************************************)

ModifyFuncName[expr_]:=Module[{wname,wfunc,cfunc,replcfunc,worig,head},
(*change name of W suitable for C++*)
If[!MatchQ[expr,Rule[(Alternatives@@weighthead)[a__],b_]],Abort[]];
worig=First[Cases[expr,(Rule[(w:(Alternatives@@weighthead))[a__],b_]):>w[a],{0}]];

wname=First[Cases[expr,(Rule[(w:(Alternatives@@weighthead))[a__],b_]):>{w,a},{0}]];
wname=Replace[wname,b_/;b<0:>ToExpression["m"<>ToString[Abs[b]]],1];
wname=Replace[wname,{1-x9->Y9,-1+x9->mY9,1+x9->X9},1];
head=wname[[1]];
wname=Drop[wname,1];
wname=StringDelete[StringJoin[Map[ToString,wname]],"x"];

wfunc=ToString[head]<>wname;


(*change name of c functions suitable for C++*)
(*cfunc=DeleteDuplicates[Cases[expr,c[_][__]|n[_][__]|d[_][__]|cT[_][__],-1]];*)
cfunc=DeleteDuplicates[Cases[expr,((Alternatives@@cfunchead)[a_][b__]),{0,Infinity}]];
replcfunc=cfunc/.(cf:Alternatives@@cfunchead)[a_][b__]:>StringReplace[ToString[cf]<>ToString[a]<>StringDrop[wname,-2]<>StringJoin[Map[ToString,{b}]],{"Kernel"->"k","$"->""," "->""}];
(*replcfunc=cfunc/.c[a_][b__]:>StringReplace["c"<>ToString[a]<>StringDrop[wname,-2]<>StringJoin[Map[ToString,{b}]],{"Kernel"->"k","$"->""," "->""}];
replcfunc=replcfunc/.cT[a_][b__]:>StringReplace["cT"<>ToString[a]<>StringDrop[wname,-2]<>StringJoin[Map[ToString,{b}]],{"Kernel"->"k","$"->""," "->""}];
replcfunc=replcfunc/.d[a_][b__]:>StringReplace["d"<>ToString[a]<>StringDrop[wname,-2]<>StringJoin[Map[ToString,{b}]],{"Kernel"->"k","$"->""," "->""}];
replcfunc=replcfunc/.n[a_][b__]:>StringReplace["n"<>ToString[a]<>StringDrop[wname,-2]<>StringJoin[Map[ToString,{b}]],{"Kernel"->"k","$"->""," "->""}];*)

replcfunc=MapThread[#1->#2&,{cfunc,replcfunc}];

(*(*Return in the form {repl for W ,{repl for c}}*)
Return[{{wfunc->expr[[2,1]]/.replcfunc},expr[[2,2]]/.replcfunc}];*)
replcfunc=Append[replcfunc,(worig->wfunc)];
Return[replcfunc]
]


(* ::Subsection::Closed:: *)
(*PrepareFuncData*)


(*********************************************************************************)
(*From the forms {W[__]\[Rule]{function of c ,{replancements for c}}, W[__]\[Rule]{ ,{}},...}, *)
(* Produce the {replacements of c functions, relacements of W, integrands in W}   *) 
(*********************************************************************************)





Options[PrepareFuncData]={DoSeparateFraction->False};
PrepareFuncData[intlist0_List,repl0_List,apmax_,epmax_,OptionsPattern[]]:=Module[{time,intlist,repl,wfunc,newwfunc,intnew,replnew,out},

intlist=Map[Plus@@Flatten[Table[ap^iap ep^iep Coefficient[Coefficient[#,ap,iap],ep,iep],{iap,-1,apmax},{iep,-3,epmax}]]&,intlist0];
intlist=DeleteCases[intlist,0];
Print[intlist];
repl=Normal[KeyTake[Association[repl0],DeleteDuplicates[Cases[intlist,(Alternatives@@weighthead)[__],-1]]]];
repl=repl/.sel[a_]:>a;
If[OptionValue[DoSeparateFraction]===True,
{time,repl}=AbsoluteTiming[SeparateFraction[repl]]
(*Print["SeparateFraction ",time];*)
];




(*Print[repl];*)

wfunc=DeleteDuplicates[Cases[intlist,(Alternatives@@weighthead)[a__],-1]];
newwfunc=wfunc/.(w:Alternatives@@weighthead)[a__]:>Replace[{w,a},b_/;b<0:>ToExpression["m"<>ToString[Abs[b]]],1];
(*Print[newwfunc];*)
newwfunc=ReplaceAll[newwfunc,{1-x9->Y9,-1+x9->mY9,1+x9->X9}];
newwfunc=Map[(*"W"<>*)StringJoin[Map[ToString,#]]&,newwfunc];
newwfunc=Map[StringDelete[#,"x"]&,newwfunc];

intnew=intlist/.MapThread[#1->#2&,{wfunc,newwfunc}];

(*If[intnew=!={},intnew={Plus@@intnew}];*)(*merge integrands*)


(*The core of this function*)
replnew=Map[ModifyFuncName,repl];
(*Print[{repl[[1,1]]\[Rule]repl[[1,2,1]],repl[[1,2,2]]}];*)
replnew=MapThread[{{#1[[1]]->#1[[2,1]]},#1[[2,2]]}/.#2&,{repl,replnew}];
(*Print[replnew];*)
replnew=If[replnew=!={},Union@@@Transpose[replnew],{{},{}}];

out={replnew[[2]],replnew[[1]],intnew};
Return[out]
];






(* ::Subsection::Closed:: *)
(*GenFuncData*)


(******************************************************************************************)
(*This function produces a list of {names of functions, expression( content ) of functions},*)
(*         i.e.            output[[1,i]]:=output[[2,i]]                                     *)
(******************************************************************************************)


Options[GenFuncData]={CutOff->10^-7};


GenFuncData[name_, sector_,apmax_,epmax_,OptionsPattern[]] := Module[{namelist={}, datalist={},
                                                 tmp1list = {}, tmp2list = {}, 
						 finalname= {}, finaldata= {},
						 Repl1,coeff, rename ,pair,coeffi,
						 varlist,
						 Replcfunc,
						 ReplWfunc,
						 term,
						 tmp3list,activevars
						},

  (* Each sector is a list   {replacement of c func,
                            replancements of weights
                            List of integrand} 
  *)

  (* // Iterate over sectors // *)
  Do[
  If[sector[[isec,3]]=!={}&&VerboseFunctions>2,Print[isec,",   ",sector[[isec,3]]]];
(* namelist =Cases[sector[[isec,1]],Rule[a_,b_]\[RuleDelayed]StringJoin[a,name,"subsec",ToString[isec]]];*)

rename =Cases[sector[[isec,1]],Rule[a_,b_]:>Rule[a,StringDelete[StringJoin[a,name,"s",ToString[isec]],"x"]]];
(*Print[rename];*)
namelist =Cases[sector[[isec,1]],Rule[a_,b_]:>a]/.rename;
datalist =Cases[sector[[isec,1]],Rule[a_,b_]:>b];
varlist=Take[Join[Sort[DeleteDuplicates[Cases[#,x1|x2|x3|x4|x6|x7|x8|x9,{0,Infinity}]]],Table[1,{i,8}]],8]&/@datalist;(* list of variables c functions are dependent of*)

Replcfunc = MapThread[(#1-> ToExpression[#1][Sequence@@#2,par])&,{namelist,varlist}];

(*****************************************************************************************************)
 (* Weights W in explicit form *)
rename =Join[rename,
Cases[sector[[isec,2]],Rule[a_,b_]:>Rule[a,StringDelete[StringJoin[a,name,"s",ToString[isec]],"x"]]]
];

tmp1list=Cases[sector[[isec,2]],Rule[a_,b_]:>a]/.rename;
tmp2list=Cases[sector[[isec,2]],Rule[a_,b_]:>b]/.rename/.Replcfunc;
tmp3list=Take[Join[Sort[DeleteDuplicates[Cases[#,x1|x2|x3|x4|x6|x7|x8|x9,{0,Infinity}]]],Table[1,{i,8}]],8]&/@tmp2list;(* list of variables W functions are dependent of*)
ReplWfunc = MapThread[(# 1-> ToExpression[#1][Sequence@@#2,par])&,{tmp1list,tmp3list}];
(*Print["ReplWfunc ",ReplWfunc];*)
tmp2list=tmp2list/.ReplWfunc;

namelist =Join[namelist,tmp1list];(* add weight names *)
datalist =Join[datalist,tmp2list];(* add explicit form of weights *)
(*Print["datalist   ",datalist];*)

(*Repl1 = (# -> ToExpression[#][x1,x2,x3,x4,x6,x7,x8,x9,par])&/@namelist;*)
(*Print[Repl1];*)

        (* Series coefficients with sybolic weights *)
coeff={};
For[term=1,term<=Length[sector[[isec,3]]],term++,

coeffi=Normal[Series[sector[[isec,3,term]],{ap,0,apmax}]];
coeffi=Normal[Series[coeffi,{ep,0,epmax}]];

coeffi=Table[Coefficient[Coefficient[coeffi,ap,ipow],ep,jpow],
		{ipow,apmin,apmax},{jpow,epmin,epmax}];
coeffi=Map[CoefficientList[#,Lmu]&,coeffi,{2}];

coeffi=coeffi/.rename/.ReplWfunc;
(*Print["coeffi",coeffi];*)
activevars=Map[Times@@DeleteDuplicates[Cases[#,activevariables,{0,Infinity}]]&,coeffi,{2}];
(*Print[activevars];*)


coeffi=Table[Table[
	pair[name<>"s"<>ToString[isec]<>"t"<>ToString[term]<>
	"l"<>
	ToString[LmuPow-1]<>
	If[ipow<0,"m","p"]<>
	ToString[Abs[ipow]]<>
	If[jpow<0,"m","p"]<>
	ToString[Abs[jpow]],
	HeavisideTheta[(activevars[[ipow+(1-apmin),jpow+(1-epmin)]])-(OptionValue[CutOff])]*(*Cut off*)
	coeffi[[ipow+(1-apmin),jpow+(1-epmin),LmuPow]]],
	{LmuPow,1,Length[coeffi[[ipow+(1-apmin),jpow+(1-epmin)]]]}],
		{ipow,apmin,apmax},{jpow,epmin,epmax}];
(*Print["coeffi ",coeffi];*)		
		
coeffi=Flatten[coeffi];

AppendTo[coeff,coeffi]
];

(*If[coeff=!={},Print[isec,"coeff",coeff]];*)


coeff=Flatten[coeff]/.pair->List;
coeff=DeleteCases[coeff,{_,0}];

(*Print["coefficient ",coeff];*)
If[coeff==={},coeff={{},{}},
coeff=Transpose[coeff]
];

namelist = Join[namelist,coeff[[1]]];
datalist = Join[datalist,coeff[[2]]];
(*Print[datalist];*)

datalist = datalist/.rename(*//.Repl1*);

(*Print[datalist];*)

finalname = Join[finalname, namelist];
finaldata = Join[finaldata, datalist];

    ,{isec, 1, Length[sector]}
  ];


  Return[{finalname, finaldata}];
]


(* ::Subsection::Closed:: *)
(*CIntNames*)


(***********************************************************************************)
(*generate a file for beamcalc                                                      *)
(*with this no need to change the bit starting "inline void fill_map()...." ,        *)
(*but the file created here must be included and                                    *)
(*fill_map_"name"() has to be added somewhere. where "name" is the name of integral.*)
(***********************************************************************************)


CIntNames[file_,res_,name_]:=Module[{fi},
fi=OpenWrite[file];
(*fi=OpenWrite["~/beam/n3lo/beamcalc/"<>name<>"names.hh"];*)
WriteString[fi,"inline void fill_map_"<>name<>"() {\n"];
Do[WriteString[fi,"functions[\""<>ToString[i]<>"\"]="<>ToString[i]<>";\n"],{i,res}];
WriteString[fi,"};\n"];
(*WriteString[fi,"fill_map_"<>name<>"();"];*)
Close[fi];
]


(* ::Subsection::Closed:: *)
(*Master function CInt*)


CInt[integi_,associ_,name_,apmax_,epmax_]:=Module[{ vars, funcdata , funcbody ,res ,namelong =
				 StringReplace[name,"int"-> "integral"],
reszero,ndim},

funcdata =(*Parallelize[*)Table[PrepareFuncData[integi[[i]],associ[[i]],apmax,epmax],{i,Length[integi]}](*]*);

(*Return[funcdata];*)

 Print["GenFuncData"];

funcdata =GenFuncData[name, funcdata ,apmax,epmax];

 Print["GenFuncBody"];

 funcbody = GenFuncBody[funcdata];
 

 res =Flatten[Table[Table[Table[name <> "s" <> ToString[isec] <> "t"<>ToString[term]<>
			 "l"<>ToString[LmuPow]<>
            If[j < 0, "m", "p"] <> ToString[Abs[j]]<> 
            If[i < 0, "m", "p"] <> ToString[Abs[i]] Power[ep,i]Power[ap,j]Power[Lmu,LmuPow],
            {LmuPow,0,Exponent[integi[[isec,term]],Lmu]}], 
	    {i, epmin, epmax},{j,apmin, apmax},{term,1,Length[integi[[isec]]]}],{isec,1,Length[integi]}]];
	    

reszero =res/.{ep->1,ap->1,Lmu->1}; 


reszero =Complement[reszero,funcdata[[1]]];

reszero=Map[#->0&,reszero];

res=DeleteCases[res/.reszero,0];



  If[VerboseFunctions>=1,Print[res/.{ep->1,ap->1}]];
 vars=Flatten[Pick[funcdata[[2]], funcdata[[1]],#]]&/@ReplaceAll[res,{ep->1,ap->1,Lmu->1}];
 If[VerboseFunctions>=2,Print["implicit form of int in terms of W, vars ",vars]];
 
 CFileString[intdir<>"/"<>namelong<>".hh", funcbody, funcdata[[1]]];
 
 CIntNames[intdir<>"/"<>namelong<>"names.hh",res/.{ep->1,ap->1,Lmu->1},namelong];
 
 
 ndim=DeleteDuplicates[Cases[#,x1|x2|x3|x4|x6|x7|x8|x9,{0,Infinity}]]&/@vars;
 ndim=Length/@ndim;

 (*Export[dir<>"beamcalc/integrals/"<>namelong<>".dat", Thread[{res/.{ep->1,ap->1}, 8}]];*)
 Export[intdir<>"/"<>namelong<>".dat", Transpose[{res/.{ep->1,ap->1,Lmu->1}, ndim}]];
 Return[Plus@@res];
]


(* ::Section:: *)
(*FOI*)


(* ::Subsection:: *)
(*FOI Miscellaneous*)


combinelog={a_. Log[a1_]+b_. Log[b1_]:>Log[a1^a b1^b]/;And[IntegerQ[a],IntegerQ[b]]}


RelevantFactor[expr_,var_,repl_,repllim_]:=Module[{faclist,factors,tf,relevant,irrelevant},
(*Print["RelevantFactor ",$KernelID];*)
faclist=expr//FactorList;
factors=faclist//Map[First,#]&;
factors=factors/.var->0;
factors=Map[Quiet[Simplify[#,TimeConstraint->1]]&,factors]//ExpandPower;
factors=factors/.repllim//Quiet[Simplify[ExpandPower[#],TimeConstraint->2]]&;
tf=Map[PossibleZeroQ,factors//ExpandPower];
(***************************************************)
relevant=Pick[faclist,tf,True];
irrelevant=Times@@Power@@@Pick[faclist,tf,False];
(******************************************************)

(*Print["RelevantFactor End",$KernelID];*)
Return[{relevant,irrelevant}];
];


ConstantTerm[expr_,var_]:=Module[{const,rest},
(*This function splits a function f(x) to {f(0), f(x)-f(0)}*)
const=expr//.var->0;
rest=Collect[expr-const,var];
Return[{const,rest}]
];


ScaleOut[relevant_,irrelevant_,var_,repl_,repllim_]:=Module[{
relpow,relbase,coefffactorlist,const,tf,position,scaleterms, order,
simplelist,scalings,csol, cnewsol,newscaleterms,replacements,
newcoefffactorlist,newrelbase,newrelevant},

(*Print["ScaleOut begin ",$KernelID];*)
(*****************************************************)
(*       relevant part has scalings in var,           *)
(*Express relevant factor as a list of Subscript[C, ijk ]such that *)
(*            relevant = \!\(
\*SubscriptBox[\(\[Product]\), \(i\)]\(\((
\*SubscriptBox[\(\[Sum]\), \(j\)]
\*SuperscriptBox[\(x\), \(j\)]\((
\*SubscriptBox[\(\[Product]\), \(k\)]\ 
\*SubscriptBox[\(C\), \(ijk\)])\))\)\(.\)\)\)          *)
(*  and replace one of Subscript[C, i0k ]with new black box(es)     *)
(*****************************************************)
(*decompose in power and base*)
relpow=Map[Last,relevant];
relbase=Map[First,relevant];

(*split base in constant term and the rest*)
coefffactorlist=Map[FactorList,Map[ConstantTerm[#,var]&,relbase],{2}];

const=Map[First,coefffactorlist];

(*find which terms become 0*)
tf=Map[And[(#[[1]]=!=0),Quiet[Simplify[ReplaceAll[#[[1]],repllim]/.var->0,TimeConstraint->3]]===0]&,const,{2}];
position=Position[tf,True];

(*list of terms that vanish*)
scaleterms=Map[First,Part[const,Sequence@@#]&/@position];
order=OrderingBy[scaleterms,ByteCount];


scaleterms=Pick[scaleterms,order,1]//First;
position=Pick[position,order,1]//First;


simplelist=ForceFindSimpleRelations[scaleterms, repl, var];

scalings=Map[First,Map[ExtractMonomial,simplelist//.repl]];
If[VerboseFunctions>=1,Print[$KernelID," scalings: " ,scalings," for ",simplelist ]];


{csol, cnewsol} = FindSolutions[scaleterms, simplelist, scalings, var];

(*replacing only the relevant factor of constant coefficient of relevant factor of the expression*) 
scaleterms=Part[const,Sequence@@position];
newscaleterms=scaleterms//.csol//Factor;
If[VerboseFunctions>2,Print[$KernelID,"  ",scaleterms,"  ",newscaleterms]];
If[VerboseFunctions>2,Print[$KernelID,"  ",scaleterms,"  ",newscaleterms]];

replacements=Rule[Insert[position,1,2],newscaleterms];
newcoefffactorlist=ReplacePart[coefffactorlist,replacements];

(*newrelbase=Plus@@Table[(Times@@Power@@@(#[[i]]))*var^(i-1),{i,1,Length[#]}]&/@newcoefffactorlist;*)
newrelbase=Plus@@Table[(Times@@Power@@@(#[[i]])),{i,1,Length[#]}]&/@newcoefffactorlist;

newrelevant=Transpose[{newrelbase,relpow}];
(*Print["ScaleOut begin ",$KernelID];*)
Return[{newrelevant,irrelevant, cnewsol,csol}];
]


FOIMain[expr_,repl_,repllim_,var_]:=Module[{variables,newvar,newname,newrepl,relevant,irrelevant,cnewsol,time,csol},
variables=Variables[expr];
newvar=Complement[variables,Map[First,repl],activevariables];

If[newvar=!={},
Print["SpecialFunctions ",newvar];
newname=Table[c["k"<>ToString[$KernelID]<>ToString[Unique[]]][l,l],{i,Length[newvar]}];
newrepl=MapThread[Rule[#1,Cancel[Together[#2//.repl]]]&,{newname,newvar}];
expr=expr/.MapThread[#1->#2&,{newvar,newname}];
repl=Join[repl,newrepl];
];

If[VerboseTiming>=1,Print[$KernelID,". FOIMain."]];
(*Put[{expr,var,repl,repllim},"~/out.m"];*)
time=First[Timing[{relevant,irrelevant}=RelevantFactor[expr,var,repl,repllim]]];
If[VerboseTiming>=1,Print[$KernelID,". RelevantFactor ",time]];
If[Map[Factor,DeleteCases[Map[First,relevant]/.var->0],0]==={},Return[(Times@@Power@@@relevant)*irrelevant]];

time=First[Timing[{relevant,irrelevant,cnewsol,csol}=ScaleOut[relevant,irrelevant,var,repl,repllim]]];
If[VerboseTiming>=1,Print[$KernelID,". ScaleOut ",time]];

Return[{Cancel[(Times@@Power@@@relevant)]*irrelevant,cnewsol,csol}];
];


(*Options[IsolateJacobian]={ReturnList->False};
IsolateJacobian[expr_,heads_List,OptionsPattern[]]:=Module[{list,tf,meas,retexpr},
list=FactorList[expr];
tf=Map[FreeQ[#,Apply[Alternatives,heads]]&,list];
meas=Times@@Power@@@Pick[list,tf,False];
retexpr=Pick[list,tf,True];
If[!OptionValue[ReturnList],retexpr=Times@@Power@@@retexpr];
(*Print[tf];*)
Return[{meas,retexpr}]
];*)


Options[IsolateJacobian]={ReturnList->False};
IsolateJacobian[list_,heads_List,OptionsPattern[]]:=Module[{expr,expr2,rem,count,tf},
expr=list;
expr2=1;
rem=expr;
count=1;
While[rem=!=1&&count<4,
count=count+1;
expr=If[Head[#]===Times,List@@#,{#}]&[expr];
tf=Map[FreeQ[#,Alternatives@@heads]&,expr];
rem=Times@@Pick[expr,tf,True];
expr=Times@@Pick[expr,tf,False];
expr2=expr2*rem;
expr=expr//Factor;
];

(*Print["isolate jac", Cancel[expr*expr2/list]\[NotEqual]1];*)
Return[{expr,expr2}]
];


TestMeasure[meas_,replc_,var_]:=Module[{seltiming,measout},
If[Simplify[(meas/.var->0//.{sel[_]->1,Alternatives[J,J1,J2,J3,J4][a_]:>a})]===0,Print["Problem with measure"];
 Print[meas]
];

If[Simplify[(meas/.var->0//.{sel[a_]:>a,Alternatives[J,J1,J2,J3,J4][a_]:>1})]===0,
Print[Style["selector 0."]];
 Print[meas];

  If[OptionValue[CheckSelector]===True,
seltiming=Timing[Cancel[(Cancel[meas//.{Alternatives[J,J1,J2,J3,J4][a_]:>1}/.ReplaceAll[replc,var->0]/.{sel[a_]:>a}]/.var->0)]];
Print["sel 2 ",seltiming[[1]]];
 seltiming= seltiming[[2]],
seltiming=0
 ];

If[seltiming=!=0,
 Print[Style["selector 0. Wrong",Red]],
(*Print["selector 0. Ok"];*)
measout=meas/.var->0;
 ],
measout=meas/.var->0;
];

(*meas=meas/.var->0;*)
If[VerboseFunctions>=3, Print["measure  ",meas]];
];


CancelSimple[weights_]:=Module[{wei},
wei=weights;
wei=If[Head[#]===Times,List@@#,List[#]]&[wei];
wei=Map[If[Head[#]===Plus,List@@#,{#}]&,Expand[wei]];
wei=Map[GatherBy[#,FactorTermsList[Denominator[#]]&]&,wei];
wei=Table[Cancel[Together[Plus@@@i]],{i,wei}];
wei=Table[Cancel[Together[Plus@@i]],{i,wei}];
wei=Cancel[Together[Times@@wei]];
Return[wei];
]


(* ::Subsection::Closed:: *)
(*find solution*)


FindSolutions[eqthis_, eqlist_, scalist_, var_]:=Module[{full,newc,sol,pos,expr,solc,i},
If[VerboseFunctions>=2,Print["FindSolutions"]];
(*full=DeleteDuplicates[Cases[eqlist,c[_][__],-1]];
newc=Take[full,Length[eqlist]]/.c[a_][b__]:>c["Kernel"<>ToString[$KernelID] Unique[]][b];*)
full=DeleteDuplicates[Cases[eqlist,cT[_][_]|c[_][__]|d[_][__]|n[_][__],-1]];
newc=Take[full,Length[eqlist]]/.{
cT[a_][b_]:>c["Kernel"<>ToString[$KernelID] Unique[]][b],
c[a_][b__]:>c["Kernel"<>ToString[$KernelID] Unique[]][b],
d[a_][b__]:>d["Kernel"<>ToString[$KernelID] Unique[]][b],
n[a_][b__]:>n["Kernel"<>ToString[$KernelID] Unique[]][b]};
sol=MapThread[#1==#2 #3&,{eqlist,scalist,newc}];

solc=MapThread[If[#3=!=0,#1->#2/#3,Null]&,{newc,eqlist,scalist}];
solc=Simplify[DeleteCases[solc,Null]];

sol=Solve[sol,full]//Simplify;
newc=Cases[sol,Alternatives@@newc];


pos=Map[PossibleZeroQ[#[[2]]]&,sol,{2}];
sol=Delete[sol,Position[Or@@@pos,True]];



FindSolutions::nosolution="solution is not valid";
For[i=1,i<=Length[sol],i++,
expr=eqthis/.sol[[i]]/.var->0//Factor;
If[expr===0,Return[ExpandPower[{sol[[i]],solc}]](*,Print[expr,"  ",eqthis,"  ",sol]*)];
];
Message[FindSolutions::nosolution];Print[eqthis,"  ",sol,"  " ,solc];Abort[];
];




(* ::Subsection:: *)
(*FactorOutImplicit*)


FactorOutImplicit[expr0_, var_, repl0_] := Module[{timing,
random,expr,repl,repllim,meas,time,exparg,expnum, expden,num, den,factorlist,dofoi=True,factorlim,pos1,pos2,foiloop,pos,exprfoi,foiout,cnewsol,cnewsollim,csol,allcsol={},out,i},
If[VerboseFunctions>=2, Print[Style["FactorOutImplicit Begin", Red]]];
timing=DateList[];
random=Join[Map[#->RandomReal[]&,Complement[{x1, x2,x3, x4,x6,x7,x8},{var}]],Map[#->RandomReal[]&,{ph2,ph3}]];
expr=expr0//ExpandLog;

repl=Join[repl0,{}];
repllim=repl//.Rule[a_,b_]:>Rule[a,Factor[Expand[Quiet[Limit[b,{var->0},Direction->"FromAbove"]]]]];
repllim=ExpandPower[Quiet[Simplify[repllim,TimeConstraint->1]]];
(*Print[repllim];*)
If[VerboseTiming>=1, Print[ "ReplLim: ", {60,1}.((DateList[]-timing)[[5;;6]] )]];
timing=DateList[];

{meas,expr}=IsolateJacobian[ expr,{J,J1,J2,J3,J4,sel}];
If[VerboseTiming>=1, Print["Isolate Jacobian: " , {60,1}.((DateList[]-timing)[[5;;6]] )]];
timing=DateList[];

{meas,expr}={meas,expr}//.{(head:Alternatives[J,J1,J2,J3,J4,sel,HeavisideTheta])[a_]:>head[Cancel[Together[a]]]};
 expr=expr/.{Log[a_]:>Log[Cancel[Together[a]]],PolyLog[2,a_]:>PolyLog[2,Cancel[Together[a]]]}; 

(*TestMeasure[meas,repl,var];*)


If[VerboseTiming>=1, Print[ {60,1}.((DateList[]-timing)[[5;;6]] )]];
timing=DateList[];

time=Timing[ {num, den} =NumeratorDenominator[ Together[Cancel[ExpandPower[expr]]]]];
(*Print["Cancelled ",$KernelID];*)

If[VerboseTiming>=1, Print[$KernelID, " To cancel ",time[[1]]]];
If[expr===0,
	If[VerboseFunctions>=2,
		Print[0,"  ",Style[" FactorOutImplicit End",Green]]
	];
	Return[{expr,{}}]
];
exparg  = DeleteDuplicates[Cases[expr, exp[arg_] :> Cancel[Together[arg]],-1]];
exparg =Last[exparg ];
If[exparg ===0,Print[Style["Exp 0",Red]]];
{expnum, expden} =NumeratorDenominator[ Together[Cancel[ExpandPower[exparg ]]]];
 num = num /. {exp[_] -> 1};
 
factorlist={den,num,expden,expnum};
(*****************************************************************************************)
For[foiloop=1,dofoi,foiloop++,
	If[VerboseFunctions>=2,Print[$KernelID, "  " ,foiloop," th loop"]];

	If[foiloop>20,Print["something is wrong"];Break[]];

	factorlim=Map[Factor,factorlist//.Flatten[(allcsol/.var->0)]/.var->0];
	
	Do[
		If[factorlim[[1]]===0&&factorlim[[2]]===0,
			Print["Denominator/Numerator are not cancelled?!"];
			{num,den}=NumeratorDenominator[ Cancel[Together[Cancel[ExpandPower[(factorlist[[2]]/factorlist[[1]])/.Last[allcsol]]]]]] ;
			factorlist=ReplacePart[factorlist,{{2}->num,{1}->den}];
			(*factorlist={den,num,expden,expnum};*)
			factorlim=Map[Factor,factorlist//.Flatten[(allcsol/.var->0)]/.var->0];
			, Break[];
		]
	,{i,0,2}];
	
	(**********************************************************************)
	(*  pos1 is x\[Rule]0 limits of denom. numer.  with posssible hidden scalings*)
	(**************************************************************8*******)
	
	pos1=Position[factorlim,0,{1}];
	(*Print[$KernelID,"  explicitly 0 ", pos1];*)
		(*Print[factorlim, "  ",repllim];*)
	factorlim={factorlim[[1]]/.repllim,factorlim[[2]],factorlim[[3]]/.repllim,factorlim[[4]]};
	factorlim=Map[Factor,factorlim];
	(*********************************************)
	(*  pos2 is full x\[Rule]0 limits of denom. numer.  *)
	(*********************************************)
	pos2=Position[factorlim,0,{1}];

	(*Print[$KernelID," Implicitly 0 " ,pos2];*)
	(*Print[pos1,pos2];*)

	If[FreeQ[pos2,1|3],
		If[VerboseFunctions>2,Print["All fine (denominators finite)"]];
		If[!FreeQ[Complement[pos2,pos1],2|4],
			Print["Go further"];
			dofoi=True,
			
			If[VerboseFunctions>1,Print["Nothing to be done"]];
			dofoi=False;
		],
		(*If[VerboseFunctions>1,Print["FOI start"]];*)	
		dofoi=True
	];
	If[!dofoi,Break[]];
	If[SameQ[pos2,pos1],
		factorlim=Map[Quiet[Simplify[#,TimeConstraint->1]]&,ExpandPower[factorlim /.repllim]];(*Do it again here since it takes long*)
		factorlim=factorlim//ExpandPower;
		pos2=Position[factorlim,0,{1}]
	];

	
	If[SameQ[pos2,pos1],
		Print["There is nothing hidden"];
		Print["Zero positions are ",pos1, "  ", pos2];
		Put[factorlist,"./factorlist.m"];
		Put[allcsol,"./csol.m"];
		Put[repllim,"./repllim.m"];
		Abort[]
	];

	pos=First[Complement[pos2,pos1]];
	If[VerboseFunctions>=1,Print[$KernelID, " FOI start ",{"Denominator ","Numerator ","Denominator exp","Numerator exp"}[[pos]] ]];

	(*Print[pos];*)
	exprfoi=factorlist[[Sequence@@pos]];
	{foiout,cnewsol,csol}=FOIMain[exprfoi,repl,repllim,var];
	AppendTo[allcsol,csol];

	time=First[Timing[
	factorlist=ReplacePart[factorlist,{pos->Together[foiout]}]]];
	If[VerboseTiming>=1, Print[$KernelID, ". To cancel 2 ",time]];

	cnewsol=cnewsol//.repl/.{Rule[a_,b_]:>Rule[a,Quiet[Simplify[Cancel[ExpandPower[b]],TimeConstraint->1]]]}//ExpandPower;
	cnewsollim=cnewsol/.{Rule[a_,b_]:>Rule[a,Quiet[Limit[Cancel[b],{var->0},Direction->"FromAbove"]]]};
	cnewsollim=cnewsollim//ExpandPower;
	repl=Join[repl,cnewsol];
	repllim=Join[repllim,cnewsollim];
	
	If[MemberQ[{{1},{2}},pos],
		time=Timing[{num,den}=NumeratorDenominator[Cancel[ Together[Cancel[ExpandPower[factorlist[[2]]/factorlist[[1]]]]]] ]];
		If[VerboseTiming>=1, Print[$KernelID ,". To cancel 3 ",time[[1]]]];
		factorlist=ReplacePart[factorlist,{{2}->num,{1}->den}];
	];
	If[MemberQ[{{3},{4}},pos],
		{expnum,expden}=NumeratorDenominator[ Cancel[Together[Cancel[ExpandPower[factorlist[[4]]/factorlist[[3]]]]]]];
		factorlist=ReplacePart[factorlist,{{4}->expnum,{3}->expden}];
	];
];
(****************************************************************************************************)
If[VerboseFunctions>=2,Print[$KernelID,".  loop end. ",foiloop]];

allcsol=Flatten[allcsol];


If[(allcsol=!={}),
If[(VerboseFunctions>=1),Print[$KernelID,"  csol ",allcsol/.var->0]];
repllim=Normal[KeyDrop[Association[repllim],allcsol[[All,1]]]];
];

factorlist=factorlist//.(allcsol/.var->0)/.var->0;
out= meas* Cancel[factorlist[[2]]/factorlist[[1]]] exp[ Cancel[factorlist[[4]]/factorlist[[3]]]];
out=out//.(allcsol/.var->0)/.var->0;
out={out,(*repl,*)repllim};

If[(allcsol=!={}),
If[!FreeQ[out,Alternatives@@allcsol[[All,1]]],
Print["remaining old c "];
Print[DeleteDuplicates[Cases[out,Alternatives@@allcsol[[All,1]],-1]]];
Abort[]];
];
Return[out];
];



(* ::Subsection:: *)
(*Series scaling for irrational functions *)


SeriesScaling[numerator_,repl_,var_]:=Module[{faclist,part1,expr,bb,bbrepl,bbrev,exprbb,constterm,remterm,consttermfaclist,
det0,scaleterm0,scaleterm,ser,scaling,newname,newconsttermfaclist,newrepl,scalingpart,newnumerator,counter=1},
faclist=numerator//FactorList;
part1=Map[And[FreeQ[#,Alternatives[J1,J2,sel,exp]],!NumberQ[#[[1]]],!FreeQ[#,Alternatives[c[_],cT[_]]],!FreeQ[#,Log[_]]]&,faclist];
part1=Position[part1,True]//Flatten(*~th part of faclist to deal with*);

(*If[Length[part1]=!=1,Print[Length[part1],"possible factors in the numerator, provide algorith to choose one!"];Abort[]];*)
(*Print[part1];*)
While[counter<=Length[part1],
expr=(faclist[[part1[[counter]]]][[1]])(*~th part of faclist to deal with*);
(*Print[expr];*)
bb=Variables[expr];
bb=DeleteCases[bb,var];
bbrepl=Map[{#,Unique[]}&,bb];
bbrev=Map[Rule@@Reverse[#]&,bbrepl];
bbrepl=Map[Rule@@#&,bbrepl];
exprbb=expr/.bbrepl;
(*Print[bbrepl];
Print[exprbb];*)
If[Complement[Variables[exprbb],{var},bbrepl[[All,2]]]=!={},Print["variables remaining",Complement[Variables[exprbb],{var},bbrepl[[All,2]]]];Abort[]];
constterm=exprbb/.var->0;
(*Print[ReplaceAll[Factor[constterm],bbrev]];*)
remterm=exprbb-constterm;
If[((remterm//.bbrepl/.var->0)=!=0),Print["Scaling part separation error "]; Abort[]];
consttermfaclist=constterm//FactorList;
det0=ExpandLog[ReplaceRepeated[consttermfaclist/.(bbrev/.var->0/.(repl/.var->0)),combinelog]]//Simplify;
det0=ReplaceRepeated[det0,combinelog]//Simplify;
(*Print[det0];*)
det0=Map[PossibleZeroQ[#[[1]]]&,det0]//Position[#,True]&//Flatten;
If[Length[det0]=!=1,
Print[Length[det0]," Candidate factors in the chosen factor ! "]];
If[Length[det0]>0,Break[],counter+=1;];
];

det0=det0//First;
(*Print[det0];*)
scaleterm0=First[(consttermfaclist[[det0]])];
Print[" SCALING BIT ",scaleterm0/.bbrev];
scaleterm=Simplify[Simplify[scaleterm0/.bbrev]/.repl];
ser=Series[scaleterm,{var,0,1}];
(*Print[List@@ser];*)
ser=Normal[SeriesData@@ExpandLog[Simplify[ReplaceRepeated[ExpandLog[Simplify[List@@ser]],combinelog]]]];
(*Print[ser];*)
scaling=First[ExtractMonomial[ser,{var}]];(*scaling around var\[Equal]0*)
If[scaling===1,Print["noscaling"];Abort[]];
(***********************************************************)
(*          start modifying the numerator                   *)
(***********************************************************)
newname=c["Kernel"<>ToString[$KernelID] Unique[]][l,l];(*new black box*)
(*newFactor List of constant term*)
newconsttermfaclist=ReplacePart[consttermfaclist,{det0,1}->scaling newname];

(*new repl to be added*)
newrepl={newname->Simplify[ser/scaling]}; (*since it can't be sent to 0 later, it is done here!!!!*)
scalingpart=remterm+Times@@Power@@@newconsttermfaclist/.bbrev;(*combine implicitly scaling part and explicitly scaling part*)
(*Print[faclist[[part1[[counter]],1]]];*)
Print["new scaling factor " ,newconsttermfaclist/.bbrev];
newnumerator=Times@@Power@@@ReplacePart[faclist,{part1[[counter]],1}->scalingpart];(*replace scaling part of original numerator*)
If[(newnumerator/.var->0)=!=0,Print["Scaling not right"];Abort[]];
Return[{newnumerator,Join[repl,newrepl]}]
]



(* ::Section:: *)
(*Miscellaneous*)


(* ::Subsection::Closed:: *)
(*Breakc*)


Breakc[replc_]:=Module[{expr,factor,newname,replfactor,newrepl,newreplc},
expr=replc[[All,2]];
expr=Map[FactorList,expr];
factor=Map[First,Join@@expr];
factor=DeleteCases[factor,a_/;NumberQ[a]]//DeleteDuplicates;
newname=Table[c2[0][i],{i,Length[factor]}];
replfactor=MapThread[#1->#2&,{factor,newname}];
newrepl=MapThread[#2->#1&,{factor,newname}];
expr=expr/.replfactor;
expr=Map[Times@@Power@@@#&,expr];
newreplc=MapThread[#1->#2&,{replc[[All,1]],expr}];
If[!FreeQ[newreplc,x1|x2|x3|x4|x6|x7|x8|x9],Print["newreplc is fatally wrong"];Abort[]];
Return[{newreplc,newrepl}]
];


(* ::Subsection:: *)
(*RemoveDuplicates*)


RemoveDuplicates[expr_, repl_] := Module[
{combi, ratio ,tf ,repladd,replrem,scalings, posscal,torescal,selscal ,rulesresc,replresc},
  If[expr==0,  If[VerboseFunctions>=2, Print[{}]; Print[{}]];Return[{expr,{}}]];
  
  (* Form all ratios of c[][] functions *)
  
  combi = Append[repl[[All,1]],1];
  combi = Subsets[combi,{2}]; 
  ratio = MapThread[Cancel[ExpandPower[#1/#2/.repl]]&,Transpose[combi]];
  tf = Map[NumberQ, ratio];
  combi=MapThread[{#1[[1]],#1[[2]] #2}&,{combi,ratio}];
  combi = Pick[combi,tf, True];
  
  (* repladd is a set of replacements in the form c[][__]\[Rule] Num c[][__], or c[][__]\[Rule] Num,
   so the c's that only differ by number are replaced or c that are just number are also replaced. *)
   
  repladd = And@@(Equal@@@combi);
  repladd = Solve[repladd];
  repladd = First[repladd];
  
  If[!FreeQ[repladd[[All,2]],0,1],Print[repladd];Abort[]];
  
  replrem = Normal[KeyDrop[Association[repl],repladd[[All,1]]]];
  (******************************************************************************************)
   (* Checks scalings in replrem and determine positions of corresp. entries *)
  scalings = Times @@ (Cases[#, (List[v_, pow_] /; !  FreeQ[Join@@Map[{Sqrt[#],#}&,activevariables](*{Sqrt[x1], Sqrt[x2],
		       Sqrt[x3], Sqrt[x4], x1,x2,x3, x4, 0}*), v]) :> v^pow, -1]) & /@ (FactorList[ Cancel[#]] & /@replrem[[All, 2]]);
  posscal = Flatten[Position[scalings, x1^pow_.|x2^pow_.|x3^pow_.|x4^pow_.]]; 
  
  (* Entries to rescale and scalings *)
  torescal = Part[replrem, posscal];
  selscal = Part[scalings, posscal];

  (* Rescaled c[][] symbols *)
  replresc = MapThread[Rule[#1, #2 #1 ]&, {torescal[[All,1]], selscal}];

  (* Rescaled rules for the tne c[][] functions*)
  rulesresc = MapThread[Rule[#1[[1]], Cancel[#1[[2]]/#2]] &, {torescal, selscal}];(*Cancel added T*)

  (* Final list of replacements rules: without duplicates and rescalings *)
  replrem = Join[Delete[replrem, {#} &/@ posscal], rulesresc];

  If[VerboseFunctions>=2, Print[repladd]; Print[replresc]];
  
  
 
  Return[{expr/.sel[arg_]:>sel[Cancel[arg]]//.repladd/.replresc//Cancel, replrem}]
  
];


(* ::Subsection::Closed:: *)
(*ExpandList*)


ExpandList[expr_]:=Module[{out},
out=expr//Expand;
If[Head[out]===Plus,out=List@@out,out={out}];
Return[out]]


(* ::Subsection::Closed:: *)
(*SplitScaling*)


SplitScaling[myme_,replsp_]:=Module[{scalingparts,names,mymeframe,mymescale,explist,monlists,scalingtypes,posit,factors,replsc,mymescaling},
scalingparts=myme//DeleteDuplicates[Cases[#,SCALE[a_],{0,Infinity}]]&;
names=Table["sc"<>ToString[i],{i,Length[scalingparts]}];
If[VerboseFunctions>=1,Print[Thread[scalingparts->names]]];
mymeframe=myme//.Thread[scalingparts->names];

If[(mymeframe/.Map[#->0&,names])=!=0,Print[mymeframe/.Map[#->0&,names]];Print["SCALE[] is absent in some terms"];Abort[]]; 

mymescale=Map[Coefficient[mymeframe,#]&,names];
explist=Map[ExpandList,scalingparts/.SCALE[a_]:>a,{1}];

explist=explist/.replsp//ExpandPower[#,exclusions->{}]&;

monlists=Map[ExtractMonomial,explist,{2}];

scalingtypes=Union@@Map[First,monlists,{2}];
posit=Table[
Map[Position[Map[First,monlists[[ii]]],#,1]&,scalingtypes],
{ii,Length[monlists]}];

posit=Map[Flatten,posit,{2}];

If[VerboseFunctions>=2,Print[scalingtypes]];
factors=Table[Map[J4[Apply[Plus,monlists[[ii,#,2]]]]&,posit[[ii]]],
{ii,Length[monlists]}];
replsc=MapThread[Thread[#1->#2]&,{names,factors}];
replsc=Transpose[replsc]/.{J4[a_]/;NumberQ[a]:>a};
(*Print[replsc];*)
mymescaling=Table[{scalingtypes[[ii]],mymeframe/.replsc[[ii]]},{ii,Length[scalingtypes]}];
Return[mymescaling]
]


(* ::Subsection::Closed:: *)
(*AbstractFactors*)


AbstractFactors[expr_,factors_]:=Module[{tf=False,
count=0,exprrepl=expr,
names={},
replall={},repl,cases,repl1,repl2,full,i},

While[!tf,
If[count>100,Break[]];
cases=DeleteDuplicates[Cases[expr,a_/;And[!NumberQ[a],FreeQ[a,Apply[Alternatives,Join[factors,names]]]],{count}]];
repl=Map[#->Unique[]&,cases];
AppendTo[replall,repl];
(*Print[repl];*)
names=Join[names,Map[Last,repl]];
(*Print[names];*)
exprrepl=Replace[exprrepl,repl,count];
tf=SameQ[{},Complement[Variables[exprrepl],Join[names,factors]]];
count+=1];

(*full=Length[replall];
For[i=1,i<full,i++,
replall=Join[(replall[[1;;full-i]])/.Flatten[replall[[full-i+1;;full]]],replall[[full-i+1;;full]]];
];
repl=Flatten[replall];
names=Map[Last,repl];
tf=And[SubsetQ[names,DeleteDuplicates[Cases[First[#],a_/;SameQ[Head[a],Symbol],{0,Infinity}]]],!NumberQ[N[First[#]]]]&/@repl;
repl1=Reverse/@Pick[repl,tf,True];
(*Print[repl1];*)
repl2=Reverse/@Pick[repl,tf,False];
exprrepl=exprrepl(*//.repl1*);*)
replall=Map[Reverse,replall,{2}];
Return[{exprrepl,replall(*repl1,repl2*)}]
];


(* ::Subsection::Closed:: *)
(*deltaScale*)


deltaScale[in_delta]:=Module[{expr,tf,out,stay},
expr=FactorList@@in;
tf =Map[FreeQ[#,sp]&,expr,{1}];
out=Times@@Power@@@Pick[expr,tf,True];
stay=Times@@Power@@@Pick[expr,tf,False];
Return[out^-1 delta[stay]]
];


(* ::Section:: *)
(**)


End[];
EndPackage[];
