(* ::Package:: *)

lq=Import["lq2.m"];

lq=(pm/s1234^3)lq/.eps->ep;
(*compact=AbstractFactors[lq,{CF,CA,TF,ep}];*)

expr=Fold[Coefficient[#1,#2[[1]],#2[[2]]]&,lq,factor] ;
expr=Coefficient[expr,ep,epspow];



ReplVars={s123-> s12+s23+s13,s124-> s14+s24+s12,s134-> s14+s34+s13,s234-> s24+s34+s23,s1234->s14+s24+s34+s12+s13+s23};
Repls={s12->2sp[l1,l2],s13->2sp[l1,l3],s23->2sp[l2,l3],s14->-2sp[l1,p],s34->-2sp[l3,p],s24->-2sp[l2,p],z1->sp[l1,n2]/sp[n2,p],z2->sp[l2,n2]/sp[n2,p],z3->sp[l3,n2]/sp[n2,p]};
lq=expr//.ReplVars//.Repls;
(*lq=lq/.z->zval;*)
ClearAll[expr,ReplVars,Repls];
