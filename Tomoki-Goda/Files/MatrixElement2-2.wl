(* ::Package:: *)

expr=(ep TF CF (s23-s123 (z2+z3)))/s23+(s123 TF CF (-((-(2 s12 z3)+2 s13 z2+s23 (z2-z3))^2/(s123 s23 (z2+z3)^2))-s23/s123+(4 z1+(z2-z3)^2)/(z2+z3)+z2+z3))/(2 s23)
ReplVars={s123-> s12+s23+s13};
Repls={s12->2sp[l1,l2],s13->2sp[l1,l3],s23->2sp[l2,l3],s14->-2sp[l1,p],s34->-2sp[l3,p],s24->-2sp[l2,p],z1->sp[l1,n2]/sp[n2,p],z2->sp[l2,n2]/sp[n2,p],z3->sp[l3,n2]/sp[n2,p]};
expr=4 z (pm/s123^2)expr//.ReplVars/.{s12->s14,s13->s24,s23->s12}/.{z1->1/z,z2->-z1/z,z3->-z2/z}//Simplify
expr=expr//.Repls;
expr=expr//Collect[#,ep,Simplify]&
(*lq=lq/.z->zval;*)
expr=Fold[Coefficient[#1,#2[[1]],#2[[2]]]&,expr,factor] ;
lq=Coefficient[expr,ep,epspow];
ClearAll[(*expr,*)ReplVars,Repls];



