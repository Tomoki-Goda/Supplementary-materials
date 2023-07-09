(* ::Package:: *)

mat=CF((1+z^2)/(1-z) -ep (1-z));
mat=mat/.z->1/z//Simplify;
mat=4z mat;
mat=(pm/s12)mat/.{s12->-sp[p,l1]};
mat=Coefficient[mat,ep,epspow];
lq=Fold[Coefficient[#1,Sequence@@#2]&,mat,factor]
lq=SCALE[1]lq;
ClearAll[mat];


