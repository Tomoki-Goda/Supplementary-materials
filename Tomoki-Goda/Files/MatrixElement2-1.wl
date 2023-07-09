(* ::Package:: *)

mat=Import["./NNLO/MatrixElement.wl"];


expr=sp[n2,p]/s12  mat;
expr=expr/.Power[a_,b_]:>Power[a,b/.ep->ep2]/;!FreeQ[b,ep];
expr=expr/.{s12->-2sp[p,l1]};
Print[epspow];
expr=expr//Coefficient[#,ep,epspow]&;
expr=Fold[Coefficient[#1,Sequence@@#2]&,expr,factor]
lq=expr/.ep2->ep//.Replp//Simplify
















