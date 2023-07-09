(* ::Package:: *)

lq=Import["./oneloop/lq1L.m"];
lq=(pm/s123^2)lq;

$Assumptions=Simplify[$Assumptions&&And@@Map[#>0&,{s23,z1,z2,z3}]&&And@@Map[#<0&,{s12,s123,s13}]//DeleteCases[#,List[__]]&,Assumptions->True];
ReplVars={s123-> s12+s23+s13};
Repls={s12->-2sp[p,l1],s13->-2sp[p,l2],s23->2sp[l1,l2],(*z1->sp[p,n2]/sp[n2,p],*)z2->sp[l1,n2]/sp[n2,p],z3->sp[l2,n2]/sp[n2,p]};
Replz={z->1-z2-z3}
ReplPolyLog=PolyLog[2,1-s23/s123]:>-PolyLog[2,s23/s123]+Pi^2/6-Log[s23/s123]Log[1-s23/s123]

compact=AbstractFactors[lq,{CF,CA,TF,ep}];

expr=Fold[Coefficient[#1,#2[[1]],#2[[2]]]&,compact[[1]],factor] ;
expr=Coefficient[expr,ep,epspow];
expr=expr//.Flatten[compact[[2]]];
expr=expr//.ReplPolyLog;
expr=expr//.Log[s23/s123]->-I Pi+Log[-(s23/s123)]//ExpandLog;
expr=expr/.Complex[a_,b_]:>a+com b;
frame=AbstractFactors[expr,{com}];
expr=CoefficientList[frame[[1]],com];
If[Length[expr]>2,Print["eror"]];
expr=expr[[1]]//Simplify;
expr=expr//.Flatten[frame[[2]]];

lq=expr//.ReplVars//.Repls;
lq=lq/.z->zval/.ep2->ep/.\[Delta]RS->1/.\[Mu]->1/.i0->0;


ClearAll[expr,ReplVars,Repls,ReplPolyLog,frame];
