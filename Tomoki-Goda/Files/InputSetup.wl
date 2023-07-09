(* ::Package:: *)

If[!ValueQ[cstog],
Import["./Files/MatrixElement"<>ToString[order]<>"-"<>ToString[numberofparticles]<>".wl"];
cstog=lq//.Replp/.eps->ep;
]
(*cstog=ctog/.z\[Rule]zval;*)


If[bar===True,
	measureorig= Product[TEST[ First[Flatten[Position[li,lj]]]][delta[sp[lj,lj]]d[d,lj]]TEST[First[Flatten[Position[li,lj]]]][sp[n1,lj]^-ap],{lj,li}]delta[sp[n2,Plus@@li]-(1-z)sp[n2,p]],
	measureorig= Product[TEST[ First[Flatten[Position[li,lj]]]][delta[sp[lj,lj]] d[d,lj]]TEST[First[Flatten[Position[li,lj]]]][sp[n2,lj]^-ap],{lj,li}]delta[sp[n2,Plus@@li]-(1-z)sp[n2,p]]
];


measureorig=measureorig//.Replp;




CompactForm=True;
ktpow=1;
If[CompactForm,
Print[Style["CompactForm==True. spT[k,k] omitted",Red]];
myme=-exp[-spT[Plus@@li,Plus@@li]]cstog,
myme=spT[Plus@@li,Plus@@li]^ktpow exp[-spT[Plus@@li,Plus@@li]]cstog
];
myme=myme(*//.ReplspTArgSum*)//.ReplFuncArgSum/.prop[arg_]:>arg//.sp[Times[-1,arg1_],arg2_]:> -sp[arg1,arg2](*/.ReplspT*)//.Replp/.sp[arg_,arg_]->0//Cancel;



fullvariables=Cases[myme,sp[__]|spT[__],-1]//Variables
reducedvariables=Cases[fullvariables//.ReplFuncArgSum//.ReplspT//.ReplFuncArgSum,sp[__]|spT[__],-1]//.Replp/.pm->1//Variables


ClearAll[lq,ReplVars,Repls,cstog,ktpow,coeffCF2TF]


{selectors,inputraw}=Selector[];
input=Map[InputFormat[Sequence@@#]&,inputraw];
parametrizations=Map[FreeParametrization[Sequence@@#]&,input];





variables//.ReplFuncArgSum
