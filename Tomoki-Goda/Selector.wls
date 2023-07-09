#!/usr/bin/env wolframscript
(* ::Package:: *)

Quit[]


(* ::Text:: *)
(*This file can be used to generate Selector functions.*)
(**)


(* ::Section:: *)
(*Setup*)


SetDirectory[NotebookDirectory[]]
$Path=Join[$Path,{NotebookDirectory[]<>"Files"}];
positions=All;(*Choose primary sectors *)

name="n3lo";(* Gve a name*)

order=3;(* oder in alpha *)
numberofparticles=3; (* number of real emission*)
zval=1/3; (* vale of v *)
deltaterm=False; (* True if z\[Equal]1*)
If[deltaterm,zval=1];
factor={{CF,2},{TF,1}}; (* Clour factor for which we compute the coefficients.*)
epspow=0; (*order of epsilon already present in the matrix element *)


If[deltaterm,name=name<>"del"];
name=name<>StringJoin@@Map[ToString,Flatten[factor]]<>"n"<>ToString[order]<>"lo"<>"r"<>ToString[numberofparticles]<>"e"<>ToString[If[epspow<0,"m",""]]<>ToString[Abs[epspow]]<>"z"<>ToString[Numerator[zval]]<>"-"<>ToString[Denominator[zval]]
(*bar=True;*)
bar=False;





<<Initialize`
<<SelectorV4`


(* ::Section:: *)
(*66 primary sectors*)


(* ::Text:: *)
(*This is the raw Selector.*)
(*In the second part of each selector function contains "\!\(\*SuperscriptBox[SubscriptBox[\(d\), \(i\)], \(-1\)]\)"  which is the vanishing combination of the corresponding sector.*)
(*Below I show selector for triple collinear pf l1 l2 l3.*)
(*Note,  times[]=combine[] = Times[]*)


Selector66[SeparationLevel->1]
%[[1]]//Simplify


(* ::Section:: *)
(*Reduced (39) sectors*)


(* ::Text:: *)
(*The output of this function contains selector functions in the first part, and identification code in the second. *)
(*The Head "combine" is simply a multiplication of Transverse and Angle& Energy  selectors.*)
(**)


Selector[SeparationLevel->1]
Transpose[%][[1]]/.combine->Times//Simplify



