(* ::Package:: *)

(* ::Title:: *)
(*Posecpp GRN & GEN*)


BeginPackage["Posecpp`"];


(* ::Section:: *)
(*Generate Network 2*)


Clear[LoadNetwork2Raw];Clear[ConstructNetwork2]
LoadNetwork2Raw[fn_]:=ReadList[fn,{Number,Number,Real,Real,Real,Real,Real,Real}];
ConstructNetwork2[rdata_,mincount_,totalvar_]:=
(* mincout = 100, totalvar=1~.5 *)
Module[{rgood1,rselected1,el},
rgood1=Select[rdata,#[[2]]>mincount&];
rgood1[[All,5]]=Sqrt[rgood1[[All,5]]]/(Abs[rgood1[[All,8]]]);
rgood1[[All,3]]=Sqrt[rgood1[[All,3]]]/(Abs[rgood1[[All,8]]]+1);
rgood1[[All,4]]=Sqrt[rgood1[[All,4]]]/(Abs[rgood1[[All,8]]]+1);
rselected1=Select[rgood1,Total[#[[3;;5]]]<totalvar&&Quotient[#[[1]],10000]!=Mod[#[[1]],10000]&];
el = Map[Quotient[#[[1]],10000]<->Mod[#[[1]],10000]&,rselected1];
el
]
ConstructNetwork2New[rdata_,mincount_,totalvar_]:=
(* mincout = 100, totalvar=1~.5 *)
Module[{rgood1,rselected1,el},
rgood1=Select[rdata,#[[2]]>mincount&];
rgood1[[All,3]]=Sqrt[rgood1[[All,3]]]/(Exp[Abs[rgood1[[All,8]]]]+1);
rgood1[[All,4]]=Sqrt[rgood1[[All,4]]]/(Exp[Abs[rgood1[[All,8]]]]+1);
rselected1=Select[rgood1,Total[#[[3;;5]]]<totalvar&&Quotient[#[[1]],10000]!=Mod[#[[1]],10000]&];
el = Map[Quotient[#[[1]],10000]<->Mod[#[[1]],10000]&,rselected1];
el
]


(* ::Section:: *)
(*Analyze Network 3*)


Clear[LoadNetwork3];
LoadNetwork3[fn_]:=Module[{el,g3,comm3},
Map[#[[1]]<->#[[2]]&,ReadList[fn,{Number,Number}]]]
(*g3 = Graph[el,VertexLabels->"Name"];
comm3 = FindGraphCommunities[g3]];*)


(* ::Section:: *)
(*Reading Global Data*)


Clear[LoadGlobalData]
LoadGlobalData[fnx_,fny_,fns_,fnn_]:=Module[{xvr,yvr,svr,noisyr,pickFinal,xv,yv,sv,nv,summary,gx,gy,gs,gn},
(*scale= ReadList[home<>"/geo/converge_s.txt",{Number,Real, Real,Real,Real, Real,Real, Real,Real,Real, Real,Real}][[;;,{1,12}]];*)
xvr= ToExpression//@StringSplit/@ReadList[fnx,Record];
yvr= ToExpression//@StringSplit/@ReadList[fny,Record];
svr= ToExpression//@StringSplit/@ReadList[fns,Record];
noisyr= ToExpression//@StringSplit/@ReadList[fnn,Record]/.{false->False,true->True};
pickFinal[x_]:=x[[;;,{1,-1}]];
xv=pickFinal[xvr];yv=pickFinal[yvr];sv=pickFinal[svr];nv=pickFinal[noisyr];summary=Join[xv,yv,sv,nv,2];
gx = Association[Map[Function[x,x[[1]]->x[[2]]],summary]];
gy = Association[Map[Function[x,x[[1]]->x[[4]]],summary]];
gs = Association[Map[Function[x,x[[1]]->x[[6]]],summary]];
gn = Association[Map[Function[x,x[[1]]->x[[8]]],summary]];
(*xmap=Select[(#->gx[#]+gs[#]/2 )&/@Range[1006],NumberQ[#[[2]]]&];*)
{gx,gy,gs,gn}
];

SLayer[keys_,ags_]:=Module[{sBin},
sBin = Exp[FindDivisions[Log[{Min[ags],Max[ags]}],10]]//N;
GroupBy[keys,Function[u, LengthWhile[sBin,ags[u]>#&]]]];

Clear[SLayerPlot];
SLayerPlot[ii_,asLayers_\:ff0cacx _,acy_]:=
Module[{pos,idx,name},
name = asLayers[ii];idx={name}//Transpose;
pos=Map[{acx[#],acy[#]}&,idx,{2}];
ListPlot[pos,PlotMarkers->Map[ToString,name],ImageSize->Large]]


EndPackage[]
