(* ::Package:: *)

(* ::Title:: *)
(*Posecpp GRN & GEN*)


(* ::Section:: *)
(*Set Environment*)


(* ::Input:: *)
(*home = "Y:/posecpp";*)


(* ::Section:: *)
(*Generate Network 2*)


(* ::Input:: *)
(*rdata=ReadList[home<>"/recheck/net2_raw.txt",{Number,Number,Real,Real,Real,Real,Real,Real}];*)
(*rgood1=Select[rdata,#[[2]]>100&];*)
(*rgood1[[All,5]]=Sqrt[rgood1[[All,5]]]/(Abs[rgood1[[All,8]]]);*)
(*rgood1[[All,3]]=Sqrt[rgood1[[All,3]]]/(Abs[rgood1[[All,8]]]+1);*)
(*rgood1[[All,4]]=Sqrt[rgood1[[All,4]]]/(Abs[rgood1[[All,8]]]+1);*)
(*rselected1=Select[rgood1,Total[#[[3;;5]]]<1&&Quotient[#[[1]],10000]!=Mod[#[[1]],10000]&];rgraph1=Graph[Map[Quotient[#[[1]],10000]<->Mod[#[[1]],10000]&,rselected1],VertexLabels->"Name"];*)
(**)


(* ::Section:: *)
(*Generate Network 3*)


(* ::Input:: *)
(*el=Map[#[[1]]<->#[[2]]&,ReadList[home<>"/recheck/net3_el_015_02.txt",{Number,Number}]];*)
(*g3 = Graph[el,VertexLabels->"Name"];*)
(*comm3 = FindGraphCommunities[g3]*)


(* ::Section:: *)
(*Reading Global Data*)


(* ::Input:: *)
(*scale= ReadList[home<>"/geo/converge_s.txt",{Number,Real, Real,Real,Real, Real,Real, Real,Real,Real, Real,Real}][[;;,{1,12}]];*)
(*scalemap=Select[(#->s[#])&/@Range[1006],NumberQ[#[[2]]]&];*)
(*scalemap2=Select[(#->s[#] 2)&/@Range[1006],NumberQ[#[[2]]]&];*)
(*scalemap4=Select[(#->s[#] 4)&/@Range[1006],NumberQ[#[[2]]]&];*)
(*xvr= ToExpression//@StringSplit/@ReadList[home<>"/geo/converge_x.txt",Record];*)
(*yvr= ToExpression//@StringSplit/@ReadList[home<>"/geo/converge_y.txt",Record];*)
(*svr= ToExpression//@StringSplit/@ReadList[home<>"/geo/converge_s.txt",Record];*)
(*noisyr= ToExpression//@StringSplit/@ReadList[home<>"/geo/converge_n.txt",Record]/.{false->False,true->True};*)
(*pickFinal[x_]:=x[[;;,{1,-1}]];*)
(*xv=pickFinal[xvr];yv=pickFinal[yvr];sv=pickFinal[svr];nv=pickFinal[noisyr];summary=Join[xv,yv,sv,nv,2];*)
(*(gx[#[[1]]]=#[[2]];gy[#[[1]]]=#[[4]];gs[#[[1]]]=#[[6]];gn[#[[1]]]=#[[8]];)&/@summary;*)
(*xmap=Select[(#->gx[#]+gs[#]/2 )&/@Range[1006],NumberQ[#[[2]]]&];*)
(**)
