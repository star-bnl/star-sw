// $Id: St2009W_histo.cxx,v 1.1 2009/11/23 23:00:18 balewski Exp $
//
//*-- Author : Jan Balewski, MIT

#include <TH2.h>
#include <TList.h>
#include <TLine.h>
#include <TMath.h>

#include "St2009WMaker.h"

//________________________________________________
//________________________________________________
void
St2009WMaker::initHistos(){
  const float PI=TMath::Pi();

  //...... data histograms
  memset(hA,0,sizeof(hA));
  TList *Lx;  TLine *ln;TH1 *h; 
  char txt[1000], txt0[100];
  int nCase=20;
  hA[0]=h=new TH1F("muStatEve","W-algo: event count",nCase,0,nCase);
  h->GetXaxis()->SetTitleOffset(0.4);  h->GetXaxis()->SetLabelSize(0.06);  h->GetXaxis()->SetTitleSize(0.05); h->SetMinimum(0.8);
  h->SetLineColor(kBlue);h->SetLineWidth(2);

  char key[][200]={"inp","BHT3Id","L2wId","L2wBits","L2wET","L2wRnd","tpcOn","primVert","vertZ","Pt10",
	       "B-in","B200","TrB","Tr2Cl","noNear","noAway","goldW","bigAway","goldZ"};
  for(int i=0;i<19;i++) h->Fill(key[i],0.); // preset the order of keys
  
  hA[1]=h=new TH1F("muInTrg","muW input triggers, WARN: scrambled if manyruns are combined by hadd.C; trigID (random order)",nCase,0,nCase);
  h->GetXaxis()->SetLabelSize(0.06);

  hA[2]=h=new TH1F("mubX48","L2W-ET events vs. bXing; bXing= raw bx48",128,-0.5,127.5);
  h->SetFillColor(kGreen);

  hA[3]=h=new TH1F("mubX7","L2W-ET events vs. bXing; bXing= raw bx7",128,-0.5,127.5);
  h->SetFillColor(kBlue);

  hA[4]=new TH1F("mubX48v","L2W-ET & primVertex  vs. bXing; bXing= raw bx48",128,-0.5,127.5);
  hA[5]=h=new TH1F("mubX7v","L2W-ET & primVertex; bXing= raw bx7",128,-0.5,127.5);
  h->SetFillColor(kBlue);

  //... DMS data
  hA[6]=h=new TH1F("muDsm1","L2W-ET events DMS spectrum; DSM value",64,-0.5,63.5); 
  h->SetMinimum(0.8);
  hA[7]=h=new TH1F("muDsm2","L2W-Rnd events DMS spectrum; DSM value",64,-0.5,63.5); 
  h->SetMinimum(0.8);
  sprintf(txt,"L2W-ET events w/ DMS>%d vs.BTOW TP ID bXing; Hanks' TP ID",par_DsmThres);
  hA[8]=new TH1F("muDsm3",txt,300,-0.5,299.5);
  sprintf(txt,"L2W-ET events w/ DMS>%d & primVertexvs.BTOW TP ID bXing; Hanks' TP ID",par_DsmThres);
  hA[9]=h=new TH1F("muDsm4",txt,300,-0.5,299.5);
  h->SetFillColor(kBlue); h->SetLineColor(kBlue);


  //.... vertex histograms .....
  hA[10]=h=new TH1F("muVRf","PPV Vertex rank, funny X-axis; X=Log10(rank)+offset", 150, -9,25);
  Lx=h->GetListOfFunctions();
  ln=new TLine(0,0,0,1.e6);  ln->SetLineColor(kRed);  Lx->Add(ln);


  hA[11]=h=new TH1F("muZv","Z of any vertex w/ rank>0;Z-vertex (cm)",100,-200,200);
  Lx=h->GetListOfFunctions();
  ln=new TLine(par_vertexZ,0,par_vertexZ,1.e6);  ln->SetLineColor(kRed);  Lx->Add(ln);
  ln=new TLine(-par_vertexZ,0,-par_vertexZ,1.e6);  ln->SetLineColor(kRed);  Lx->Add(ln);

  hA[12]=new TH1F("muNV","#  vertices per event, rank>0 & Z in range; # of vertices",10,0,10);

  //..... Tracks....
  hA[20]=h=new TH1F("muStatTrk","W-algo:  track  count; cases",nCase,0,nCase);
  h->GetXaxis()->SetTitleOffset(0.4);  h->GetXaxis()->SetLabelSize(0.06);  h->GetXaxis()->SetTitleSize(0.05); h->SetMinimum(0.8);
  h->SetLineColor(kGreen); h->SetLineWidth(2);
  char keyT[][200]={"101","pt1","nHit","Hfrac","Rin","Rout","ptOK","@B","CL","fr24",
"#Delta R","noNear","noAway","bigAway"};
	     
  for(int i=0;i<14;i++) h->Fill(keyT[i],0.); // preset the order of keys



  hA[21]=h=new TH1F("muTrNfit","global track  in-selection & vertexZ; nFitPoints",50,0,50);
  Lx=h->GetListOfFunctions();
  ln=new TLine(par_nFitPts,0,par_nFitPts,1.e6);  ln->SetLineColor(kRed);  Lx->Add(ln);

  hA[22]=h=new TH1F("muTrFitFrac","global track in-selection & vertexZ; nFit/nPoss ",50,0,1.1);
  Lx=h->GetListOfFunctions();
  ln=new TLine(par_nHitFrac,0,par_nHitFrac,1.e6);  ln->SetLineColor(kRed);  Lx->Add(ln);

  hA[23]=h=new TH1F("muTrRxyIn","global track first hit  in-selection & vertexZ; Rxy (cm)",60,50,170.);
  Lx=h->GetListOfFunctions();
  ln=new TLine(par_trackRin,0,par_trackRin,1.e6);  ln->SetLineColor(kRed);  Lx->Add(ln);

  hA[24]=h=new TH1F("muTrRxyOut","global track last hit  in-selection & vertexZ; Rxy (cm)",60,100,220.);
  Lx=h->GetListOfFunctions();
  ln=new TLine(par_trackRout,0,par_trackRout,1.e6);  ln->SetLineColor(kRed);  Lx->Add(ln);

  hA[25]=h=new TH1F("muTrPt1","global track PT ; track PT (GeV/c)",160,0,80);
  Lx=h->GetListOfFunctions();
  ln=new TLine(par_trackPt,0,par_trackPt,1.e6);  ln->SetLineColor(kRed);  Lx->Add(ln); h->SetFillColor(kYellow);


  hA[26]=h=new TH2F("muTr2D1","lastHit on track; detector eta ; detector phi (rad)",100,-1.1,1.1,200,-3.2,3.2);

  hA[27]=h=new TH1F("muTrPt1N","global NEGATIVE track PT; track PT (GeV/c)",160,0,80);
  h->SetFillColor(7);

  hA[28]=h=new TH2F("muTrdEdX"," dEdX vs. momentum; track P (GeV); dE/dx (keV)",20,0,10,100,0,10);


  hA[29]=h=new TH1F("muTrPt1Pr","primary  track PT; track PT (GeV/c)",160,0,80);
  hA[30]=h=new TH1F("muTrPt1NPr","primary  NEGATIVE track PT; track PT (GeV/c)",160,0,80);


  //..... BTOW .....

  hA[31]=h=new TH1F("muBmaxAdc","BTOW maxADC in event, in-selection; max tower ADC",200,0,5500);
  hA[32]=h=new TH1F("muBtotAdc","BTOW sum of ADC>thres , in-selection;ADC sum/event", 120,0,12000.);

  hA[33]=h=new TH1F("muBclET","matched BTOW 2x2 cluster ET  ;cluster  ET (GeV)",100,0,100);
  Lx=h->GetListOfFunctions();
  ln=new TLine(par_clustET,0,par_clustET,1.e6);  ln->SetLineColor(kRed);  Lx->Add(ln);

  hA[34]=h=new TH2F("muBclAdcPt"," matched  TPC PT vs. 2x2 cluster ADC sum ; cluster (ADC sum);TPC  PT (GeV)",50,0,5000,75,0,150);

  // free 35, 36
  
  //.... 4x4 cluster
  hA[37]=h=new TH1F("muBclET24","matched BTOW 4x4 cluster ET ;cluster 4x4  ET (GeV)",100,0,100);
   hA[38]=h=new TH2F("muBclE242D","Excess energy in 4x4 cluster vs. 2x2 E;2x2 cluster E (GeV); E(4x4)-E(2x2)  E (GeV)",50,0,80,50,0,60);

   hA[39]=h=new TH1F("muBclET24R"," ratio (2x2/4x4) cluster ET ; fraction: cluster ET 2x2/ 4x4 ET",100,0,1.2);
  Lx=h->GetListOfFunctions();
  ln=new TLine(par_clustFrac24,0,par_clustFrac24,1.e6);  ln->SetLineColor(kRed);  Lx->Add(ln);

   //..... jet energy , fraction
   
   hA[40]=h=new TH1F("muBjetET"," near 'EM jet' ET ; 'EM jet'  ET (GeV)",100,0,100);

   hA[41]=h=new TH2F("muBclEjetE2D","Excess nearCone ET  vs. 2x2 E;2x2 cluster ET (GeV); ET(cone-2x2) (GeV)",50,0,80,50,0,60);

   hA[42]=h=new TH1F("muBjetETR"," ratio (2x2/nearCone) ET ; cluster ET/ near cone ET",100,0,1.2);
  Lx=h->GetListOfFunctions();
  ln=new TLine(par_nearTotEtFrac,0,par_nearTotEtFrac,1.e6);  ln->SetLineColor(kRed);  Lx->Add(ln);
  

  // .... track-EMC distance cuts
  hA[43]=h=new TH2F("muBdist1","3D Distance(track-cluster) vs. 2x2 E;2x2 cluster E (GeV); | distance | (cm)",50,0,80,50,0,25);
  hA[44]=h=new TH2F("muBdist2","#Delta Z   (track-cluster) vs.Z-clust; Z-cluster (cm); #Delta Z (cm)",100,-300,300,40,-20,20);
  hA[45]=h=new TH2F("muBdist3","R#Delta #phi   (track-cluster) vs. 2x2 E;2x2 cluster E (GeV); R#Delta #phi (cm)",50,0,80,80,-20,20);
  hA[46]=h=new TH1F("muBdist4","3D Distance(track-cluster) vs. 2x2 E;| 3D distance |   (cm)",100,0,50);
  Lx=h->GetListOfFunctions();
  ln=new TLine(par_delR3D,0,par_delR3D,1.e6);  ln->SetLineColor(kRed);  Lx->Add(ln);

  //.. continue same side jet veto
  hA[47]=h=new TH1F("muTjetET"," quenched near 'TPC jet' PT ; 'TPC jet'  PT (GeV)",100,0,100);
  hA[48]=h=new TH2F("muTjetBjet2D","quenched  near 'TPC jet' vs.  near 'EMC jet' ; EMC jet ET (GeV); TPC PT  (GeV/c)",50,0,80,50,0,60);
  hA[49]=h=new TH1F("muTBjetET"," near 'TPC+EMC jet' ET ; jet ET (GeV), no double counting",100,0,100);

  //.. away-side jet  veto
  hA[50]=h=new TH1F("muTwayET"," TPC away-cone PT sum;   PT (GeV)",100,0,100);
  hA[51]=h=new TH1F("muBwayET"," BTOW away-cone ET sum;   ET (GeV)",100,0,100);
  hA[52]=h=new TH2F("muTotwayET2D"," away TPC+EMC ET sum  vs. 2x2 cluster ET;  2x2 ET (GeV); away ET (GeV)",50,0,100,150,0,100);
  Lx=h->GetListOfFunctions();
  ln=new TLine(0,par_awayTotET,200,par_awayTotET);  ln->SetLineColor(kRed);  Lx->Add(ln);


  hA[53]=h=new TH2F("muAwayET2Db"," away EMC ET sum  vs. 2x2 cluster ET;  2x2 ET (GeV); away EMC ET (GeV)",50,0,100,80,0,80);


  hA[54]=h=new TH1F("muAwayTotEt"," away-cone TPC+EMC ET sum ; away ET (GeV)",200,0,100);
  Lx=h->GetListOfFunctions();
  ln=new TLine(par_awayTotET,0,par_awayTotET,1e6);  ln->SetLineColor(kRed);  Lx->Add(ln);

  hA[55]=h=new TH1F("muEwayET"," ETOW away-cone ET sum;   ET (GeV)",100,0,100); // away side energy  

  //free 56-59

  //... final golden plots ....

  hA[60]=h=new TH2F("muBclETPt"," TPC PT vs.  isolated cluster 2x2 ET, matched;2x2 cluster ET (GeV) ; TPC  PT (GeV)",50,0,100,75,0,150);
  
  hA[61]=new TH1F("mubX7bht3","L2W-BHT3-rnd  events vs. bXing; bXing= raw bx7",128,-0.5,127.5);
  // free 62-69

  //..... BSMD ......reserve 2x10= [70:e.... 80:p.... 89]
  const char cPlane[ mxBSmd]={'E','P'};
  for(int iep=0;iep<mxBSmd;iep++){
    sprintf(txt0,"muS%cadc1",cPlane[iep]);
    sprintf(txt,"BSMD-%c whole plane ADC; ADC",cPlane[iep]);
    hA[70+10*iep]=h=new TH1F(txt0,txt,275,0,1100);
  }// end of E-,P-planes

  //.... final Ws
  hA[90]=h=new TH1F("muWET","  Final selection; 2x2 cluster ET (GeV)", 100,0,100);
  Lx=h->GetListOfFunctions();
  ln=new TLine(par_highET,0,par_highET,1.e6);  ln->SetLineColor(kRed);  Lx->Add(ln);

  sprintf(txt," Final W selection, ET>%.0f GeV 'goldenW'; detector eta ; detector phi (rad)",par_highET);
  hA[91]=new TH2F("muW2D1",txt,10,-1.0,1.0,24,-PI,PI);

  hA[92]=new TH2F("muWdedx","Track dEdx, final W selection; 2x2 ET (GeV); dEdx (keV)",100,0,100,100,0,10);
  hA[93]=new TH2F("muWglDca","Track glob vertex abs(DCA), final W ; 2x2 ET (GeV); |DCA| (cm)",100,0,100,100,0,5);
  hA[94]=new TH2F("muWglDcaSP","Track prim POSITIVE glob signed DCA, final W; 2x2 ET (GeV); sDCA (cm)",100,0,100,100,-5,5);
  hA[95]=new TH2F("muWglDcaSN","Track prim NEGATIVE glob signed DCA, final W ; 2x2 ET (GeV); sDCA (cm)",100,0,100,100,-5,5);

  sprintf(txt,"Vertex ID, final W selection, 2x2 ET>%.0f GeV; vertex ID",par_highET);
  hA[96]=h=new TH1F("muWcar1",txt,10,-0.5,9.5);
  h->SetLineColor(kBlue);h->SetLineWidth(3);

  sprintf(txt,"Vertex 'funny' rank, final W selection, 2x2 ET>%.0f GeV;  X=Log10(rank)+offset",par_highET);
  hA[97]=new TH1F("muWcar2",txt, 150, -9,25);
  
  sprintf(txt,"Vertex Z , final W selection, 2x2 ET>%.0f GeV; Z(cm)",par_highET);
  hA[98]=new TH1F("muWcar3",txt, 100, -200,200);

  // free 98-109

  //..... series of electron ET plots after succesive cuts
  char tt2[][200]={"max 2x2","track matched","no near ET","no away ET"};
  for(int i=0;i<4;i++){
    sprintf(txt,"electron candidate, cut=%s; 2x2 ET (GeV)",tt2[i]);
    sprintf(txt0,"muETlive%d",i);
    hA[110+i]=h=new TH1F(txt0,txt, 100,0,100);
  }
  
  // free 

  hA[117]=h=new TH2F("awayCone_Clust"," away Cone ET Ratio vs. 2x2 cluster ET;  2x2 ET (GeV); away Cone ET / 2x2 ET ",50,0,100,40,0,2); //JS

  // add histos to the list (if provided)
  for(int i=0;i<mxHA;i++) {
    if(  hA[i]==0) continue;
    HList->Add( hA[i]);
  }
  
  //  HList->ls();
  LOG_INFO<<Form("%s::initHistos done1",GetName())<<endm;

}

// $Log: St2009W_histo.cxx,v $
// Revision 1.1  2009/11/23 23:00:18  balewski
// code moved spin-pool
//
