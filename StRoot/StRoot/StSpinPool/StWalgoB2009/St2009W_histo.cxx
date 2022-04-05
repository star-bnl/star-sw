// $Id: St2009W_histo.cxx,v 1.20 2011/09/14 14:23:21 stevens4 Exp $
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
  int nCase=18;
  hA[0]=h=new TH1F("muStatEve","W-algo: event count",nCase,0,nCase);
  h->GetXaxis()->SetTitleOffset(0.4);  h->GetXaxis()->SetLabelSize(0.06);  h->GetXaxis()->SetTitleSize(0.05); h->SetMinimum(0.8);
  h->SetLineColor(kBlue);h->SetLineWidth(2);
  h->SetMarkerSize(2);//<-- large text

  char key[][200]={"inp","BHT3Id","L2wId","L2wBits","L2wET","L2wRnd","tpcOn","primVert","vertZ","Pt10",
		   "B-in","B200","TrB","Tr2Cl","eta1","goldW"};
  for(int i=0;i<16;i++) h->Fill(key[i],0.); // preset the order of keys
  
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
  char keyT[][200]={"101","pt1","nHit","Hfrac","Rin","Rout","ptOK","@B","CL","#Delta R","fr24",
		    "noNear","noAway","goldW"};
	     
  for(int i=0;i<14;i++) h->Fill(keyT[i],0.); // preset the order of keys


  hA[21]=h=new TH1F("muTrNfit","primary track  in-selection & vertexZ; nFitPoints",50,0,50);
  Lx=h->GetListOfFunctions();
  ln=new TLine(par_nFitPts,0,par_nFitPts,1.e6);  ln->SetLineColor(kRed);  Lx->Add(ln);

  hA[22]=h=new TH1F("muTrFitFrac","primary track in-selection & vertexZ; nFit/nPoss ",50,0,1.1);
  Lx=h->GetListOfFunctions();
  ln=new TLine(par_nHitFrac,0,par_nHitFrac,1.e6);  ln->SetLineColor(kRed);  Lx->Add(ln);

  hA[23]=h=new TH1F("muTrRxyIn","primary track first hit  in-selection & vertexZ; Rxy (cm)",60,50,170.);
  Lx=h->GetListOfFunctions();
  ln=new TLine(par_trackRin,0,par_trackRin,1.e6);  ln->SetLineColor(kRed);  Lx->Add(ln);

  hA[24]=h=new TH1F("muTrRxyOut","primary track last hit  in-selection & vertexZ; Rxy (cm)",60,100,220.);
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

  hA[35]=new TH1F("muTrch2","track glob chi2/dof X-Y",100,0,5);
  hA[36]=new TH2F("muTrch2b","track glob chi2/dof; chi2/dof  X-Y; last hit eta",30,0,5.,30,-1,1);

  
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
  Lx=h->GetListOfFunctions();
  ln=new TLine(0,par_delR3D,1.e6,par_delR3D);  ln->SetLineColor(kRed);  Lx->Add(ln);
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


  hA[53]=h=new TH2F("muAwayET2Db"," away EMC ET sum  vs. 2x2 cluster ET;  2x2 ET (GeV); away EMC ET (GeV)",50,0,100,80,0,80);


  hA[54]=h=new TH1F("muAwayTotEt"," away-cone TPC+EMC ET sum ; away ET (GeV)",200,0,100);

  hA[55]=h=new TH1F("muEwayET"," ETOW away-cone ET sum;   ET (GeV)",100,0,100); // away side energy  
  
  hA[56]=h=new TH1F("muSmallNearTpcPT",Form("TPC PT in #Delta R =%.1f from lepton candidate; PT (GeV)",par_smallNearDeltaR),100,0,100);
  hA[57]=h=new TH2F("muTr2D1pt5","lastHit on track (pt > 5); detector eta ; detector phi (rad)",100,-1.1,1.1,240,-PI,PI);


  hA[58]=new TH1F("muTrch2West","track glob chi2/dof  West TPC ; chi2/dof",100,0,5);
  hA[59]=new TH1F("muTrch2East","track glob chi2/dof  East TPC ; chi2/dof",100,0,5);

  //... final golden plots ....

  hA[60]=h=new TH2F("muBclETPt"," TPC PT vs.  isolated cluster 2x2 ET, matched;2x2 cluster ET (GeV) ; TPC  PT (GeV)",50,0,100,75,0,150);
  
  hA[61]=new TH1F("mubX7bht3","L2W-BHT3-rnd  events vs. bXing; bXing= raw bx7",128,-0.5,127.5); // filled on input
  hA[62]=h=new TH2F("muEne_Deta","final W: cluster energy vs. detector eta; barrel eta bin; 2x2 Energy (GeV)",40,0,40,50,0,100);

  // free 63-69

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
  hA[99]=h=new TH1F("muWeta","final Ws ; lepton eta",100, -1.5,1.5);

  // free 100-109

  //..... series of electron ET plots after succesive cuts
  char tt2[][200]={"max 2x2","track matched","2x2 / 4x4","no near ET","no away ET"};
  for(int i=0;i<5;i++){
    sprintf(txt,"electron candidate, cut=%s; 2x2 ET (GeV)",tt2[i]);
    sprintf(txt0,"muETlive%d",i);
    hA[110+i]=h=new TH1F(txt0,txt, 100,0,100);
  }
  
  //free 115-131

  hA[117]=h=new TH2F("mujetQAeta_phi","Input Jet phi vs eta ;  eta ; phi ",50,-3,3,63,-PI,PI);
  hA[118]=h=new TH1F("mujetQApt","Input Jet pt; pt;",100,0,100);

  hA[132]=h=new TH2F("muptBalance_clust","ptBalance vs cluster ET; 2x2 Cluster ET; ptBalance",100,0,100,100,0,100);
  hA[133]=h=new TH2F("muptBalance_awayTot","ptBalance vs awayside PT; awayside PT; ptBalance",100,0,100,100,0,100);

  hA[134]=h=new TH2F("musPtBalance_clust","sPtBalance vs cluster ET; 2x2 Cluster ET (GeV); signed Pt balance (GeV)",100,0,100,100,-100,100);
  Lx=h->GetListOfFunctions();
  ln=new TLine(0,par_ptBalance,100,par_ptBalance);  ln->SetLineColor(kRed);  Lx->Add(ln);

  hA[135]=h=new TH2F("musPtBalance_awayTot","sPtBalance vs awayside PT; awayside PT; sPtBalance",100,0,100,100,-100,100);
  Lx=h->GetListOfFunctions();
  ln=new TLine(0,par_ptBalance,100,par_ptBalance);  ln->SetLineColor(kRed);  Lx->Add(ln);


  hA[136]=h=new TH1F("muclustPtBal",Form("PT Balance > %.1f ; 2x2 Cluster ET",par_ptBalance),100,0,100);
  hA[137]=h=new TH1F("muclustPtBal_bckgrd",Form("PT Balance < %.1f ; 2x2 Cluster ET",par_ptBalance),100,0,100);
  hA[138]=h=new TH1F("muclustAwayPt","AwaySide PT < 8 ; 2x2 Cluster ET",100,0,100);
  hA[139]=h=new TH1F("muclustAwayPt_bckgrd","AwaySide PT > 8 ; 2x2 Cluster ET",100,0,100);
  hA[140]=h=new TH1F("muclustPtBalnoE",Form("sPT Balance > %.1f (EEMC not included); 2x2 Cluster ET",par_ptBalance),100,0,100);
  hA[141]=h=new TH1F("muclustAwayPtnoE","AwaySide PT < 8 (EEMC not included); 2x2 Cluster ET",100,0,100);

  // Histograms added for background subtraction and systematic
  char str[200];
  for (int i=0; i<=20; i++) {
    sprintf(str,"neg_failAwaySide_Awayside_pt_bin_%d",i);
    hA[142+i] = new TH2F(str,str,100,0,100,21,0,21);
  }

  for (int i=0; i<=20; i++) {
    sprintf(str,"pos_failAwaySide_Awayside_pt_bin_%d",i);
    hA[163+i] = new TH2F(str,str,100,0,100,21,0,21);
  }

  hA[184+2] = new TH1F("pos_muclustpTbal_wE","pos_muclustpTbal_wE",100,0,100);
  hA[184+1] = new TH1F("neg_muclustpTbal_wE","neg_muclustpTbal_wE",100,0,100);
  hA[184+4] = new TH1F("pos_muclustpTbal_noE","pos_muclustpTbal_noE",100,0,100);
  hA[184+3] = new TH1F("neg_muclustpTbal_noE","neg_muclustpTbal_noE",100,0,100);
  hA[184+6] = new TH1F("pos_muclustpTbal_back","pos_muclustpTbal_back",100,0,100);
  hA[184+5] = new TH1F("neg_muclustpTbal_back","neg_muclustpTbal_back",100,0,100);

  hA[190] = new TH1F("muZvReweight","Reweight Z vertex to match data",100,-200,200);

  //algo efficiency ET dependence
  hA[191]=h=new TH2F("muBclET24R_ET","ratio (2x2/4x4) cluster ET vs 2x2 cluster ET ; 2x2 cluster ET (GeV); fraction: cluster ET 2x2/ 4x4 ET",100,0,100,100,0,1.2);
  Lx=h->GetListOfFunctions();
  ln=new TLine(0,par_clustFrac24,1.e6,par_clustFrac24);  ln->SetLineColor(kRed);  Lx->Add(ln);
  hA[192]=h=new TH2F("muBclEjetE2D_ET","ratio (2x2/nearCone) ET vs. 2x2 cluster ET; 2x2 cluster ET (GeV); ET(cone-2x2) (GeV)",100,0,100,100,0,1.2);
  Lx=h->GetListOfFunctions();
  ln=new TLine(0,par_nearTotEtFrac,1.e6,par_nearTotEtFrac);  ln->SetLineColor(kRed);  Lx->Add(ln);

  //eta dependent signal and background
  hA[200+2] = new TH2F("pos_muclustpTbal_wE_etaBin","pos_muclustpTbal_wE_etaBin",100,-1,1,100,0,100);
  hA[200+1] = new TH2F("neg_muclustpTbal_wE_etaBin","neg_muclustpTbal_wE_etaBin",100,-1,1,100,0,100);
  hA[200+4] = new TH2F("pos_muclustpTbal_noE_etaBin","pos_muclustpTbal_noE_etaBin",100,-1,1,100,0,100);
  hA[200+3] = new TH2F("neg_muclustpTbal_noE_etaBin","neg_muclustpTbal_noE_etaBin",100,-1,1,100,0,100);
  hA[200+6] = new TH2F("pos_muclustpTbal_back_etaBin","pos_muclustpTbal_back_etaBin",100,-1,1,100,0,100);
  hA[200+5] = new TH2F("neg_muclustpTbal_back_etaBin","neg_muclustpTbal_back_etaBin",100,-1,1,100,0,100);

  //eta dependent histos for background subtraction systematic
  for (int i=0; i<=20; i++) {
    sprintf(str,"neg_failsPtBal_sPtBal_bin_%d",i);
    hA[210+i] = new TH2F(str,str,100,-1,1,100,0,100);
  }
  for (int i=0; i<=20; i++) {
    sprintf(str,"pos_failsPtBal_sPtBal_bin_%d",i);
    hA[231+i] = new TH2F(str,str,100,-1,1,100,0,100);
  }

  //some golden W plots for comparison to embedding
  hA[260]=h=new TH2F("muWbX7","L2W-ET events vs. bXing; bXing= raw bx7",100,0,200000,128,-0.5,127.5);
  hA[261]=h=new TH2F("muWNV","#  vertices per event, rank>0 & Z in range; # of vertices",100,0,200000,10,0,10);

  hA[262]=h=new TH2F("muWTrNfit","prim tr nFitP; nFitPoints",100,0,200000,50,0,50);
  hA[263]=h=new TH2F("muWTrFitFrac","prim tr nFitFrac; nFit/nPoss ",100,0,200000,50,0,1.1);
  
  hA[264]=h=new TH2F("muWTrRxyOut","prim tr last hit filter; Rxy (cm)",100,0,200000,60,100,220.);
  hA[265]=h=new TH2F("muWTrRxyIn","prim tr 1st hit  filter; Rxy (cm)",100,0,200000,60,50,170.);
  hA[266]=h=new TH2F("muWTrPt1","primary track PT ; track PT (GeV/c)",100,0,200000,160,0,80);
  hA[267]=h=new TH2F("muWTrch2","track glob chi2/dof X-Y",100,0,200000,100,0,5);
  hA[268]=h=new TH2F("muWTrInvPt","primary track 1/PT",100,0,200000,100,0,0.1);
  hA[269]=h=new TH2F("muWglDcaGold","Track glob vertex abs(DCA), final W; |DCA| (cm)",100,0,200000,100,0,5);
  hA[270]=h=new TH2F("muWglsDcaGold","Track glob signed DCA, final W; sDCA (cm)",100,0,200000,100,-5,5);
  hA[271]=h=new TH2F("muWglDcaZGold","Track glob Z DCA, final W; DCAZ (cm)",100,0,200000,100,-5,5);
  hA[272]=h=new TH2F("muWglPt1TrVert","Track global Pt, final W from one track vertices",100,0,200000,160,0,80);
  hA[273]=h=new TH2F("muWglInvPt1TrVert","Track global 1/Pt, final W from one track vertices",100,0,200000,100,0,0.1);

  //2 eta bins for x-section ratio
  hA[280+2] = new TH2F("pos_muclustpTbal_wE_etaBin2","pos_muclustpTbal_wE_etaBin2",100,0,1,100,0,100);
  hA[280+1] = new TH2F("neg_muclustpTbal_wE_etaBin2","neg_muclustpTbal_wE_etaBin2",100,0,1,100,0,100);
  hA[280+4] = new TH2F("pos_muclustpTbal_noE_etaBin2","pos_muclustpTbal_noE_etaBin2",100,0,1,100,0,100);
  hA[280+3] = new TH2F("neg_muclustpTbal_noE_etaBin2","neg_muclustpTbal_noE_etaBin2",100,0,1,100,0,100);
  hA[280+6] = new TH2F("pos_muclustpTbal_back_etaBin2","pos_muclustpTbal_back_etaBin2",100,0,1,100,0,100);
  hA[280+5] = new TH2F("neg_muclustpTbal_back_etaBin2","neg_muclustpTbal_back_etaBin2",100,0,1,100,0,100);
  //eta dependent histos for background subtraction systematic
  for (int i=0; i<=20; i++) {
    sprintf(str,"neg_failsPtBal_sPtBal_bin_%d_etaBin2",i);
    hA[290+i] = new TH2F(str,str,100,0,1,100,0,100);
  }
  for (int i=0; i<=20; i++) {
    sprintf(str,"pos_failsPtBal_sPtBal_bin_%d_etaBin2",i);
    hA[311+i] = new TH2F(str,str,100,0,1,100,0,100);
  }

  // add histos to the list (if provided)
  for(int i=0;i<mxHA;i++) {
    if(  hA[i]==0) continue;
    HList->Add( hA[i]);
  }
  
  //  HList->ls();
  LOG_INFO<<Form("%s::initHistos done",GetName())<<endm;

}

// $Log: St2009W_histo.cxx,v $
// Revision 1.20  2011/09/14 14:23:21  stevens4
// update used for cross section PRD paper
//
// Revision 1.19  2010/11/09 23:00:50  balewski
// added chi2/dof for East & West TPC separately
//
// Revision 1.18  2010/05/01 01:31:44  balewski
// added W->JJ code & JES calibration
//
// Revision 1.17  2010/04/27 16:53:45  stevens4
// add code to remove events tagged as Zs from W candidates
//
// Revision 1.16  2010/03/23 01:31:40  seelej
// Fix to the filling of the histograms for the background systematic.
//
// Revision 1.15  2010/03/20 18:38:34  balewski
// *** empty log message ***
//
// Revision 1.14  2010/03/18 16:52:17  balewski
// corrected sPtBalance for no-endcap
//
// Revision 1.13  2010/03/14 22:50:31  balewski
// *** empty log message ***
//
// Revision 1.12  2010/02/22 15:49:34  seelej
// Joe : Changes to code for inclusion of background subtraction and systematic studies
//
// Revision 1.11  2010/01/29 01:56:01  stevens4
// disable lepton track reco in TPC sector 20
//
// Revision 1.10  2010/01/28 20:10:05  balewski
// added eta dependent spin sorting
//
// Revision 1.9  2010/01/27 22:12:24  balewski
// spin code matched to x-section code
//
// Revision 1.8  2010/01/18 03:26:15  balewski
// expanded TPC track filtering, not finished
//
// Revision 1.7  2010/01/10 01:45:10  stevens4
// fix plots w/o EEMC in veto
//
// Revision 1.6  2010/01/09 02:29:19  stevens4
// fix histo names
//
// Revision 1.5  2010/01/09 00:07:16  stevens4
// add jet finder
//
// Revision 1.4  2010/01/06 19:16:48  stevens4
// track cuts now on primary component, cleanup
//
// Revision 1.3  2010/01/06 14:11:13  balewski
// one Z-plot added
//
// Revision 1.2  2009/12/08 04:48:35  balewski
// *** empty log message ***
//
// Revision 1.1  2009/11/23 23:00:18  balewski
// code moved spin-pool
//
