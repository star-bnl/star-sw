// $Id: St2011W_histo.cxx,v 1.18 2016/01/08 02:08:49 jlzhang Exp $
//
//*-- Author : Jan Balewski, MIT

#include <TH2.h>
#include <TList.h>
#include <TLine.h>
#include <TMath.h>

#include "St2011WMaker.h"

//________________________________________________
//________________________________________________
void
St2011WMaker::initHistos(){
  const float PI=TMath::Pi();

  //...... data histograms
  memset(hA,0,sizeof(hA));
  TList *Lx;  TLine *ln;TH1 *h; 
  char txt[1000], txt0[100];
  int nCase=20;
  hA[0]=h=new TH1F("muStatEve",Form("Barrel W-algo: event count : %s",coreTitle.Data()),nCase,0,nCase);
  h->GetXaxis()->SetTitleOffset(0.4);  h->GetXaxis()->SetLabelSize(0.06);  h->GetXaxis()->SetTitleSize(0.05); h->SetMinimum(0.8);
  h->SetLineColor(kBlue);h->SetLineWidth(2);
  h->SetMarkerSize(2);//<-- large text

  char key[][200]={"inp","L2bwId","L2bwBits","L2bwET","L2bwRnd","tpcOn","primVert","vertZ","Pt10",
		   "B-in","B200","TrB","Tr2Cl","eta1","noNear","noAway","goldW","goldW+","goldW-"};
  for(int i=0;i<19;i++) h->Fill(key[i],0.); // preset the order of keys
  
  hA[1]=h=new TH1F("muInTrg","mu Barrel W input triggers, WARN: scrambled if manyruns are combined by hadd.C; trigID (random order)",nCase,0,nCase);
  h->GetXaxis()->SetLabelSize(0.06);

  hA[2]=h=new TH1F("mubX48","L2WB-ET events vs. bXing; bXing= raw bx48",128,-0.5,127.5);
  h->SetFillColor(kGreen);

  hA[3]=h=new TH1F("mubX7","L2WB-ET events vs. bXing; bXing= raw bx7",128,-0.5,127.5);
  h->SetFillColor(kBlue);

  hA[4]=new TH1F("mubX48v","L2WB-ET & primVertex  vs. bXing; bXing= raw bx48",128,-0.5,127.5);
  hA[5]=h=new TH1F("mubX7v","L2WB-ET & primVertex; bXing= raw bx7",128,-0.5,127.5);
  h->SetFillColor(kBlue);

  //... DMS data
  hA[6]=h=new TH1F("muDsm1","L2WB-ET events DMS spectrum; DSM value",64,-0.5,63.5); 
  h->SetMinimum(0.8);
  hA[7]=h=new TH1F("muDsm2","L2WB-Rnd events DMS spectrum; DSM value",64,-0.5,63.5); 
  h->SetMinimum(0.8);
  sprintf(txt,"L2WB-ET events w/ DMS>%d vs.BTOW TP ID bXing; Hanks' TP ID",par_DsmThres);
  hA[8]=new TH1F("muDsm3",txt,300,-0.5,299.5);
  sprintf(txt,"L2WB-ET events w/ DMS>%d & primVertexvs.BTOW TP ID bXing; Hanks' TP ID",par_DsmThres);
  hA[9]=h=new TH1F("muDsm4",txt,300,-0.5,299.5);
  h->SetFillColor(kBlue); h->SetLineColor(kBlue);


  //.... vertex histograms .....
  hA[10]=h=new TH1F("muVRf","L2WB: PPV Vertex rank, funny X-axis; X=Log(rank)+offset", 150, -9,25);
  Lx=h->GetListOfFunctions();
  ln=new TLine(0,0,0,1.e6);  ln->SetLineColor(kRed);  Lx->Add(ln);


  hA[11]=h=new TH1F("muZv","L2WB: Z of any vertex w/ rank>0;Z-vertex (cm)",100,-200,200);
  Lx=h->GetListOfFunctions();
  ln=new TLine(par_vertexZ,0,par_vertexZ,1.e6);  ln->SetLineColor(kRed);  Lx->Add(ln);
  ln=new TLine(-par_vertexZ,0,-par_vertexZ,1.e6);  ln->SetLineColor(kRed);  Lx->Add(ln);

  hA[12]=new TH1F("muNV","L2WB: # vertices per event, rank>0 & Z in range; # of vertices",10,0,10);
  hA[13]=h=new TH1F("muZdcx","zdcx rate; zdcx rate (kHz)",500,0.,5e5);

  //..... Tracks....
  hA[20]=h=new TH1F("muStatTrk","Barrel W-algo: track  count; cases",nCase,0,nCase);
  h->GetXaxis()->SetTitleOffset(0.4);  h->GetXaxis()->SetLabelSize(0.06);  h->GetXaxis()->SetTitleSize(0.05); h->SetMinimum(0.8);
  h->SetLineColor(kGreen); h->SetLineWidth(2);
  char keyT[][200]={"flag","pt1","nHit","Hfrac","Rin","Rout","ptOK","@B","CL","fr24",
		    "#Delta R","eta1","noNear","noAway","goldW"};
	     
  for(int i=0;i<15;i++) h->Fill(keyT[i],0.); // preset the order of keys


  hA[21]=h=new TH1F("muTrNfit","Barrel: primary track in-selection & vertexZ; nFitPoints",50,0,50);
  Lx=h->GetListOfFunctions();
  ln=new TLine(par_nFitPts,0,par_nFitPts,1.e6);  ln->SetLineColor(kRed);  Lx->Add(ln);

  hA[22]=h=new TH1F("muTrFitFrac","Barrel: primary track in-selection & vertexZ; nFit/nPoss ",50,0,1.1);
  Lx=h->GetListOfFunctions();
  ln=new TLine(par_nHitFrac,0,par_nHitFrac,1.e6);  ln->SetLineColor(kRed);  Lx->Add(ln);

  hA[23]=h=new TH1F("muTrRxyIn","Barrel: primary track first hit  in-selection & vertexZ; Rxy (cm)",60,50,170.);
  Lx=h->GetListOfFunctions();
  ln=new TLine(par_trackRin,0,par_trackRin,1.e6);  ln->SetLineColor(kRed);  Lx->Add(ln);

  hA[24]=h=new TH1F("muTrRxyOut","Barrel: primary track last hit  in-selection & vertexZ; Rxy (cm)",60,100,220.);
  Lx=h->GetListOfFunctions();
  ln=new TLine(par_trackRout,0,par_trackRout,1.e6);  ln->SetLineColor(kRed);  Lx->Add(ln);
  
  hA[25]=h=new TH1F("muTrPt1","Barrel: global track PT ; track PT (GeV/c)",160,0,80);
  Lx=h->GetListOfFunctions();
  ln=new TLine(par_trackPt,0,par_trackPt,1.e6);  ln->SetLineColor(kRed);  Lx->Add(ln); h->SetFillColor(kYellow);


  hA[26]=h=new TH2F("muTr2D1","Barrel: lastHit on track; detector eta ; detector phi (rad)",100,-1.1,1.1,200,-3.2,3.2);

  hA[27]=h=new TH1F("muTrPt1N","Barrel: global NEGATIVE track PT; track PT (GeV/c)",160,0,80);
  h->SetFillColor(7);

  hA[28]=h=new TH2F("muTrdEdX","Barrel: dEdX vs. momentum; track P (GeV); dE/dx (keV)",20,0,10,100,0,10);


  hA[29]=h=new TH1F("muTrPt1Pr","Barrel: primary  track PT; track PT (GeV/c)",160,0,80);
  hA[30]=h=new TH1F("muTrPt1NPr","Barrel: primary  NEGATIVE track PT; track PT (GeV/c)",160,0,80);


  //..... BTOW .....

  hA[31]=h=new TH1F("muBmaxAdc","Barrel: BTOW maxADC in event, in-selection; max tower ADC",200,0,4500);
  hA[32]=h=new TH1F("muBtotAdc","Barrel: BTOW sum of ADC>thres , in-selection;ADC sum/event", 120,0,12000.);

  hA[33]=h=new TH1F("muBclET","matched BTOW 2x2 cluster ET  ;cluster  ET (GeV)",100,0,100);
  Lx=h->GetListOfFunctions();
  ln=new TLine(par_clustET,0,par_clustET,1.e6);  ln->SetLineColor(kRed);  Lx->Add(ln);

  hA[34]=h=new TH2F("muBclAdcPt","Barrel: matched  TPC PT vs. 2x2 cluster ADC sum ; cluster (ADC sum); TPC  PT (GeV)",50,0,5000,75,0,150);

  hA[35]=new TH1F("muTrch2","Barrel: track glob chi2/dof X-Y",100,0,5);
  hA[36]=new TH2F("muTrch2b","Barrel: track glob chi2/dof; chi2/dof  X-Y; last hit eta",30,0,5.,30,-1,1);

  
  //.... 4x4 cluster
  hA[37]=h=new TH1F("muBclET24","matched BTOW 4x4 cluster ET ;cluster 4x4  ET (GeV)",100,0,100);
   hA[38]=h=new TH2F("muBclE242D","Barrel: Excess energy in 4x4 cluster vs. 2x2 E;2x2 cluster E (GeV); E(4x4)-E(2x2)  E (GeV)",50,0,80,50,0,60);

   hA[39]=h=new TH1F("muBclET24R","Barrel: ratio (2x2/4x4) cluster ET ; fraction: cluster ET 2x2/ 4x4 ET",100,0,1.2);
   Lx=h->GetListOfFunctions();
  ln=new TLine(par_clustFrac24,0,par_clustFrac24,1.e6);  ln->SetLineColor(kRed);  Lx->Add(ln);

   //..... jet energy , fraction
   
   hA[40]=h=new TH1F("muBjetET","Barrel: near 'EM jet' ET ; 'EM jet'  ET (GeV)",100,0,100);

   hA[41]=h=new TH2F("muBclEjetE2D","Barrel: Excess nearCone ET  vs. 2x2 E;2x2 cluster ET (GeV); ET(cone-2x2) (GeV)",50,0,80,50,0,60);

   hA[42]=h=new TH1F("muBjetETR","Barrel: ratio (2x2/nearCone) ET ; cluster ET/ near cone ET",100,0,1.2);
  Lx=h->GetListOfFunctions();
  ln=new TLine(par_nearTotEtFrac,0,par_nearTotEtFrac,1.e6);  ln->SetLineColor(kRed);  Lx->Add(ln);
  

  // .... track-EMC distance cuts
  hA[43]=h=new TH2F("muBdist1","Barrel: 3D Distance(track-cluster) vs. 2x2 E;2x2 cluster E (GeV); | distance | (cm)",50,0,80,50,0,25);
  hA[44]=h=new TH2F("muBdist2","Barrel: #Delta Z   (track-cluster) vs.Z-clust; Z-cluster (cm); #Delta Z (cm)",100,-300,300,40,-20,20);
  hA[45]=h=new TH2F("muBdist3","Barrel: R#Delta #phi   (track-cluster) vs. 2x2 E;2x2 cluster E (GeV); R#Delta #phi (cm)",50,0,80,80,-20,20);
  hA[46]=h=new TH1F("muBdist4","Barrel: 3D Distance(track-cluster) vs. 2x2 E;| 3D distance |   (cm)",100,0,50);
  Lx=h->GetListOfFunctions();
  ln=new TLine(par_delR3D,0,par_delR3D,1.e6);  ln->SetLineColor(kRed);  Lx->Add(ln);

  //.. continue same side jet veto
  hA[47]=h=new TH1F("muTjetET","Barrel: quenched near 'TPC jet' PT ; 'TPC jet'  PT (GeV)",100,0,100);
  hA[48]=h=new TH2F("muTjetBjet2D","Barrel: quenched  near 'TPC jet' vs.  near 'EMC jet' ; EMC jet ET (GeV); TPC PT  (GeV/c)",50,0,80,50,0,60);
  hA[49]=h=new TH1F("muTBjetET","Barrel: near 'TPC+EMC jet' ET ; jet ET (GeV), no double counting",100,0,100);

  //.. away-side jet  veto
  hA[50]=h=new TH1F("muTwayET","Barrel: TPC away-cone PT sum;   PT (GeV)",100,0,100);
  hA[51]=h=new TH1F("muBwayET","Barrel: BTOW away-cone ET sum;   ET (GeV)",100,0,100);
  hA[52]=h=new TH2F("muTotwayET2D","Barrel: away TPC+EMC ET sum  vs. 2x2 cluster ET;  2x2 ET (GeV); away ET (GeV)",50,0,100,150,0,100);


  hA[53]=h=new TH2F("muAwayET2Db","Barrel: away EMC ET sum  vs. 2x2 cluster ET;  2x2 ET (GeV); away EMC ET (GeV)",50,0,100,80,0,80);


  hA[54]=h=new TH1F("muAwayTotEt","Barrel: away-cone TPC+EMC ET sum ; away ET (GeV)",200,0,100);
  Lx=h->GetListOfFunctions();
  ln=new TLine(par_awayET,0,par_awayET,1.e6);  ln->SetLineColor(kRed);  Lx->Add(ln);

  hA[55]=h=new TH1F("muEwayET","Barrel: ETOW away-cone ET sum;   ET (GeV)",100,0,100); // away side energy  
  
  hA[57]=h=new TH2F("muTr2D1pt5","Barrel: lastHit on track (pt > 5); detector eta ; detector phi (rad)",100,-1.1,1.1,240,-PI,PI);


  hA[58]=new TH1F("muTrch2West","Barrel: track glob chi2/dof  West TPC ; chi2/dof",100,0,5);
  hA[59]=new TH1F("muTrch2East","Barrel: track glob chi2/dof  East TPC ; chi2/dof",100,0,5);

  //... final golden plots ....

  hA[60]=h=new TH2F("muBclETPt","Barrel: Awayside TPC PT vs.  isolated cluster 2x2 ET, matched;2x2 cluster ET (GeV) ; Awayside TPC  PT (GeV)",50,0,100,75,0,150);
  
  hA[61]=new TH1F("mubX7bht","L2BW-BHT-rnd  events vs. bXing; bXing= raw bx7",128,-0.5,127.5); // filled on input
  hA[62]=h=new TH2F("muEne_Deta","Barrel W: cluster energy vs. detector eta, final selection; barrel eta bin; 2x2 Energy (GeV)",40,0,40,50,0,100);

  
  // track matched to cluster plots
  hA[63]=h=new TH1F("muTrNfitTr2Cl","Barrel: primary track in-muTr2Cl; nFitPoints",50,0,50);
  Lx=h->GetListOfFunctions();
  ln=new TLine(par_nFitPts,0,par_nFitPts,1.e6);  ln->SetLineColor(kRed);  Lx->Add(ln);

  hA[64]=h=new TH1F("muTrFitFracTr2Cl","Barrel: primary track in-muTr2Cl; nFit/nPoss ",50,0,1.1);
  Lx=h->GetListOfFunctions();
  ln=new TLine(par_nHitFrac,0,par_nHitFrac,1.e6);  ln->SetLineColor(kRed);  Lx->Add(ln);

  hA[65]=h=new TH1F("muTrRxyInTr2Cl","Barrel: primary track first hit  in-muTr2Cl; Rxy (cm)",60,50,170.);
  Lx=h->GetListOfFunctions();
  ln=new TLine(par_trackRin,0,par_trackRin,1.e6);  ln->SetLineColor(kRed);  Lx->Add(ln);

  // free 63-69

  //..... BSMD ......reserve 2x10= [70:e.... 80:p.... 89]
  const char cPlane[ mxBSmd]={'E','P'};
  for(int iep=0;iep<mxBSmd;iep++){
    sprintf(txt0,"muS%cadc1",cPlane[iep]);
    sprintf(txt,"BSMD-%c whole plane ADC; ADC",cPlane[iep]);
    hA[70+10*iep]=h=new TH1F(txt0,txt,275,0,1100);
  }// end of E-,P-planes

  //.... final Ws
  hA[90]=h=new TH1F("muWET",Form("Barrel W: Final Selection : %s; 2x2 cluster ET (GeV)",coreTitle.Data()), 100,0,100);
  Lx=h->GetListOfFunctions();
  ln=new TLine(par_highET,0,par_highET,1.e6);  ln->SetLineColor(kRed);  Lx->Add(ln);

  sprintf(txt,"Barrel W: Final selection, ET>%.0f GeV 'goldenW'; detector eta ; detector phi (rad)",par_highET);
  hA[91]=new TH2F("muW2D1",txt,10,-1.0,1.0,24,-PI,PI);

  hA[92]=new TH2F("muWdedx","Barrel W: Track dEdx, final selection; 2x2 ET (GeV); dEdx (keV)",100,0,100,100,0,20);
  hA[93]=new TH2F("muWglDca","Barrel W: Track glob vertex abs(DCA), final selection ; 2x2 ET (GeV); |DCA| (cm)",100,0,100,100,0,5);
  hA[94]=new TH2F("muWglDcaSP","Barrel W: Track prim POSITIVE glob signed DCA, final selection; 2x2 ET (GeV); sDCA (cm)",100,0,100,100,-5,5);
  hA[95]=new TH2F("muWglDcaSN","Barrel W: Track prim NEGATIVE glob signed DCA, final selection ; 2x2 ET (GeV); sDCA (cm)",100,0,100,100,-5,5);

  sprintf(txt,"Barrel W: Vertex ID, final selection, 2x2 ET>%.0f GeV; vertex ID",par_highET);
  hA[96]=h=new TH1F("muWcar1",txt,10,-0.5,9.5);
  h->SetLineColor(kBlue);h->SetLineWidth(3);

  sprintf(txt,"Barrel W: Vertex 'funny' rank, final W selection, 2x2 ET>%.0f GeV;  X=Log(rank)+offset",par_highET);
  hA[97]=new TH1F("muWcar2",txt, 150, -9,25);
  
  sprintf(txt,"Barrel W: Vertex Z, final selection, 2x2 ET>%.0f GeV; Z(cm)",par_highET);
  hA[98]=new TH1F("muWcar3",txt, 100, -200,200);
  hA[99]=h=new TH1F("muWeta",Form("Barrel W: lepton eta final selection %s ; lepton eta",coreTitle.Data()),400,-2.0,2.0);

  //Q/pt plots
  sprintf(txt,"TPC GLOB Q/PT  ; 2x2 cluster ET (GeV); Q/PT");
  hA[100]=h=new TH2F("muChRecPNg", txt,100,0.,100.,100,-0.1,0.1);
  Lx=h->GetListOfFunctions();
  ln=new TLine(0,0,100,0);  ln->SetLineColor(kMagenta);  Lx->Add(ln);

  sprintf(txt,"TPC PRIM  Q/PT ; 2x2 cluster ET (GeV); Q/PT");
  hA[101]=h=new TH2F("muChRecPNp", txt,100,0.,100.,100,-0.1,0.1);
  Lx=h->GetListOfFunctions();
  ln=new TLine(0,0,100,0);  ln->SetLineColor(kMagenta);  Lx->Add(ln);

  // "straightened" charge separation plots
  sprintf(txt,"TPC GLOB Charge Separation Hyperbola Corrected ; 2x2 cluster ET (GeV); Q*ET/PT");
  hA[102]=h=new TH2F("muChRecHypCorrPNg", txt,100,0.,100.,100,-4,4);
  Lx=h->GetListOfFunctions();
  ln=new TLine(0,0,100,0);  ln->SetLineColor(kMagenta);  Lx->Add(ln);

  sprintf(txt,"TPC PRIM Charge Separation Hyperbola Corrected ; 2x2 cluster ET (GeV); Q*ET/PT");
  hA[103]=h=new TH2F("muChRecHypCorrPNp", txt,100,0.,100.,100,-4,4);
  Lx=h->GetListOfFunctions();
  ln=new TLine(0,0,100,0);  ln->SetLineColor(kMagenta);  Lx->Add(ln);
  
  hA[104]=h=new TH1F("muWtime",Form("Barrel Golden W: unix time final selection %s 3/17/12 - 4/20/12; unixtime (bin = 10 min)",coreTitle.Data()),4896,1331942400,1334880000);

  hA[105]=h=new TH2F("muWfreeQ","reco charge vs. TPC dedx, golden W+; Q *ET/PT;  dEdx (keV)",200,-5,5,100,0,20);

  hA[106]=h=new TH1F("muWzdcx","Barrel Golden W: zdcx rate, final selection ; zdcx rate (kHz)",500,0.,5e5);


  //..... series of electron ET plots after succesive cuts
  char tt2[][200]={"max 2x2","track matched","no near ET","no away ET"};
  for(int i=0;i<4;i++){
    sprintf(txt,"Barrel electron candidate, cut=%s; 2x2 ET (GeV)",tt2[i]);
    sprintf(txt0,"muETlive%d",i);
    hA[110+i]=h=new TH1F(txt0,txt, 100,0,100);
  }
  
  //free 114-131

  hA[114]=h=new TH2F("musptBalance_Eta","sptBalance vs cluster Eta; eta; sptBalance",50,-3,3,100,-100,100);
  hA[115]=h=new TH2F("musptBalance_Phi","sptBalance vs cluster Phi; phi; sptBalance",63,-PI,PI,100,-100,100);

  hA[117]=h=new TH2F("mujetQAeta_phi","Input Jet phi vs eta ;  eta ; phi ",50,-3,3,63,-PI,PI);
  hA[118]=h=new TH1F("mujetQApt","Input Jet pt; pt;",100,0,100);
  
  // add 119-122 for jet counts and correlation, jinlong, 12/19/2014
  hA[119]=h=new TH1F("mujetQAno","Number of Jets per event; ; counts", 10, 0,10);
  hA[120]=h=new TH2F("mujetCorrEta","Eta Correlation: candidates vs jet; candidate #eta ; jet #eta", 50, -2.5,2.5, 50, -2.5,2.5);
  hA[121]=h=new TH2F("mujetCorrPhi","Phi Correlation: candidates vs jet; candidate #phi; jet #phi", 63, -PI,PI, 63, -PI, PI);
  hA[122]=h=new TH1F("mujetQApt_out","Jet pt outside nearCone; jet pt;",100, 0, 100);
  hA[123]=h=new TH2F("mujetQApt_DeltaPhi","Jet pt outside nearCone vs #Delta#phi;jet pt; #Delta#phi ",100, 0, 100, 63, -PI, PI);
  hA[124]=h=new TH2F("mujetQApt_Eta","Jet pt outside nearCone vs #eta;jet pt; #eta ",100, 0, 100, 50, -2.5, 2.5);

  //jet check after all the cuts
  hA[125]=h=new TH1F("muWjetQAno","Number of Jets per event, final selection; ; counts", 10, 0,10);
  hA[126]=h=new TH2F("muWjetCorrEta","Eta Correlation: candidates vs jet, final selection; candidate #eta ; jet #eta", 50, -2.5,2.5, 50, -2.5,2.5);
  hA[127]=h=new TH2F("muWjetCorrPhi","Phi Correlation: candidates vs jet, final selection; candidate #phi; jet #phi", 63, -PI,PI, 63, -PI, PI);
  hA[128]=h=new TH1F("muWjetQApt_out","Jet pt outside nearCone, final selection; jet pt;",100, 0, 100);
  hA[129]=h=new TH2F("muWjetQApt_DeltaPhi","Jet pt outside nearCone vs #Delta#phi, final selection;jet pt; #Delta#phi ",100, 0, 100, 63, -PI, PI);
  hA[130]=h=new TH2F("muWjetQApt_Eta","Jet pt outside nearCone vs #eta, final selection;jet pt; #eta ",100, 0, 100, 50, -2.5, 2.5);
  
  //add to see impact on totnearCone ET fraction from the eemc 
  hA[131]=h=new TH1F("muBjetETR_noEEMC","Barrel: ratio (2x2/nearCone) ET w/o EEMC ; cluster ET/ near cone ET w/o eemc",100,0,1.2);
  Lx=h->GetListOfFunctions();
  ln=new TLine(par_nearTotEtFrac,0,par_nearTotEtFrac,1.e6);  ln->SetLineColor(kRed);  Lx->Add(ln);


  hA[132]=h=new TH2F("muptBalance_clust","ptBalance vs cluster ET; 2x2 Cluster ET; ptBalance",100,0,100,100,0,100);
  hA[133]=h=new TH2F("muptBalance_awayTot","ptBalance vs awayside PT; awayside PT; ptBalance",100,0,100,100,0,100);

  hA[134]=h=new TH2F("musPtBalance_clust","Barrel: sPtBalance vs cluster ET; 2x2 Cluster ET (GeV); signed Pt balance (GeV)",100,0,100,200,-100,100);
  Lx=h->GetListOfFunctions();
  ln=new TLine(0,par_ptBalance,100,par_ptBalance);  ln->SetLineColor(kRed);  Lx->Add(ln);

  //add 138,139, for charge separated 2D sptbalance. Jinlong, 12/09/2014
  hA[138]=h=new TH2F("musPtBalance2_clust_pos","Barrel W+: sPtBalance vs cluster ET; 2x2 Cluster ET (GeV); signed Pt balance (GeV)",100,0,100,200,-100,100);
  Lx=h->GetListOfFunctions();
  ln=new TLine(0,par_ptBalance,100,par_ptBalance);  ln->SetLineColor(kRed);  Lx->Add(ln);
  hA[139]=h=new TH2F("musPtBalance2_clust_neg","Barrel W-: sPtBalance vs cluster ET; 2x2 Cluster ET (GeV); signed Pt balance (GeV)",100,0,100,200,-100,100);
  Lx=h->GetListOfFunctions();
  ln=new TLine(0,par_ptBalance,100,par_ptBalance);  ln->SetLineColor(kRed);  Lx->Add(ln);

  hA[135]=h=new TH2F("musPtBalance_awayTot","Barrel: sPtBalance vs awayside PT; awayside PT; sPtBalance",100,0,100,100,-100,100);
  Lx=h->GetListOfFunctions();
  ln=new TLine(0,par_ptBalance,100,par_ptBalance);  ln->SetLineColor(kRed);  Lx->Add(ln);

  int nETbins = 200;
  hA[136]=h=new TH1F("muclustPtBal",Form("Barrel: PT Balance > %.1f ; 2x2 Cluster ET",par_ptBalance),nETbins,0,100);
  hA[137]=h=new TH1F("muclustPtBal_bckgrd",Form("Barrel: PT Balance < %.1f ; 2x2 Cluster ET",par_ptBalance),nETbins,0,100);
  hA[140]=h=new TH1F("muclustPtBalnoE",Form("Barrel: sPT Balance > %.1f (EEMC not included); 2x2 Cluster ET",par_ptBalance),nETbins,0,100);
  
  // Histograms added for background subtraction and systematic
  char str[200];
  for (int i=0; i<=20; i++) {
    sprintf(str,"neg_failAwaySide_Awayside_pt_bin_%d",i);
    hA[142+i] = new TH2F(str,str,nETbins,0,100,81,0,81);
  }

  for (int i=0; i<=20; i++) {
    sprintf(str,"pos_failAwaySide_Awayside_pt_bin_%d",i);
    hA[163+i] = new TH2F(str,str,nETbins,0,100,81,0,81);
  }

  hA[184+2] = new TH1F("pos_muclustpTbal_wE","Barrel: pos_muclustpTbal_wE",nETbins,0,100);
  hA[184+1] = new TH1F("neg_muclustpTbal_wE","Barrel: neg_muclustpTbal_wE",nETbins,0,100);
  hA[184+4] = new TH1F("pos_muclustpTbal_noE","Barrel: pos_muclustpTbal_noE",nETbins,0,100);
  hA[184+3] = new TH1F("neg_muclustpTbal_noE","Barrel: neg_muclustpTbal_noE",nETbins,0,100);
  hA[184+6] = new TH1F("pos_muclustpTbal_back","Barrel: pos_muclustpTbal_back",nETbins,0,100);
  hA[184+5] = new TH1F("neg_muclustpTbal_back","Barrel: neg_muclustpTbal_back",nETbins,0,100);

  
  hA[191]=h=new TH2F("muclustEt_etaWp","Barrel W+: 2x2 Cluster ET vs. lepton eta, final selection; lepton eta in LAB; lepton 2x2 Cluster ET (GeV)",32,-2.,2.,60,0.,60.);
  hA[192]=h=new TH2F("muclustEt_etaWm","Barrel W-: 2x2 Cluster ET vs. lepton eta, final selection; lepton eta in LAB; lepton 2x2 Cluster ET (GeV)",32,-2.,2.,60,0.,60.);
  
  
  hA[195]=h=new TH1F("muEtaLT0_maxTowADC","max tower ADC for eta < 0",4096,0,4096);
  hA[196]=h=new TH1F("muEtaGT0_maxTowADC","max tower ADC for eta > 0",4096,0,4096);
  hA[197]=h=new TH2F("muTr2D1pt10","lastHit on track (pt > 10); detector eta ; detector phi (rad)",100,-1.1,1.1,240,-PI,PI);
  hA[198]=h=new TH2F("muTrPt_eta","Track PT vs lastHit eta; detector eta ; prim track pt (GeV)",100,-1.1,1.1,100,0,100);
  hA[199]=h=new TH2F("muBdist5","Barrel: 3D Distance(track-cluster) vs. cluster eta; cluster eta; | distance | (cm)",100,-1.,1.,50,0,25);
  hA[200]=h=new TH2F("muWClustET_eta","W: 2x2 cluster ET vs. cluster eta; cluster detector eta; 2x2 cluster ET",100,-1.,1.,60,0,60);
  hA[201]=h=new TH2F("muWTrPT_eta","W: Track PT vs. cluster eta; cluster detector eta; Track PT",100,-1.,1.,100,0,100);
  hA[202]=h=new TH2F("muBTrPT_eta","Background: Track PT vs. cluster eta; cluster detector eta; Track PT",100,-1.,1.,100,0,100);
  hA[203]=h=new TH2F("muWE2P_eta","W: E/P vs. cluster eta; cluster detector eta; E/P",100,-1.,1.,15,0,3);
  hA[204]=h=new TH2F("muBE2P_eta","Background: E/P vs. cluster eta; cluster detector eta; E/P",100,-1.,1.,15,0,3);
  hA[205]=h=new TH2F("muWTr2D1","lastHit on track (W candidate); detector eta ; detector phi (rad)",100,-1.1,1.1,240,-PI,PI);
  hA[206]=h=new TH2F("muWClustET_eta_clgt14","2x2 cluster ET vs. cluster eta (Cluster ET > 14); cluster detector eta; 2x2 cluster ET",100,-1.,1.,60,0,60);
  hA[207]=h=new TH2F("muWClustET_eta_towerIso","2x2 cluster ET vs. cluster eta (2x2/4x4 frac); cluster detector eta; 2x2 cluster ET",100,-1.,1.,60,0,60);
  hA[208]=h=new TH2F("muWClustET_eta_delR","2x2 cluster ET vs. cluster eta (track-clust match); cluster detector eta; 2x2 cluster ET",100,-1.,1.,60,0,60);
  hA[209]=h=new TH2F("muWClustET_eta_nearIso","2x2 cluster ET vs. cluster eta (2x2/near frac); cluster detector eta; 2x2 cluster ET",100,-1.,1.,60,0,60);
    
  //check ETOW near cone sum
  hA[210]=h=new TH2F("muETOWnearET_eta_tr2cl","ETOW near cone ET vs. cluster eta; cluster detector eta; ETOW near ET",100,-1.,1.,20,0,20);
  hA[211]=h=new TH2F("muETOWnearET_phi_etaGT0_tr2cl","ETOW near cone ET vs. cluster phi (cluster eta > 0); cluster detector phi; ETOW near ET",240,-PI,PI,20,0,20);
  hA[212]=h=new TH2F("muETOWnearET_phi_etaLT0_tr2cl","ETOW near cone ET vs. cluster phi (cluster eta < 0); cluster detector phi; ETOW near ET",240,-PI,PI,20,0,20);

  hA[240]=h=new TH2F("muclustEt_etaFinal_noE","Barrel: 2x2 Cluster ET vs. lepton eta, final selection (no EEMC in veto); lepton eta in LAB; lepton 2x2 Cluster ET (GeV)",32,-2.,2.,60,0.,60.);
  hA[241]=h=new TH2F("muclustEt_etaFinal","Barrel: 2x2 Cluster ET vs. lepton eta, final selection; lepton eta in LAB; lepton 2x2 Cluster ET (GeV)",32,-2.,2.,60,0.,60.);

  hA[250]=h=new TH2F("muBclEjetE2D_ET","ratio (2x2/nearCone) ET vs. 2x2 cluster ET; 2x2 cluster ET (GeV); ET(cone-2x2) (GeV)",100,0,100,100,0,1.2);
  Lx=h->GetListOfFunctions();
  ln=new TLine(0,par_nearTotEtFrac,1.e6,par_nearTotEtFrac);  ln->SetLineColor(kRed);  Lx->Add(ln);

  hA[251]=h=new TH2F("muSpTbal_isoConePos","Q+ Signed p_{T} Balance vs. 2x2/nearCone; 2x2/nearCone; Signed p_{T} Balance (GeV)",110,0,1.1,100,-100,100);
  hA[252]=h=new TH2F("muSpTbal_isoConeNeg","Q- Signed p_{T} Balance vs. 2x2/nearCone; 2x2/nearCone; Signed p_{T} Balance (GeV)",110,0,1.1,100,-100,100);

  hA[253]=h=new TH2F("musPtBalance_awayTot_highEt","Barrel (ET > 25): sPtBalance vs awayside PT; awayside PT; sPtBalance",100,0,100,100,-100,100);
  Lx=h->GetListOfFunctions();
  ln=new TLine(0,par_ptBalance,100,par_ptBalance);  ln->SetLineColor(kRed);  Lx->Add(ln);
  
  //Q/pt plots for each sector
  for(int isec=0; isec<24; isec++){
    sprintf(txt,"TPC Sector %d : GLOB Q/PT  ; 2x2 cluster ET (GeV); Q/PT",isec+1);
    hA[260+isec]=h=new TH2F(Form("muChRecPNgSec%d",isec+1), txt,100,0.,100.,100,-0.1,0.1);
    Lx=h->GetListOfFunctions();
    ln=new TLine(0,0,100,0);  ln->SetLineColor(kMagenta);  Lx->Add(ln);

    sprintf(txt,"TPC Sector %d : PRIM  Q/PT ; 2x2 cluster ET (GeV); Q/PT",isec+1);
    hA[284+isec]=h=new TH2F(Form("muChRecPNpSec%d",isec+1), txt,100,0.,100.,100,-0.1,0.1);
    Lx=h->GetListOfFunctions();
    ln=new TLine(0,0,100,0);  ln->SetLineColor(kMagenta);  Lx->Add(ln);

    sprintf(txt,"Barrel W: TPC Sector %d Track prim POSITIVE glob signed DCA, final selection; 2x2 ET (GeV); sDCA (cm)",isec+1);
    hA[308+isec]=h=new TH2F(Form("muWglDcaSP_Sec%d",isec+1), txt,100,0.,100.,100,-5,5);
    sprintf(txt,"Barrel W: TPC Sector %d Track prim NEGATIVE glob signed DCA, final selection; 2x2 ET (GeV); sDCA (cm)",isec+1);
    hA[332+isec]=h=new TH2F(Form("muWglDcaSN_Sec%d",isec+1), txt,100,0.,100.,100,-5,5);

    sprintf(txt,"TPC Sector %d : PRIM Charge Separation Hyperbola Corrected ; 2x2 cluster ET (GeV); Q*ET/PT",isec+1);
    hA[356+isec]=h=new TH2F(Form("muChRecHypCorrPNpSec%d",isec+1), txt,100,0.,100.,100,-4,4);
    Lx=h->GetListOfFunctions();
    ln=new TLine(0,0,100,0);  ln->SetLineColor(kMagenta);  Lx->Add(ln);
  }    

  hA[390]=h=new TH1F("adcB_dist","btow ADC distribution; btow softID; ;",4800,0,4800);
  hA[391]=h=new TH2F("adcB2D_dist","btow ADC 2D distibution; iphi; ieta;",120,0,120,40,0,40);


  // add histos to the list (if provided)
  for(int i=0;i<mxHA;i++) {
    if(  hA[i]==0) continue;
    HList->Add( hA[i]);
  }
  
  //  HList->ls();
  LOG_INFO<<Form("%s::initHistos done",GetName())<<endm;

}

// $Log: St2011W_histo.cxx,v $
// Revision 1.18  2016/01/08 02:08:49  jlzhang
// added couples histograms and fixed a small bug
//
// Revision 1.17  2013/09/13 19:33:13  stevens4
// Updates to code for combined 2011+2012 result presented to spin PWG 9.12.13
//
// Revision 1.16  2013/06/14 21:08:51  jlzhang
// add histo Q/pT vs. nHitsFit and Q/pT vs. nHitsPos
//
// Revision 1.15  2012/10/05 17:53:53  balewski
// added correlation plots for reco Q in Z, W algos
//
// Revision 1.14  2012/09/26 14:20:59  stevens4
// use PtBal cos(phi) for WB and WE algos and use Q*ET/PT for barrel charge sign
//
// Revision 1.13  2012/09/18 21:10:08  stevens4
// Include all rank>0 vertex again (new jet format coming next), and remove rank<0 endcap vertices.
//
// Revision 1.12  2012/09/17 03:29:30  stevens4
// Updates to Endcap algo and Q*ET/PT charge separation
//
// Revision 1.11  2012/08/31 20:10:52  stevens4
// switch to second EEMC background using both isolation and sPt-Bal (for mirror symmetry (also adjust eta binning)
//
// Revision 1.10  2012/08/28 14:28:27  stevens4
// add histos for barrel and endcap algos
//
// Revision 1.9  2012/08/07 21:06:38  stevens4
// update to tree analysis to produce independent histos in a TDirectory for each eta-bin
//
// Revision 1.8  2012/07/13 20:53:16  stevens4
// Add filling of empty events in W tree
// Minor modifications to histograms
//
// Revision 1.7  2012/07/13 16:11:44  balewski
// minor clenup, prevent crash in Finish if zero input events, now it runs on M-C events as well
//
// Revision 1.6  2012/07/05 19:03:54  stevens4
// preset bin labels for TPC histos to preven merge warning
//
// Revision 1.5  2012/06/29 21:20:08  stevens4
// *** empty log message ***
//
// Revision 1.4  2012/06/25 20:53:26  stevens4
// algo and histo cleanup
//
// Revision 1.3  2012/06/18 18:28:01  stevens4
// Updates for Run 9+11+12 AL analysis
//
// Revision 1.2  2011/02/25 06:03:50  stevens4
// addes some histos and enabled running on MC
//
// Revision 1.1  2011/02/10 20:33:23  balewski
// start
//
