
// $Id: St2011W_Ehisto.cxx,v 1.15 2013/09/13 19:33:13 stevens4 Exp $
//
//*-- Author :  Endcap: Justin Stevens, IUCF

#include <TH2.h>
#include <TList.h>
#include <TLine.h>
#include <TMath.h>

#include "St2011WMaker.h"

//________________________________________________
//________________________________________________
void
St2011WMaker::initEHistos(){
  const float PI=TMath::Pi();

  //...... data histograms
  memset(hE,0,sizeof(hE));
  TList *Lx;  TLine *ln;TH1 *h;
  char txt[1000], txt0[100];
  int nCase=20;
  hE[0]=h=new TH1F("muEStatEve","Endcap W-algo: event count, Jinlong-version",nCase,0,nCase);
  h->GetXaxis()->SetTitleOffset(0.4);  h->GetXaxis()->SetLabelSize(0.06);  h->GetXaxis()->SetTitleSize(0.05); h->SetMinimum(0.8);
  h->SetLineColor(kBlue);h->SetLineWidth(2);
  h->SetMarkerSize(2);//<-- large text

  char key[][200]={"inp","L2ewId","L2ewBits","L2ewET","L2ewRnd","tpcOn","primVert","vertZ","Pt10",
                   "E-in","E200","TrE","Tr2Cl","noNear","smdRatio","noAway","goldW","goldW+","goldW-"};
  for(int i=0;i<19;i++) h->Fill(key[i],0.); // preset the order of keys

  hE[1]=h=new TH1F("muEInTrg","mu Endcap W input triggers, WARN: scrambled if manyruns are combined by hadd.C; trigID (random order)",nCase,0,nCase);
  h->GetXaxis()->SetLabelSize(0.06);

  hE[2]=h=new TH1F("muEbX48","L2WE-ET events vs. bXing; bXing= raw bx48",128,-0.5,127.5);
  h->SetFillColor(kGreen);

  hE[3]=h=new TH1F("muEbX7","L2WE-ET events vs. bXing; bXing= raw bx7",128,-0.5,127.5);
  h->SetFillColor(kBlue);

  hE[4]=new TH1F("muEbX48v","L2WE-ET & primVertex  vs. bXing; bXing= raw bx48",128,-0.5,127.5);
  hE[5]=h=new TH1F("muEbX7v","L2WE-ET & primVertex; bXing= raw bx7",128,-0.5,127.5);
  h->SetFillColor(kBlue);

  //... DMS data
  hE[6]=h=new TH1F("muEDsm1","L2WE-ET events DMS spectrum; DSM value",64,-0.5,63.5);
  h->SetMinimum(0.8);
  hE[7]=h=new TH1F("muEDsm2","L2WE-Rnd events DMS spectrum; DSM value",64,-0.5,63.5);
  h->SetMinimum(0.8);
  sprintf(txt,"L2WE-ET events w/ DMS>%d vs.ETOW TP ID bXing; Hanks' TP ID",parE_DsmThres);
  hE[8]=new TH1F("muEDsm3",txt,90,-0.5,89.5);
  sprintf(txt,"L2WE-ET events w/ DMS>%d & primVertexvs.ETOW TP ID bXing; Hanks' TP ID",parE_DsmThres);
  hE[9]=h=new TH1F("muEDsm4",txt,90,-0.5,89.5);
  h->SetFillColor(kBlue); h->SetLineColor(kBlue);


  //.... vertex histograms .....
  hE[10]=h=new TH1F("muEVRf","L2WE: PPV Vertex rank, funny X-axis; X=Log(rank)+offset", 150, -9,25);
  Lx=h->GetListOfFunctions();
  ln=new TLine(0,0,0,1.e6);  ln->SetLineColor(kRed);  Lx->Add(ln);


  hE[11]=h=new TH1F("muEZv","L2WE: Z of any vertex w/ rank>0;Z-vertex (cm)",100,-200,200);
  Lx=h->GetListOfFunctions();
  ln=new TLine(par_vertexZ,0,par_vertexZ,1.e6);  ln->SetLineColor(kRed);  Lx->Add(ln);
  ln=new TLine(-par_vertexZ,0,-par_vertexZ,1.e6);  ln->SetLineColor(kRed);  Lx->Add(ln);

  hE[12]=new TH1F("muENV","L2WE: # vertices per event, (rank>0 or matched track to ETOW) && Z in range; # of vertices",10,0,10);

  //..... Tracks....
  hE[20]=h=new TH1F("muEStatTrk","Endcap W-algo: track  count; cases",nCase,0,nCase);
  h->GetXaxis()->SetTitleOffset(0.4);  h->GetXaxis()->SetLabelSize(0.06);  h->GetXaxis()->SetTitleSize(0.05); h->SetMinimum(0.8);
  h->SetLineColor(kGreen); h->SetLineWidth(2);
  char keyT[][200]={"flag","pt1","#eta>0.7","nHit","Hfrac","Rin","Rout","ptOK","@E","CL","fr24",
                    "#Delta R","noNear","smdRatio","noAway","goldW"};

  for(int i=0;i<16;i++) h->Fill(keyT[i],0.); // preset the order of keys


  hE[21]=h=new TH1F("muETrNfit","Endcap: primary track  in-selection & vertexZ; nFitPoints",50,0,50);
  Lx=h->GetListOfFunctions();
  ln=new TLine(parE_nFitPts,0,parE_nFitPts,1.e6);  ln->SetLineColor(kRed);  Lx->Add(ln);

  hE[22]=h=new TH1F("muETrFitFrac","Endcap: primary track in-selection & vertexZ; nFit/nPoss ",50,0,1.1);
  Lx=h->GetListOfFunctions();
  Lx=h->GetListOfFunctions();
  ln=new TLine(parE_nHitFrac,0,parE_nHitFrac,1.e6);  ln->SetLineColor(kRed);  Lx->Add(ln);

  hE[23]=h=new TH1F("muETrRxyIn","Endcap: primary track first hit  in-selection & vertexZ; Rxy (cm)",60,50,170.);
  Lx=h->GetListOfFunctions();
  ln=new TLine(parE_trackRin,0,parE_trackRin,1.e6);  ln->SetLineColor(kRed);  Lx->Add(ln);
  
  hE[24]=h=new TH1F("muETrRxyOut","Endcap: primary track last hit  in-selection & vertexZ; Rxy (cm)",80,60,220.);
  Lx=h->GetListOfFunctions();
  ln=new TLine(parE_trackRout,0,parE_trackRout,1.e6);  ln->SetLineColor(kRed);  Lx->Add(ln);

  hE[25]=h=new TH1F("muETrPt1","Endcap: global track PT ; track PT (GeV/c)",80,0,80);
  Lx=h->GetListOfFunctions();
  ln=new TLine(parE_trackPt,0,parE_trackPt,1.e6);  ln->SetLineColor(kRed);  Lx->Add(ln); h->SetFillColor(kYellow);
  

  hE[26]=h=new TH2F("muETr2D1","Endcap: lastHit on track; detector eta ; detector phi (rad)",100,-0.2,1.8,200,-3.2,3.2);

  hE[27]=h=new TH1F("muETrPt1N","Endcap: global NEGATIVE track PT; track PT (GeV/c)",80,0,80);
  h->SetFillColor(7);
  
  hE[28]=h=new TH2F("muETrdEdX","Endcap: dEdX vs. momentum; track P (GeV); dE/dx (keV)",20,0,10,100,0,10);
  

  hE[29]=h=new TH1F("muETrPt1Pr","Endcap: primary track PT; track PT (GeV/c)",80,0,80);
  hE[30]=h=new TH1F("muETrPt1NPr","Endcap: primary NEGATIVE track PT; track PT (GeV/c)",80,0,80); 


  //..... ETOW .....

  hE[31]=h=new TH1F("muEmaxAdc","Endcap: ETOW maxADC in event, in-selection; max tower ADC",200,0,4200);
  hE[32]=h=new TH1F("muEtotAdc","Endcap: ETOW sum of ADC>thres , in-selection;ADC sum/event", 80,0,8000.);
  
  hE[33]=h=new TH1F("muEclET","matched ETOW 2x2 cluster ET  ;cluster  ET (GeV)",100,0,100);
  Lx=h->GetListOfFunctions();
  ln=new TLine(parE_clustET,0,parE_clustET,1.e6);  ln->SetLineColor(kRed);  Lx->Add(ln);

  hE[34]=h=new TH2F("muEclAdcPt","Endcap: matched  TPC PT vs. ETOW 2x2 cluster ADC sum ; cluster (ADC sum);TPC  PT (GeV)",50,0,5000,75,0,150);
  h->GetXaxis()->SetNdivisions(4);

  hE[35]=new TH1F("muETrch2","Endcap: track glob chi2/dof X-Y",100,0,5);
  hE[36]=new TH2F("muETrch2b","Endcap: track glob chi2/dof; chi2/dof  X-Y; last hit eta",30,0,5.,30,-0.2,1.8);


  //.... 4x4 cluster
  hE[37]=h=new TH1F("muEclET24","matched ETOW 4x4 cluster ET ;cluster 4x4  ET (GeV)",100,0,100);
  hE[38]=h=new TH2F("muEclE242D","Endcap: Excess energy in ETOW 4x4 cluster vs. 2x2 cluster E;2x2 cluster E (GeV); E(4x4)-E(2x2)  E (GeV)",60,0,120,30,0,30);

   hE[39]=h=new TH1F("muEclET24R","Endcap: ratio (2x2/4x4) cluster ET ; fraction: cluster ET 2x2/ 4x4 ET",100,0,1.2);
   Lx=h->GetListOfFunctions();
  ln=new TLine(parE_clustFrac24,0,parE_clustFrac24,1.e6);  ln->SetLineColor(kRed);  Lx->Add(ln);

   //..... jet energy , fraction

   hE[40]=h=new TH1F("muEEMCjetET","Endcap: near 'EM jet' ET ; 'EM jet'  ET (GeV)",100,0,100);

   hE[41]=h=new TH2F("muEclEMCjetE2D","Endcap: Excess nearCone ET  vs. 2x2 E;2x2 cluster ET (GeV); ET(cone-2x2) (GeV)",50,0,80,50,0,60);

   hE[42]=h=new TH1F("muETEMCjetETR","Endcap: ratio (2x2/nearCone) ET ; cluster ET/ near cone ET",100,0,1.2);
  Lx=h->GetListOfFunctions();
  ln=new TLine(parE_nearTotEtFrac,0,parE_nearTotEtFrac,1.e6);  ln->SetLineColor(kRed);  Lx->Add(ln);

  // .... track-EMC distance cuts
  hE[43]=h=new TH2F("muEdist1","Endcap: X-Y Distance(track-ETOW cluster) vs. 2x2 E;2x2 cluster E (GeV); | distance | (cm)",40,0,120,40,0,25);
  hE[44]=h=new TH2F("muEdist2","Endcap: R#Delta #phi   (track-ETOW cluster) vs.#phi-clust;  .#phi-clust(rad) ;R#Delta #phi (cm)",100,-3.2,3.2,40,-20,20);
  hE[45]=h=new TH2F("muEdist3","Endcap: R#Delta #phi   (track-ETOW cluster) vs. 2x2 E;2x2 cluster E (GeV); R#Delta #phi (cm)",40,0,120,40,-20,20);
  hE[46]=h=new TH1F("muEdist4","Endcap: X-Y Distance(track-ETOW cluster) for 2x2 E;| X-Y distance |   (cm)",100,0,50);
  Lx=h->GetListOfFunctions();
  ln=new TLine(parE_delR3D,0,parE_delR3D,1.e6);  ln->SetLineColor(kRed);  Lx->Add(ln);
  
  //.. continue same side jet veto
  hE[47]=h=new TH1F("muETjetET","Endcap: quenched near 'TPC jet' PT ; 'TPC jet'  PT (GeV)",100,0,100);
  hE[48]=h=new TH2F("muETjetEMCjet2D","Endcap: quenched  near 'TPC jet' vs.  near 'EMC jet' ; EMC jet ET (GeV); TPC PT  (GeV/c)",50,0,80,50,0,60);
  hE[49]=h=new TH1F("muETEMCjetET","Endcap: near 'TPC+EMC jet' ET ; jet ET (GeV), no double counting",100,0,100);
  
  //.. away-side jet  veto
  hE[50]=h=new TH1F("muETwayET","Endcap: TPC away-cone PT sum;   PT (GeV)",100,0,100);
  hE[51]=h=new TH1F("muEBwayET","Endcap: BTOW away-cone ET sum;   ET (GeV)",100,0,100);
  hE[52]=h=new TH2F("muETotwayET2D","Endcap: away TPC+EMC ET sum  vs. 2x2 ETOW cluster ET;  2x2 ET (GeV); away ET (GeV)",50,0,100,150,0,100);
  hE[53]=h=new TH2F("muEAwayET2Db","Endcap: away EMC ET sum  vs. 2x2 ETOW cluster ET;  2x2 ET (GeV); away EMC ET (GeV)",50,0,100,80,0,80);
  hE[54]=h=new TH1F("muEAwayTotEt","Endcap: away-cone TPC+EMC ET sum ; away ET (GeV)",200,0,100);

  hE[55]=h=new TH1F("muEEwayET","Endcap: ETOW away-cone ET sum;   ET (GeV)",100,0,100); // away side energy  

  hE[57]=h=new TH2F("muETr2D1pt5","Endcap: lastHit on track (pt > 5); detector eta ; detector phi (rad)",100,-0.2,1.8,240,-PI,PI);

  //... final golden plots ....

  hE[60]=h=new TH2F("muEEclETPt","Endcap: Awayside TPC PT vs.  isolated ETOW 2x2 cluster ET, matched;2x2 cluster ET (GeV) ; Awayside TPC PT (GeV)",50,0,100,75,0,150);
  
  hE[61]=new TH1F("muEbX7eht","L2W-EHT-rnd  events vs. bXing; bXing= raw bx7",128,-0.5,127.5); // filled on input
  hE[62]=h=new TH2F("muEEne_Deta","Endcap W: ETOW 2x2 cluster energy vs. detector eta, final selection; endcap eta bin; 2x2 Energy (GeV)",12,0,12,50,0,100);

  // track matched to cluster plots
  hE[63]=h=new TH1F("muETrNfitTr2Cl","Endcap: primary track in-muTr2Cl; nFitPoints",50,0,50);
  Lx=h->GetListOfFunctions();
  ln=new TLine(parE_nFitPts,0,parE_nFitPts,1.e6);  ln->SetLineColor(kRed);  Lx->Add(ln);

  hE[64]=h=new TH1F("muETrFitFracTr2Cl","Endcap: primary track in-muTr2Cl; nFit/nPoss ",50,0,1.1);
  Lx=h->GetListOfFunctions();
  ln=new TLine(parE_nHitFrac,0,parE_nHitFrac,1.e6);  ln->SetLineColor(kRed);  Lx->Add(ln);
  
  hE[65]=h=new TH1F("muETrRxyInTr2Cl","Endcap: primary track first hit  in-muTr2Cl; Rxy (cm)",60,50,170.);
  Lx=h->GetListOfFunctions();
  ln=new TLine(parE_trackRin,0,parE_trackRin,1.e6);  ln->SetLineColor(kRed);  Lx->Add(ln);

  hE[66]=h=new TH1F("muETrRxyOutTr2Cl","Endcap: primary track last hit  in-muTr2Cl; Rxy (cm)",80,60,220.);
  Lx=h->GetListOfFunctions();
  ln=new TLine(parE_trackRout,0,parE_trackRout,1.e6);  ln->SetLineColor(kRed);  Lx->Add(ln);

  // misc...
  hE[69]=h=new TH2F("muEeXY","Endcap W: Projected track XY at SMD depth;  X (cm); Y (cm)",100,-280,280,100,-280,280);
  hE[70]=h=new TH1F("muETEMCjetETR_EMC","Endcap: ratio (2x2/nearEmcCone) ET ; cluster ET/ nearEmcCone ET",100,0,1.2);
  Lx=h->GetListOfFunctions();
  ln=new TLine(parE_nearTotEtFrac,0,parE_nearTotEtFrac,1.e6);  ln->SetLineColor(kRed);  Lx->Add(ln);
  hE[71]=h=new TH1F("muETEMCjetETR_ETOW","Endcap: ratio (2x2/nearEtowCone) ET ; cluster ET/ nearEtowCone ET",100,0,1.2);
  Lx=h->GetListOfFunctions();
  ln=new TLine(parE_nearTotEtFrac,0,parE_nearTotEtFrac,1.e6);  ln->SetLineColor(kRed);  Lx->Add(ln);

  //.... final Ws
  hE[90]=h=new TH1F("muE_WET","Endcap W: Final selection; 2x2 ETOW cluster ET (GeV)", 100,0,100);
  Lx=h->GetListOfFunctions();
  ln=new TLine(parE_highET,0,parE_highET,1.e6);  ln->SetLineColor(kRed);  Lx->Add(ln);

  sprintf(txt,"Endcap W: Final selection, ET>%.0f GeV 'goldenW'; detector eta ; detector phi (rad)",parE_highET);
  hE[91]=new TH2F("muE_W2D1",txt,15,0.5,2.0,12,-PI,PI);

  hE[92]=new TH2F("muE_Wdedx","Endcap W: Track dEdx, final W selection; 2x2 ET (GeV); dEdx (keV)",100,0,100,100,0,20);
  hE[93]=new TH2F("muE_WglDca","Endcap W: Track glob vertex abs(DCA), final selection ; 2x2 ET (GeV); |DCA| (cm)",100,0,100,100,0,5);
  hE[94]=new TH2F("muE_WglDcaSP","Endcap W: Track prim POSITIVE glob signed DCA, final selection; 2x2 ET (GeV); sDCA (cm)",100,0,100,100,-5,5);
  hE[95]=new TH2F("muE_WglDcaSN","Endcap W: Track prim NEGATIVE glob signed DCA, final selection ; 2x2 ET (GeV); sDCA (cm)",100,0,100,100,-5,5);

  sprintf(txt,"Endcap W: Vertex ID, final selection, 2x2 ET>%.0f GeV; vertex ID",parE_highET);
  hE[96]=h=new TH1F("muE_Wcar1",txt,10,-0.5,9.5);
  h->SetLineColor(kBlue);h->SetLineWidth(3);

  sprintf(txt,"Endcap W: Vertex 'funny' rank, final selection, 2x2 ET>%.0f GeV;  X=Log(rank)+offset",parE_highET);
  hE[97]=new TH1F("muE_Wcar2",txt, 150, -9,25);

  sprintf(txt,"Vertex Z ,Endcap W: Vertex Z, final selection 2x2 ET>%.0f GeV; Z(cm)",parE_highET);
  hE[98]=new TH1F("muE_Wcar3",txt, 100, -200,200);
  hE[99]=h=new TH1F("muE_Weta","Endcap W: Lepton eta, final selection ; lepton eta",400,-2.0,2.0);
  hE[100]=h=new TH2F("muE_WXY","Endcap W: Projected track XY at SMD depth, final selection; X (cm); Y (cm)",100,-280,280,100,-280,280);

  hE[101]=h=new TH2F("muE_W_sPtBalsmallIso","Endcap W: sPt-Bal vs 2x2/4x4 ratio, final selection ; 2x2/4x4 ratio; signed Pt-Balance",100,0,1.2,100,-100,100);
  hE[102]=h=new TH2F("muE_W_sPtBalnearIso","Endcap W: sPt-Bal vs 2x2/nearCone ratio, final selection ; 2x2/nearCone ratio; signed Pt-Balance",100,0,1.2,100,-100,100);
  hE[103]=h=new TH2F("muE_W_sPtBalnearIsoEmc","Endcap W: sPt-Bal vs 2x2/nearEmcCone ratio, final selection ; 2x2/nearEmcCone ratio; signed Pt-Balance",100,0,1.2,100,-100,100);
  hE[104]=h=new TH2F("muE_W_sPtBalnearIsoEtow","Endcap W: sPt-Bal vs 2x2/nearEtowCone ratio, final selection ; 2x2/nearEtowCone ratio; signed Pt-Balance",100,0,1.2,100,-100,100);

  hE[105]=h=new TH2F("muE_W_sPtBaldedx","Endcap W: sPt-Bal vs dE/dx, final selection ; dE/dx (keV); signed Pt-Balance",100,0,20,100,-100,100);

  // plots to study charge separation
  hE[106]=h=new TH2F("muE_W_primChi2_hypCorrP","Endcap W: Q*ET/PT vs primary #Chi^{2}, final selection ; primary #Chi^{2}; Q*ET/PT",100,0,5,160,-4,4);
  hE[107]=h=new TH2F("muE_W_globChi2_hypCorrP","Endcap W: Q*ET/PT vs global #Chi^{2}, final selection ; global #Chi^{2}; Q*ET/PT",100,0,5,160,-4,4);
  hE[108]=h=new TH2F("muE_W_FitFrac_hypCorrP","Endcap W: Q*ET/PT vs FitFrac, final selection ; nFit/nPoss; Q*ET/PT",100,0,1.1,160,-4,4);
  hE[109]=h=new TH2F("muE_W_nHit_hypCorrP","Endcap W: Q*ET/PT vs nHit, final selection ; nHit; Q*ET/PT",50,0,50,160,-4,4);
  
  hE[120]=h=new TH2F("muE_W_diffChi2_hypCorrP","Endcap W: Q*ET/PT vs primary - global #Chi^{2}, final selection ; primary - global #Chi^{2}; Q*ET/PT",100,-5,5,160,-4,4);
  hE[121]=h=new TH2F("muE_W_DCA_hypCorrP","Endcap W: Q*ET/PT vs sDCA, final selection ; sDCA; Q*ET/PT",100,-5,5,160,-4,4);
  hE[122]=h=new TH2F("muE_W_ESMDdeltaU_hypCorrP","Endcap W: Q*ET/PT vs ESMDU #Delta strip, final selection; ESMDU #Delta strip; Q*ET/PT",11,-5.5,5.5,160,-4,4);
  hE[123]=h=new TH2F("muE_W_ESMDdeltaV_hypCorrP","Endcap W: Q*ET/PT vs ESMDV #Delta strip, final selection; ESMDV #Delta strip; Q*ET/PT",11,-5.5,5.5,160,-4,4);
  hE[124]=h=new TH2F("muE_W_ESMDdeltaV_deltaU","Endcap W: ESMD #Delta strip U vs V, final selection; ESMDV #Delta strip; ESMDU #Delta strip",11,-5.5,5.5,11,-5.5,5.5);
  hE[125]=h=new TH2F("muE_W_Rin_hypCorrP","Endcap W: Q*ET/PT vs Rin, final selection ; Rin; Q*ET/PT",60,50,170.,160,-4,4);
  hE[126]=h=new TH2F("muE_W_Rout_hypCorrP","Endcap W: Q*ET/PT vs Rout, final selection ; Rout; Q*ET/PT",80,60,220.,160,-4,4);
  hE[127]=h=new TH2F("muE_W_dEdx_hypCorrP","Endcap W: Q*ET/PT vs dEdx, final selection ; dEdx; Q*ET/PT",100,0.,20.,160,-4,4);
  hE[128]=h=new TH2F("muE_W_gLength_hypCorrP","Endcap W: Global track length vs zdcx rate; Global track length; Q*ET/PT",100,0.,200.,160,-4,4);
  hE[129]=h=new TH2F("muE_W_deltaR_hypCorrP","Endcap W: Global track radial length vs zdcx rate; Global track radial length; Q*ET/PT",100,0.,100.,160,-4,4);
  hE[130]=h=new TH2F("muE_W_ESMDdeltaPhi_hypCorrP","Endcap W: Q*ET/PT vs ESMD #Delta#phi (centroid - track), final selection; ESMD #Delta#phi; Q*ET/PT",100,-0.5,0.5,160,-4,4);
  hE[131]=h=new TH2F("muE_W_trackDeltaPhi_hypCorrP","Endcap W: Q*ET/PT vs Track #Delta#phi (primary - global), final selection; Track #Delta#phi; Q*ET/PT",100,-0.5,0.5,160,-4,4);

  // extra correlations for "or" type cuts
  hE[150]=h=new TH2F("muE_WP_Rout_sDCA","Endcap Reco W+: Rout vs sDCA ; sDCA; Rout (cm)",160,-4,4,100,0.,200.);
  hE[151]=h=new TH2F("muE_WN_Rout_sDCA","Endcap Reco W-: Rout vs sDCA ; sDCA; Rout (cm)",160,-4,4,100,0.,200.);
  
  hE[152]=h=new TH2F("muE_WP_Rout_ESMDdeltaPhi","Endcap Reco W+: Rout vs ESMD #Delta#phi (centroid - track); ESMD #Delta#phi; Rout (cm)",100,-0.5,0.5,100,0,200);
  hE[153]=h=new TH2F("muE_WN_Rout_ESMDdeltaPhi","Endcap Reco W-: Rout vs ESMD #Delta#phi (centroid - track); ESMD #Delta#phi; Rout (cm)",100,-0.5,0.5,100,0,200);
  hE[154]=h=new TH2F("muE_WP_deltaR_ESMDdeltaPhi","Endcap Reco W+: global track radial length vs ESMD #Delta#phi (centroid - track); ESMD #Delta#phi ; global track radial length (cm)",100,-0.5,0.5,100,0.,100.);
  hE[155]=h=new TH2F("muE_WN_deltaR_ESMDdeltaPhi","Endcap Reco W-: global track radial length vs ESMD #Delta#phi (centroid - track); ESMD #Delta#phi ; global track radial length (cm)",100,-0.5,0.5,100,0.,100.);
  hE[156]=h=new TH2F("muE_WP_deltaR_trackDeltaPhi","Endcap Reco W+: global track radial length vs Track #Delta#phi (primary - global); Track #Delta#phi ; global track radial length (cm)",100,-0.5,0.5,100,0.,100.);
  hE[157]=h=new TH2F("muE_WN_deltaR_trackDeltaPhi","Endcap Reco W-: global track radial length vs Track #Delta#phi (primary - global); Track #Delta#phi ; global track radial length (cm)",100,-0.5,0.5,100,0.,100.);
  hE[158]=h=new TH2F("muE_WP_primChi2_trackDeltaPhi","Endcap Reco W+: primary #Chi^{2} vs Track #Delta#phi (primary - global); Track #Delta#phi ; primary #Chi^{2}",100,-0.5,0.5,100,0.,5.);
  hE[159]=h=new TH2F("muE_WN_primChi2_trackDeltaPhi","Endcap Reco W-: primary #Chi^{2} vs Track #Delta#phi (primary - global); Track #Delta#phi ; primary #Chi^{2}",100,-0.5,0.5,100,0.,5.);
  hE[160]=h=new TH2F("muE_WP_primChi2_ESMDdeltaPhi","Endcap Reco W+: primary #Chi^{2} vs ESMD #Delta#phi (centroid - track); ESMD #Delta#phi ; primary #Chi^{2}",100,-0.5,0.5,100,0.,5.);
  hE[161]=h=new TH2F("muE_WN_primChi2_ESMDdeltaPhi","Endcap Reco W-: primary #Chi^{2} vs ESMD #Delta#phi (centroid - track); ESMD #Delta#phi ; primary #Chi^{2}",100,-0.5,0.5,100,0.,5.);
  hE[162]=h=new TH2F("muE_WP_primChi2_trackDeltaR","Endcap Reco W+: primary #Chi^{2} vs global track radial length; global track radial length; primary #Chi^{2}",100,0.,100.,100,0.,5.);
  hE[163]=h=new TH2F("muE_WN_primChi2_trackDeltaR","Endcap Reco W-: primary #Chi^{2} vs global track radial length; global track radial length; primary #Chi^{2}",100,0.,100.,100,0.,5.);
  
  //158-169

  hE[170]=h=new TH2F("muE_W_cutA_nHit_hypCorrP","Endcap W: Q*ET/PT vs nHit, cut A selection; nHit; Q*ET/PT",35,0,35,160,-4,4);
  hE[171]=h=new TH2F("muE_W_cutB_nHit_hypCorrP","Endcap W: Q*ET/PT vs nHit, cut B selection; nHit; Q*ET/PT",35,0,35,160,-4,4);
  hE[172]=h=new TH2F("muE_W_cutC_nHit_hypCorrP","Endcap W: Q*ET/PT vs nHit, cut C selection; nHit; Q*ET/PT",35,0,35,160,-4,4);
  hE[173]=h=new TH2F("muE_W_cutD_nHit_hypCorrP","Endcap W: Q*ET/PT vs nHit, cut D selection; nHit; Q*ET/PT",35,0,35,160,-4,4);
  hE[174]=h=new TH2F("muE_W_cutE_nHit_hypCorrP","Endcap W: Q*ET/PT vs nHit, cut E selection; nHit; Q*ET/PT",35,0,35,160,-4,4);
  hE[175]=h=new TH2F("muE_W_cutF_nHit_hypCorrP","Endcap W: Q*ET/PT vs nHit, cut F selection; nHit; Q*ET/PT",35,0,35,160,-4,4);
  hE[176]=h=new TH2F("muE_W_cutG_nHit_hypCorrP","Endcap W: Q*ET/PT vs nHit, cut G selection; nHit; Q*ET/PT",35,0,35,160,-4,4);

  // free 101-109
  //..... series of electron ET plots after succesive cuts
  char tt2[][200]={"max 2x2","track matched","no near ET","smdRatio","no away ET"};
  for(int i=0;i<5;i++){
    sprintf(txt,"Endcap electron candidate, cut=%s; 2x2 ET (GeV)",tt2[i]);
    sprintf(txt0,"muE_ETlive%d",i);
    hE[110+i]=h=new TH1F(txt0,txt, 100,0,100);
  }
  
  hE[132]=h=new TH2F("muEptBalance_clust","Endcap: ptBalance vs cluster ET; 2x2 Cluster ET; ptBalance",100,0,100,100,0,100);
  hE[133]=h=new TH2F("muEptBalance_awayTot","ptBalance vs awayside PT; awayside PT; ptBalance",100,0,100,100,0,100);

  hE[134]=h=new TH2F("muEsPtBalance_clust","Endcap: sPtBalance vs cluster ET; 2x2 Cluster ET (GeV); signed Pt balance (GeV)",100,0,100,100,-100,100);
  Lx=h->GetListOfFunctions();
  ln=new TLine(0,parE_ptBalance,100,parE_ptBalance);  ln->SetLineColor(kRed);  Lx->Add(ln);

  hE[135]=h=new TH2F("muEsPtBalance_awayTot","Endcap: sPtBalance vs awayside PT; awayside PT; sPtBalance",100,0,100,100,-100,100);
  Lx=h->GetListOfFunctions();
  ln=new TLine(0,parE_ptBalance,100,parE_ptBalance);  ln->SetLineColor(kRed);  Lx->Add(ln);


  hE[136]=h=new TH1F("muEclustPtBal",Form("Endcap: PT Balance > %.1f ; 2x2 Cluster ET",parE_ptBalance),100,0,100);
  hE[137]=h=new TH1F("muEclustPtBal_bckgrd",Form("Endcap: PT Balance < %.1f ; 2x2 Cluster ET",parE_ptBalance),100,0,100);
  hE[140]=h=new TH1F("muEclustPtBalnoE",Form("Endcap: sPT Balance > %.1f (EEMC not included); 2x2 Cluster ET",parE_ptBalance),100,0,100);
  
  hE[141]=h=new TH2F("muEsPtBalanceDiff_sPtBalanceDiff","Endcap:(sPtBalance - sPtBalance2) vs sPtBalance; sPtBalance; sPtBalance - sPtBalance2",100,-100,100,100,-50,50);

  hE[184+2] = new TH1F("Epos_muEclustpTbal_wE","Endcap: pos_muEclustpTbal_wE",100,0,100);
  hE[184+1] = new TH1F("Eneg_muEclustpTbal_wE","Endcap: neg_muEclustpTbal_wE",100,0,100);
  hE[184+4] = new TH1F("Epos_muEclustpTbal_noE","Endcap: pos_muEclustpTbal_noE",100,0,100);  
  hE[184+3] = new TH1F("neg_muEclustpTbal_noE","Endcap: neg_muEclustpTbal_noE",100,0,100);
  hE[184+6] = new TH1F("Epos_muEclustpTbal_back","Endcap: pos_muEclustpTbal_back",100,0,100);
  hE[184+5] = new TH1F("Eneg_muEclustpTbal_back","Endcap: neg_muEclustpTbal_back",100,0,100);

  hE[190]=h=new TH2F("muEclustEt_etaWp","Endcap Wp: 2x2 Cluster ET vs. lepton eta, final selection; lepton eta in LAB; lepton 2x2 Cluster ET (GeV)",32,-2.,2.,60,0.,60.);
  hE[191]=h=new TH2F("muEclustEt_etaWm","Endcap Wm: 2x2 Cluster ET vs. lepton eta, final selection; lepton eta in LAB; lepton 2x2 Cluster ET (GeV)",32,-2.,2.,60,0.,60.);

  sprintf(txt,"Endcap: TPC GLOB Q/PT  ; ETOW 2x2 cluster ET (GeV); Q/PT");
  hE[200]=h=new TH2F("muEchRecPNg", txt,100,0.,100.,150,-0.15,0.15);
  Lx=h->GetListOfFunctions();
  ln=new TLine(0,0,100,0);  ln->SetLineColor(kMagenta);  Lx->Add(ln);

  sprintf(txt,"Endcap: TPC PRIM  Q/PT ; ETOW 2x2 cluster ET (GeV); Q/PT");
  hE[201]=h=new TH2F("muEchRecPNp", txt,100,0.,100.,150,-0.15,0.15);
  Lx=h->GetListOfFunctions();
  ln=new TLine(0,0,100,0);  ln->SetLineColor(kMagenta);  Lx->Add(ln);

  // "straightened" charge separation plots
  sprintf(txt,"Endcap: TPC GLOB Charge Separation Hyperbola Corrected ; 2x2 cluster ET (GeV); Q*ET/PT");
  hE[202]=h=new TH2F("muEchRecHypCorrPNg", txt,100,0.,100.,160,-4,4);
  Lx=h->GetListOfFunctions();
  ln=new TLine(0,0,100,0);  ln->SetLineColor(kMagenta);  Lx->Add(ln);

  sprintf(txt,"Endcap: TPC PRIM Charge Separation Hyperbola Corrected ; 2x2 cluster ET (GeV); Q*ET/PT");
  hE[203]=h=new TH2F("muEchRecHypCorrPNp", txt,100,0.,100.,160,-4,4);
  Lx=h->GetListOfFunctions();
  ln=new TLine(0,0,100,0);  ln->SetLineColor(kMagenta);  Lx->Add(ln);

  hE[211]=h=new TH2F("muEsmdNhit","Number of hit strips; U plane; V plane",50,0.,50.,50,0.,50.);
  hE[212]=h=new TH2F("muEsmdEne","Energy in SMD Planes; U plane; V plane",100,0.,500.,100,0.,500.);
  hE[213]=h=new TH2F("muEsmdNhit_Ene","Number of hit strips vs Energy in SMD Planes; Nhit (U+V); Energy (U+V)",100,0.,100.,500,0.,1000.);
  hE[214]=h=new TH2F("muEclustET_esmdEne","Tower cluster E_{T} vs. Energy in SMD Planes; Tower cluster E_{T}; SMD Energy (U+V)",100,0.,100.,500,0.,1000.);
  hE[215]=h=new TH2F("muEclustET_esmdNhit","Tower cluster E_{T} vs. Number of hit strips; Tower cluster E_{T}; Nhit (U+V)",100,0.,100.,100,0.,100.);
  hE[216]=0;
  hE[217]=h=new TH2F("muEsmdCrossXY","Difference in SMD XP (track - SMD); X position; Y postion",50,-2.5,2.5,50,-2.5,2.5);
  hE[218]=h=new TH2F("muEsmdCrossEtaPhi","Difference in SMD XP (track - SMD); Eta position; Phi postion",50,-.02,.02,50,-.02,.02);
  hE[219]=h=new TH2F("muEsmdRatioUV","Ratio of 7 strip sum to 41 strip sum; U ratio ; V ratio",50,0.,1.,50,0.,1.);
  hE[220]=h=new TH2F("muEclustET_esmdRatio","Tower cluster E_{T} vs. Ratio of 7 strip sum to 41 strip sum (U+V); Tower cluster E_{T} ; U+V ratio",100,0.,100.,50,0.,1.);

  hE[221]=h=new TH2F("muEpre2_pre1","Preshower 2 vs Preshower 1 Energy; Pre1 ene; Pre2 ene",50,0.,.02,50,0.,.05);
  hE[222]=h=new TH2F("muEpost_pre12","Postshower vs Preshower 1+2; Pre1+2 ene; Post ene",50,0.,.1,60,0.,.03);
  hE[223]=h=new TH2F("muEclustET_pre12","Preshower 1+2 vs Tower cluster E_{T}; Tower cluster E_{T}; Pre1+2 ene",100,0.,100.,50,0.,.1);
  hE[224]=h=new TH2F("muEclustET_post"," Postshower vs Tower cluster E_{T}; Tower cluster E_{T}; Post ene",100,0.,100.,60,0.,.03);
  hE[225]=h=new TH2F("muEsmdEne_pre12","SMD Energy vs Preshower 1+2; Pre1+2 ene; SMD energy",50.,0.,.1,100,0.,1000.);
  hE[226]=h=new TH2F("muEsmdEne_post","SMD Energy vs Postshower; Post ene; SMD energy",60.,0.,.03,500,0.,1000.);
  hE[227]=h=new TH2F("muEclustET_esmdEneSum7","Tower cluster E_{T} vs. Energy in SMD Planes (7 strip sum); Tower cluster E_{T}; SMD Energy (U+V)",100,0.,100.,500,0.,1000.);
  hE[228]=h=new TH2F("muEclustET_esmdMaxADC","Tower cluster E_{T} vs. ESMD Max ADC; Tower cluster E_{T}; SMD Max ADC",100,0.,100.,420,0.,4200.);

  hE[230]=h=new TH2F("muEsPtBalance_clustPassSMD","Endcap: sPtBalance vs cluster ET (pass SMD ratio cut); 2x2 Cluster ET (GeV); signed Pt balance (GeV)",100,0,100,100,-100,100);
  hE[231]=h=new TH2F("muEsPtBalance2_clustPassSMD","Endcap: sPtBalance2 vs cluster ET (pass SMD ratio cut); 2x2 Cluster ET (GeV); signed Pt balance 2 (GeV)",100,0,100,100,-100,100);

  hE[232]=h=new TH2F("muEsPtBalance_clustFailSMD","Endcap: sPtBalance vs cluster ET (fail SMD ratio cut); 2x2 Cluster ET (GeV); signed Pt balance (GeV)",100,0,100,100,-100,100);
  hE[233]=h=new TH2F("muEsPtBalance2_clustFailSMD","Endcap: sPtBalance2 vs cluster ET (fail SMD ratio cut); 2x2 Cluster ET (GeV); signed Pt balance 2 (GeV)",100,0,100,100,-100,100);

  hE[235]=h=new TH2F("muEsmdRatioUVfailPtBal_ET25","Ratio of 7 strip sum to 41 strip sum; U ratio ; V ratio",100,0.,1.,100,0.,1.);
  hE[236]=h=new TH2F("muEclustET_esmdRatiofailPtBal","Tower cluster E_{T} vs. Ratio of 7 strip sum to 41 strip sum (U+V); Tower cluster E_{T} ; U+V ratio",100,0.,100.,100,0.,1.);

  hE[237]=h=new TH2F("muEsPtBalance_esmdRatio_ET25","sPtBalance vs. Ratio of 7 strip sum to 41 strip sum (U+V); sPtBalance ; U+V ratio",100,-100.,100.,100,0.,1.);
  hE[238]=h=new TH2F("muEsPtBalance2_esmdRatio_ET25","sPtBalance2 vs. Ratio of 7 strip sum to 41 strip sum (U+V); sPtBalance2 ; U+V ratio",100,-100.,100.,100,0.,1.);

  hE[239]=h=new TH2F("muEsPtBalance_awayTot_ET25","Endcap (ET > 25): sPtBalance vs awayside PT; awayside PT; sPtBalance",100,0,100,100,-100,100);
  Lx=h->GetListOfFunctions();
  ln=new TLine(0,parE_ptBalance,100,parE_ptBalance);  ln->SetLineColor(kRed);  Lx->Add(ln);
  
  //charge sign flip with vertex refit
  char cPM[2]={'P','N'}; // Positive, Negative
  for(int ipn=0;ipn<2;ipn++){
    sprintf(txt0,"muE_WET%cg",cPM[ipn]);
    sprintf(txt,"Final Endcap W glob Q=%c; 2x2 cluster ET ",cPM[ipn]);
    hE[240+ipn]=h=new TH1F(txt0, txt, 100,0,100);
    Lx=h->GetListOfFunctions();
    ln=new TLine(parE_highET,0,parE_highET,1.e6);  ln->SetLineColor(kRed);  Lx->Add(ln);
    
    sprintf(txt0,"muE_WET%cp",cPM[ipn]);
    sprintf(txt,"Final W Endcap prim Q=%c; 2x2 cluster ET ",cPM[ipn]);
    hE[242+ipn]=h=new TH1F(txt0, txt, 100,0,100);
    Lx=h->GetListOfFunctions();
    ln=new TLine(parE_highET,0,parE_highET,1.e6);  ln->SetLineColor(kRed);  Lx->Add(ln);
    
    
    sprintf(txt0,"muE_CF%c0",cPM[ipn]);
    sprintf(txt,"Endcap prim sign=%c flip after V-refit; 2x2 cluster ET ",cPM[ipn]);
    hE[244+ipn]=h=new TH1F(txt0, txt, 100,0,100);
  }
  
  hE[250]=h=new TH2F("muEsPtBalance2_esmdRatio_ET25_P","Q+ : sPtBalance2 vs. Ratio of 7 strip sum to 41 strip sum (U+V); sPtBalance2 ; U+V ratio",100,-100.,100.,100,0.,1.);
  hE[251]=h=new TH2F("muEsPtBalance2_esmdRatio_ET25_N","Q- : sPtBalance2 vs. Ratio of 7 strip sum to 41 strip sum (U+V); sPtBalance2 ; U+V ratio",100,-100.,100.,100,0.,1.);

  hE[252]=h=new TH2F("muEsPtBalance_esmdRatio_ET20","ET>20: sPtBalance vs. Ratio of 7 strip sum to 41 strip sum (U+V); sPtBalance ; U+V ratio",100,-100.,100.,100,0.,1.);
  hE[253]=h=new TH2F("muEsPtBalance2_esmdRatio_ET20","ET>20: sPtBalance2 vs. Ratio of 7 strip sum to 41 strip sum (U+V); sPtBalance2 ; U+V ratio",100,-100.,100.,100,0.,1.);
  hE[254]=h=new TH2F("muEsPtBalance2_esmdRatio_ET20_P","Q+ ET>20: sPtBalance2 vs. Ratio of 7 strip sum to 41 strip sum (U+V); sPtBalance2 ; U+V ratio",100,-100.,100.,100,0.,1.);
  hE[255]=h=new TH2F("muEsPtBalance2_esmdRatio_ET20_N","Q- ET>20: sPtBalance2 vs. Ratio of 7 strip sum to 41 strip sum (U+V); sPtBalance2 ; U+V ratio",100,-100.,100.,100,0.,1.);

  hE[256]=h=new TH2F("muE_UoffStr","peak offset from track ESMD-U; track phi(rad); #Delta strip", 48,-PI,PI,11,-5.5,5.5);
  Lx=h->GetListOfFunctions();
  int dd=parE_esmdWL-parE_esmdGL+0.5;
  ln=new TLine(-PI,-dd, PI, -dd);  ln->SetLineColor(kRed);  Lx->Add(ln);
  ln=new TLine(-PI,dd, PI, dd);  ln->SetLineColor(kRed);  Lx->Add(ln);
  ln=new TLine(-PI,0, PI, 0.);  ln->SetLineColor(kBlue);  Lx->Add(ln);
  
  hE[257]=h=new TH2F("muE_VoffStr","peak offset from track ESMD-V; (4 bins per TPC sector)      TPC track phi(rad); #Delta strip", 48,-PI,PI,11,-5.5,5.5);
  Lx=h->GetListOfFunctions();  
  ln=new TLine(-PI,-dd, PI, -dd);  ln->SetLineColor(kRed);  Lx->Add(ln);
  ln=new TLine(-PI,dd, PI, dd);  ln->SetLineColor(kRed);  Lx->Add(ln);
  ln=new TLine(-PI,0, PI, 0.);  ln->SetLineColor(kBlue);  Lx->Add(ln);


  // add histos to the list (if provided)
  for(int i=0;i<mxHE;i++) {
    if(  hE[i]==0) continue;
    HList->Add( hE[i]);
  }

  //  HList->ls();
  LOG_INFO<<Form("%s::initEHistos done",GetName())<<endm;

}
