#include <TH2.h>
#include <TList.h>
#include <TLine.h>
#include <TMath.h>

#include "St2011WMaker.h"

#include "St2011ZMaker.h"

//________________________________________________
//________________________________________________
void
St2011ZMaker::initHistos(){
  const float PI=TMath::Pi();
  TString core="_Z_"; // prefix added to every histo name, to allow for multipl maker saving histos in the same root file


  //...... data histograms
  memset(hA,0,sizeof(hA));
  TList *Lx;  TLine *ln;
  TH1 *h; float yMax=1e3;
  char txt[1000];
  int nCase=16;

  hA[0]=h=new TH1F(core+"EventType",core+" event type",nCase,0,nCase);
  h->GetXaxis()->SetTitleOffset(0.4);  h->GetXaxis()->SetLabelSize(0.06);  h->GetXaxis()->SetTitleSize(0.05); h->SetMinimum(0.8);
  h->SetLineColor(kBlue);h->SetLineWidth(2);
  h->SetMarkerSize(2);//<-- large text
  const char *key[]={"inp","vert","TT","tr1","et1","con1","tr2","et2","con2","phi12","m2","QQ","Zlow","Zhigh"};
  for(int i=0;i<14;i++) h->Fill(key[i],0.); // preset the order of keys

  
  //final selection stuff:

  hA[1]=h=new TH1F(core+"Zmass","  Final Z selection; Invariant Mass (GeV)", 100,0,200);
  sprintf(txt,"Reconstructed Charge of each track in candidate  ;First Charge;Second Charge");
  hA[2]=h=new TH2F(core+"charge_v_charge", txt,3,-1.5,1.5,3,-1.5,1.5);
  sprintf(txt,"Product of Reconstructed Charges of each track in candidate");
  hA[3]=h=new TH1F(core+"charge_product", txt,3,-1.5,1.5);
  hA[4]=h=new TH2F(core+"phi1_v_phi2","Comparison of azimuthal position of first and second track;Phi 1;Phi 2",100,-PI,PI,100,-PI,PI);
  sprintf(txt,"Relative phi between two tracks in Z candidate");
  hA[5]=h=new TH1F(core+"relative_phi", txt,100,0,PI*2.02);
  hA[6]=h=new TH2F(core+"track_mom_v_mass","Q1/pT1*Q2/pT2 versus Invariant Mass;Mass (GeV);Q1/pT1*Q2/pT2",100,0,200,200,-0.1,0.1);
 sprintf(txt,"Product of Electron Charges vs Reconstructed Mass;mass (GeV);Q1*Q2");
  hA[7]=h=new TH2F(core+"charge_product_v_mass",txt,50,0,200,3,-1.5,1.5);
  hA[8]=h=new TH1F(core+"et1","ET of first cluster (after cuts)",100,0,100);
  hA[9]=h=new TH2F(core+"positron_pos","Location of e+ from Z;eta;phi",10,-1,1,10,-PI,PI);
  hA[10]=h=new TH2F(core+"electron_pos","Location of e- from Z;eta;phi",10,-1,1,10,-PI,PI);
  hA[11]=h=new TH2F(core+"fmax_v_fmax","Final Z Selection, Fmax v Fmax;fmax1;fmax2",100,0,1,100,0,1);
  hA[12]=h=new TH2F(core+"et_v_et","Transverse Energies of the two Clusters;Et1;Et2",100,0,100,100,0,100);
  hA[13]=h=new TH2F(core+"rel_phi_v_mass","Relative Phi vs Invariant Mass;mass;phi",50,0,200,100,0,PI*2.02);
  hA[14]=h=new TH1F(core+"ZmassLike","  Final Z selection with Like Charges on both tracks; Invariant Mass (GeV)", 100,0,200);

  //cut details:
  hA[15]=h=new TH1F(core+"ZmassUnlike","  Final Zs,  Unlike Charge pairs; Invariant Mass (GeV)", 100,0,200);   h->SetFillColor(kYellow);

  Lx=h->GetListOfFunctions();
  ln=new TLine(par_minMassZ,0,par_minMassZ,yMax);  ln->SetLineColor(kMagenta);  Lx->Add(ln);
  ln=new TLine(par_maxMassZ,0,par_maxMassZ,yMax);  ln->SetLineColor(kMagenta);  Lx->Add(ln);
  ln=new TLine(91.2,-.2,91.2,10.);  ln->SetLineColor(kGreen); ln->SetLineWidth(3);  ln->SetLineStyle(1); Lx->Add(ln);

  hA[21]=h=new TH2F(core+"fmax_v_fmaxbefore","Final Z Selection, Fmax v Fmax;fmax1;fmax2",100,0,1,100,0,1);
  hA[22]=h=new TH2F(core+"et_v_etbefore","Transverse Energies of the two Clusters;Et1;Et2",100,0,100,100,0,100);

  // new
  hA[23]=h=new TH1F(core+"et1val","Track-1 ET before cuts; 2x2ET (GeV)",100,0,100);
  Lx=h->GetListOfFunctions();
  ln=new TLine(par_clusterEtZ,0,par_clusterEtZ,yMax);  ln->SetLineColor(kMagenta);  Lx->Add(ln);

  hA[24]=h=new TH1F(core+"et1frac","Track-1 cone ET fraction; 2x2ET/nearET",105,0,1.05);
  Lx=h->GetListOfFunctions();
  ln=new TLine(par_nearTotEtFracZ,0,par_nearTotEtFracZ,yMax);  ln->SetLineColor(kMagenta);  Lx->Add(ln);

  hA[25]=h=new TH1F(core+"et2val","Track-2 ET before cuts; 2x2ET (GeV)",100,0,100);
  Lx=h->GetListOfFunctions();
  ln=new TLine(par_clusterEtZ,0,par_clusterEtZ,yMax);  ln->SetLineColor(kMagenta);  Lx->Add(ln);

  hA[26]=h=new TH1F(core+"et2frac","Track-2 cone ET fraction; 2x2ET/nearET",105,0,1.05);
  Lx=h->GetListOfFunctions();
  ln=new TLine(par_nearTotEtFracZ,0,par_nearTotEtFracZ,yMax);  ln->SetLineColor(kMagenta);  Lx->Add(ln);

  hA[27]=h=new TH1F(core+"phi12","delta phi tr1-tr2; delPhi12(rad)", 100 ,-PI+1.,PI+1.);
  h->SetFillColor(kBlue);
  Lx=h->GetListOfFunctions();
  ln=new TLine(par_delPhi12,0,par_delPhi12,yMax);  ln->SetLineColor(kMagenta);  Lx->Add(ln);
  ln=new TLine(-par_delPhi12,0,-par_delPhi12,yMax);  ln->SetLineColor(kMagenta);  Lx->Add(ln);


  hA[29]=h=new TH1F(core+"et1iso","Track-1 4x4 ET fract; 2x2ET / 4x4 ET",105,0,1.05);
  Lx=h->GetListOfFunctions();
  ln=new TLine(wMK->par_clustFrac24,0,wMK->par_clustFrac24,yMax);  ln->SetLineColor(kMagenta);  ln->SetLineStyle(2); Lx->Add(ln);

  hA[30]=h=new TH1F(core+"et2iso","Track-2 4x4 ET fract; 2x2ET / 4x4 ET",105,0,1.05);
  Lx=h->GetListOfFunctions();
  ln=new TLine(wMK->par_clustFrac24,0,wMK->par_clustFrac24,yMax);  ln->SetLineColor(kMagenta);  ln->SetLineStyle(2); Lx->Add(ln);


  //event/vertex details:
  hA[31]=h=new TH1F(core+"nVertices","Number of vertices per event",10,0,10);
  hA[32]=h=new TH1F(core+"nTracks","Number of tracks per vertex",20,0,20);

  hA[33]=h=new TH2F(core+"chRecPNp","TPC PRIM  Q/PT , black=pairs of unlike charges; 2x2 cluster ET (GeV); Q/PT",100,0.,100.,100,-0.1,0.1);
  Lx=h->GetListOfFunctions();
  ln=new TLine(0,0,100,0);  ln->SetLineColor(kMagenta);  Lx->Add(ln);

  hA[34]=h=new TH2F(core+"Ene_Deta","final Z: cluster energy vs. detector eta; barrel eta bin; 2x2 Energy (GeV)",40,0,40,50,0,100);

  hA[35]=h=new TH2F(core+"eta12","Final Z selection #eta_{1} vs #eta_{2} with two Barrel tracks; #eta_{1} ; #eta_{2} ", 60,-1.0,2.0,60,-1.0,2.0);
  
  hA[36]=h=new TH1F(core+"etaZ","golden Z boson #eta; #eta_{Z}",100,-5.,5.);
  hA[37]=h=new TH1F(core+"ptZ","golden Z boson p_{T}; p_{T}^{Z}",100,0.,20.);

  hA[38]=h=new TH1F(core+"Y2","golden Z spin sorted; spin4 ",16,-0.5,15.5);
  h->SetMarkerSize(2);//<-- large text
  hA[39]=h=new TH1F(core+"Y2a","golden Z , STAR #eta<0 , spin sorted; spin4 ",16,-0.5,15.5);
  h->SetMarkerSize(2);//<-- large text
  hA[40]=h=new TH1F(core+"Y2b","golden Z , STAR #eta>0 , spin sorted; spin4 ",16,-0.5,15.5);
  h->SetMarkerSize(2);//<-- large text
  
  hA[42]=h=new TH2F(core+"phiCorr12","#phi correlation; #phi_{e+} (rad); #phi_{e-} (rad)", 240,-PI,PI,240,-PI,PI);
  hA[43]=h=new TH2F(core+"ChRecHypCorrPNp","TPC PRIM  Charge Separation Hyperbola Corrected ; 2x2 cluster ET (GeV); Q*ET/PT",100,0.,100.,100,-4,4);
  ln=new TLine(0,0,100,0);  ln->SetLineColor(kMagenta);  Lx->Add(ln);
  hA[44]=h=new TH2F(core+"xCorr12","reconstructed bjorken-x correlation; x_{1}; x_{2}", 300,0,0.5,300,0,0.5);
  hA[45]=h=new TH2F(core+"xCorr12recoM","reconstructed bjorken-x correlation (reco mass); x_{1}; x_{2}", 300,0,0.5,300,0,0.5);

  hA[46]=h=new TH2F(core+"FreeQ","reco charges correlation, golden Z; e+ Q *ET/PT; e- Q *ET/PT",100,0.,5.,100,-5.,0);
  //************* endcap histos ***************

  // barrel track matched with endcap cluster (no track requirement
  hA[50]=h=new TH1F(core+"EndcapNoTrk_EventType",core+" event type",nCase,0,nCase);
  h->GetXaxis()->SetTitleOffset(0.4);  h->GetXaxis()->SetLabelSize(0.06);  h->GetXaxis()->SetTitleSize(0.05); h->SetMinimum(0.8);
  h->SetLineColor(kBlue);h->SetLineWidth(2);
  const char *keyE[]={"inp","vert","trB","etB","conB","trE","etE","conE","phi12","m2"};
  for(int i=0;i<10;i++) h->Fill(keyE[i],0.); // preset the order of keys

   hA[51]=h=new TH1F(core+"Eet1iso","Track-1 4x4 ET fract; 2x2ET / 4x4 ET",105,0,1.05);
  Lx=h->GetListOfFunctions();
  ln=new TLine(wMK->par_clustFrac24,0,wMK->par_clustFrac24,yMax);  ln->SetLineColor(kMagenta);  ln->SetLineStyle(2); Lx->Add(ln);
  hA[52]=h=new TH1F(core+"Eet1val","Track-1 ET before cuts; 2x2ET (GeV)",100,0,100);
  Lx=h->GetListOfFunctions();
  ln=new TLine(par_clusterEtZ,0,par_clusterEtZ,yMax);  ln->SetLineColor(kMagenta);  Lx->Add(ln);
  hA[53]=h=new TH1F(core+"Eet1frac","Track-1 cone ET fraction; 2x2ET/nearET",105,0,1.05);
  Lx=h->GetListOfFunctions();
  ln=new TLine(par_nearTotEtFracZ,0,par_nearTotEtFracZ,yMax);  ln->SetLineColor(kMagenta);  Lx->Add(ln);

  hA[54]=h=new TH1F(core+"Eet2isoNoTrk","Track-2 4x4 ET fract; 2x2ET / 4x4 ET",105,0,1.05);
  Lx=h->GetListOfFunctions();
  ln=new TLine(wMK->parE_clustFrac24,0,wMK->parE_clustFrac24,yMax);  ln->SetLineColor(kMagenta);  ln->SetLineStyle(2); Lx->Add(ln);
  hA[55]=h=new TH1F(core+"Eet2valNoTrk","Cluster-2 ET before cuts; 2x2ET (GeV)",100,0,100);
  Lx=h->GetListOfFunctions();
  ln=new TLine(par_clusterEtZ,0,par_clusterEtZ,yMax);  ln->SetLineColor(kMagenta);  Lx->Add(ln);
  hA[56]=h=new TH1F(core+"Eet2fracNoTrk","Cluset-2 cone ET fraction; 2x2ET/nearET",105,0,1.05);
  Lx=h->GetListOfFunctions();
  ln=new TLine(wMK->parE_nearTotEtFrac,0,wMK->parE_nearTotEtFrac,yMax);  ln->SetLineColor(kMagenta);  Lx->Add(ln);

  hA[57]=h=new TH1F(core+"Ephi12NoTrk","delta phi tr1-tr2; delPhi12(rad)", 100 ,-PI+1.,PI+1.);
  h->SetFillColor(kBlue);
  Lx=h->GetListOfFunctions();
  ln=new TLine(par_delPhi12,0,par_delPhi12,yMax);  ln->SetLineColor(kMagenta);  Lx->Add(ln);
  ln=new TLine(-par_delPhi12,0,-par_delPhi12,yMax);  ln->SetLineColor(kMagenta);  Lx->Add(ln);
  hA[58]=h=new TH1F(core+"E_ZmassNoTrk","  Final Z selection with one varrel track and one Endcap cluster (no track requirement); Invariant Mass (GeV)", 100,0,200);
  h->SetFillColor(kYellow);
  ln=new TLine(91.2,-.2,91.2,10.);  ln->SetLineColor(kGreen); ln->SetLineWidth(3);  ln->SetLineStyle(1); Lx->Add(ln);

  hA[59]=h=new TH2F(core+"Eeta12NoTrk","Final Z selection #eta_{1} vs #eta_{2} with one Barrel track and one Endcap cluster (no track requirement); #eta_{1} ; #eta_{2} ", 60,-1.0,2.0,60,-1.0,2.0);
  
  hA[60]=h=new TH1F(core+"EetaZNoTrk","Z boson #eta with one Barrel track and one Endcap cluster (no track requirement); #eta_{Z}",100,-5.,5.);
  hA[61]=h=new TH1F(core+"EptZNoTrk","Z boson p_{T} with one Barrel track and one Endcap cluster (no track requirement); p_{T}^{Z}",100,0.,20.);

  // barrel track matched with endcap track candidate
  hA[70]=h=new TH1F(core+"Endcap_EventType",core+" event type",nCase,0,nCase);
  h->GetXaxis()->SetTitleOffset(0.4);  h->GetXaxis()->SetLabelSize(0.06);  h->GetXaxis()->SetTitleSize(0.05); h->SetMinimum(0.8);
  h->SetLineColor(kBlue);h->SetLineWidth(2);
  const char *keyE2[]={"inp","vert","trB","etB","conB","trE","etE","conE","phi12","m2","QQ"};
  for(int i=0;i<11;i++) h->Fill(keyE2[i],0.); // preset the order of keys

  hA[71]=h=new TH1F(core+"Eet2iso","Track-2 4x4 ET fract; 2x2ET / 4x4 ET",105,0,1.05);
  Lx=h->GetListOfFunctions();
  ln=new TLine(wMK->parE_clustFrac24,0,wMK->parE_clustFrac24,yMax);  ln->SetLineColor(kMagenta);  ln->SetLineStyle(2); Lx->Add(ln);
  hA[72]=h=new TH1F(core+"Eet2val","Cluster-2 ET before cuts; 2x2ET (GeV)",100,0,100);
  Lx=h->GetListOfFunctions();
  ln=new TLine(par_clusterEtZ,0,par_clusterEtZ,yMax);  ln->SetLineColor(kMagenta);  Lx->Add(ln);
  hA[73]=h=new TH1F(core+"Eet2frac","Cluset-2 cone ET fraction; 2x2ET/nearET",105,0,1.05);
  Lx=h->GetListOfFunctions();
  ln=new TLine(wMK->parE_nearTotEtFrac,0,wMK->parE_nearTotEtFrac,yMax);  ln->SetLineColor(kMagenta);  Lx->Add(ln);

  hA[74]=h=new TH1F(core+"Ephi12","delta phi tr1-tr2; delPhi12(rad)", 100 ,-PI+1.,PI+1.);
  h->SetFillColor(kBlue);
  Lx=h->GetListOfFunctions();
  ln=new TLine(par_delPhi12,0,par_delPhi12,yMax);  ln->SetLineColor(kMagenta);  Lx->Add(ln);
  ln=new TLine(-par_delPhi12,0,-par_delPhi12,yMax);  ln->SetLineColor(kMagenta);  Lx->Add(ln);
  
  hA[75]=h=new TH1F(core+"E_ZmassUnlike","  Final Z unlike charge selection with one Barrel track and one Endcap track; Invariant Mass (GeV)", 100,0,200);
  h->SetFillColor(kYellow);
  ln=new TLine(91.2,-.2,91.2,10.);  ln->SetLineColor(kGreen); ln->SetLineWidth(3);  ln->SetLineStyle(1); Lx->Add(ln);
  hA[76]=h=new TH1F(core+"E_ZmassLike","  Final Z like charge selection with one Barrel track and one Endcap track; Invariant Mass (GeV)", 100,0,200);
  h->SetFillColor(kYellow);
  ln=new TLine(91.2,-.2,91.2,10.);  ln->SetLineColor(kGreen); ln->SetLineWidth(3);  ln->SetLineStyle(1); Lx->Add(ln);


  hA[77]=h=new TH2F(core+"Eeta12","Final Z selection #eta_{1} vs #eta_{2} with one Barrel track and one Endcap track; #eta_{1} ; #eta_{2} ", 60,-1.0,2.0,60,-1.0,2.0);

  hA[78]=h=new TH1F(core+"EetaZ","Z boson #eta with one Barrel track and one Endcap track; #eta_{Z}",100,-5.,5.);
  hA[79]=h=new TH1F(core+"EptZ","Z boson p_{T} with one Barrel track and one Endcap track; p_{T}^{Z}",100,0.,20.);

  hA[80]=h=new TH2F(core+"ELike_chRecPNp","TPC PRIM  Q/PT , black=pairs of unlike charges; 2x2 cluster ET (GeV); Q/PT",100,0.,100.,100,-0.1,0.1);
  Lx=h->GetListOfFunctions();
  ln=new TLine(0,0,100,0);  ln->SetLineColor(kMagenta);  Lx->Add(ln);
  hA[81]=h=new TH2F(core+"EUnlike_chRecPNp","TPC PRIM  Q/PT , black=pairs of unlike charges; 2x2 cluster ET (GeV); Q/PT",100,0.,100.,100,-0.1,0.1);
  Lx=h->GetListOfFunctions();
  ln=new TLine(0,0,100,0);  ln->SetLineColor(kMagenta);  Lx->Add(ln);

  // add histos to the list (if provided)
  for(int i=0;i<mxHA;i++) {
    if(  hA[i]==0) continue;
    HList->Add( hA[i]);
  }
  //  HList->ls();
  LOG_INFO<<Form("%s::initHistos done1",GetName())<<endm;

}
