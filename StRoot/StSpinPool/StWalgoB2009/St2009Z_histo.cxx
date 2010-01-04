#include <TH2.h>
#include <TList.h>
#include <TLine.h>
#include <TMath.h>

#include "St2009ZMaker.h"

//________________________________________________
//________________________________________________
void
St2009ZMaker::initHistos(){
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
  const char *key[]={"inp","vert","tr1","et1","Tfr1","4fr1","tr2","et2","Tfr2","4fr2","phi12","m2","QQ","Zlow","Zhigh"};
  for(int i=0;i<15;i++) h->Fill(key[i],0.); // preset the order of keys


  
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
  hA[15]=h=new TH1F(core+"ZmassUnlike","  Final Z selection with Unlike Charges on both tracks; Invariant Mass (GeV)", 100,0,200);
  Lx=h->GetListOfFunctions();
  ln=new TLine(par_minMassZ,0,par_minMassZ,yMax);  ln->SetLineColor(kMagenta);  Lx->Add(ln);
  ln=new TLine(par_maxMassZ,0,par_maxMassZ,yMax);  ln->SetLineColor(kMagenta);  Lx->Add(ln);

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

  hA[27]=h=new TH1F(core+"phi12","delta phi tr1-tr2; delPhi12(rad)", 100 ,-PI,PI);
  Lx=h->GetListOfFunctions();
  ln=new TLine(par_delPhi12,0,par_delPhi12,yMax);  ln->SetLineColor(kMagenta);  Lx->Add(ln);
  ln=new TLine(-par_delPhi12,0,-par_delPhi12,yMax);  ln->SetLineColor(kMagenta);  Lx->Add(ln);

  hA[28]=h=new TH1F(core+"m2","inv mass^2; M^2 (GeV^2)", 100 ,-1e6,1e6);
  h->SetFillColor(kBlue);


  hA[29]=h=new TH1F(core+"et1iso","Track-1 4x4 ET fract; 2x2ET / 4x4 ET",105,0,1.05);
  Lx=h->GetListOfFunctions();
  ln=new TLine(par_4x4EtFracZ,0,par_4x4EtFracZ,yMax);  ln->SetLineColor(kMagenta);  Lx->Add(ln);

  hA[30]=h=new TH1F(core+"et2iso","Track-2 4x4 ET fract; 2x2ET / 4x4 ET",105,0,1.05);
  Lx=h->GetListOfFunctions();
  ln=new TLine(par_4x4EtFracZ,0,par_4x4EtFracZ,yMax);  ln->SetLineColor(kMagenta);  Lx->Add(ln);


  //event/vertex details:
  hA[31]=h=new TH1F(core+"nVertices","Number of vertices per event",10,0,10);
  hA[32]=h=new TH1F(core+"nTracks","Number of tracks per vertex",20,0,20);

  //evolution of cuts:
  hA[40]=h=new TH1F(core+"Zmass_cut0","Before cuts; Invariant Mass (GeV)", 50,0,200);
  hA[41]=h=new TH1F(core+"Zmass_cut1","after first fmax cut; Invariant Mass (GeV)", 50,0,200);
  hA[42]=h=new TH1F(core+"Zmass_cut2","after both fmax cuts; Invariant Mass (GeV)", 50,0,200);
  hA[43]=h=new TH1F(core+"Zmass_cut3","after both ET cuts; Invariant Mass (GeV)", 50,0,200);
  hA[44]=h=new TH1F(core+"Zmass_cut4","after shared cluster cut; Invariant Mass (GeV)", 50,0,200);
  hA[45]=h=new TH1F(core+"Zmass_cut5","after min mass cut; Invariant Mass (GeV)", 50,0,200);
  hA[46]=h=new TH1F(core+"Zmass_cut6","after max mass cut; Invariant Mass (GeV)", 50,0,200);


  // add histos to the list (if provided)
  for(int i=0;i<mxHA;i++) {
    if(  hA[i]==0) continue;
    HList->Add( hA[i]);
  }
  //  HList->ls();
  LOG_INFO<<Form("%s::initHistos done1",GetName())<<endm;

}
