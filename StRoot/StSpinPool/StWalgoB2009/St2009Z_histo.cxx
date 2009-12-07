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
  TList *Lx;  TLine *ln;TH1 *h;
  char txt[1000], txt0[100];
  int nCase=2;

  hA[0]=h=new TH1F(core+"EventType",core+" event type",nCase,0,nCase);
  h->GetXaxis()->SetTitleOffset(0.4);  h->GetXaxis()->SetLabelSize(0.06);  h->GetXaxis()->SetTitleSize(0.05); h->SetMinimum(0.8);
  h->SetLineColor(kBlue);h->SetLineWidth(2);

  char *key[]={"L2W","L2Wnormal","L2Wrandom"};
  for(int i=0;i<3;i++) h->Fill(key[i],0.); // preset the order of keys
  
  //final selection stuff:

  hA[1]=h=new TH1F(core+"Zmass","  Final Z selection; Invariant Mass (GeV)", 50,0,200);
  //  Lx=h->GetListOfFunctions();
  //  ln=new TLine(par_highET,0,par_highET,1.e6);  ln->SetLineColor(kRed);  Lx->Add(ln);
  sprintf(txt,"Reconstructed Charge of each track in candidate  ;First Charge;Second Charge");
  hA[2]=h=new TH2F(core+"charge_v_charge", txt,3,-1.5,1.5,3,-1.5,1.5);
  sprintf(txt,"Product of Reconstructed Charges of each track in candidate");
  hA[3]=h=new TH1F(core+"charge_product", txt,3,-1.5,1.5);
  sprintf(txt,"Relative phi between two tracks in Z candidate");
  hA[4]=h=new TH1F(core+"relative_phi", txt,100,0,PI*2.02);
  //  Lx=h->GetListOfFunctions();
  //  ln=new TLine(0,0,100,0);  ln->SetLineColor(kMagenta);  Lx->Add(ln);


  //cut details:
  hA[5]=h=new TH2F(core+"fmax_v_fmax","Final Z Selection, Fmax v Fmax;fmax1;fmax2",100,0,1,100,0,1);
  hA[6]=h=new TH2F(core+"et_v_et","Transverse Energies of the two Clusters;Et1;Et2",100,0,100,100,0,100);
  hA[7]=h=new TH1F(core+"et1before","Transverse Energy of first Cluster (before cuts)",100,0,100);
  hA[8]=h=new TH2F(core+"rel_phi_v_mass","Relative Phi vs Invariant Mass;mass;phi",50,0,200,100,0,PI*2.02);

  //event/vertex details:
  hA[9]=h=new TH1F(core+"nVertices","Number of vertices per event",10,0,10);
  hA[10]=h=new TH1F(core+"nTracks","Number of tracks per vertex",20,0,20);

 sprintf(txt,"Product of Electron Charges vs Reconstructed Mass;mass (GeV);Q1*Q2");
  hA[11]=h=new TH2F(core+"charge_product_v_mass",txt,50,0,200,3,-1.5,1.5);
  hA[12]=h=new TH2F(core+"phi1_v_phi2","Comparison of azimuthal position of first and second track;Phi 1;Phi 2",100,-PI,PI,100,-PI,PI);
  hA[13]=h=new TH1F(core+"et1","ET of first cluster (after cuts)",100,0,100);
  hA[14]=h=new TH2F(core+"positron_pos","Location of e+ from Z;eta;phi",10,-1,1,10,-PI,PI);
  hA[15]=h=new TH2F(core+"electron_pos","Location of e- from Z;eta;phi",10,-1,1,10,-PI,PI);


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
