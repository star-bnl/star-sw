// $Id: StMcJetCalib_histo.cxx,v 1.1 2010/05/01 01:31:45 balewski Exp $
//
//*-- Author : Jan Balewski, MIT


#include <TH2.h>
#include <TList.h>
#include <TLine.h>
#include <TMath.h>

#include "StMcJetCalibMaker.h"

//________________________________________________
//________________________________________________
void
StMcJetCalibMaker::initHistos(){
  //  const float PI=TMath::Pi();

  //...... data histograms
  memset(hA,0,sizeof(hA));
  TList *Lx;  TLine *ln;
  TH1 *h;

  int nCase=10;
  hA[0]=h=new TH1F(core+"StatEve",core+" event count",nCase,0,nCase);
  h->GetXaxis()->SetTitleOffset(0.4);  h->GetXaxis()->SetLabelSize(0.06);  h->GetXaxis()->SetTitleSize(0.05); h->SetMinimum(0.8);
  h->SetLineColor(kBlue);h->SetLineWidth(2); 
  h->SetMarkerSize(2);//<-- large text
 
  const char *key[]={"inp","verG","etaG","W,Z","verR","anyJ","mulJ","delR","xx1","xx2"};
  for(int i=0;i<nCase;i++) h->Fill(key[i],0.); // preset the order of keys
 

  hA[1]=h=new TH1F(core+"_vz1","geant vertex Z; gen Z(cm)",100,-200,200);
  Lx=h->GetListOfFunctions();
  ln=new TLine(-par_vertexZ,0,-par_vertexZ,1e6);  ln->SetLineColor(kRed);  Lx->Add(ln);
  ln=new TLine(par_vertexZ,0,par_vertexZ,1e6);  ln->SetLineColor(kRed);  Lx->Add(ln);


  hA[2]=h=new TH1F(core+"_vz2","1st reco Z vertex error ; Zreco-Zgeant (cm)",100,-4,4);
  Lx=h->GetListOfFunctions();
  ln=new TLine(-par_verZerr,0,-par_verZerr,1e6);  ln->SetLineColor(kRed);  Lx->Add(ln);
  ln=new TLine(par_verZerr,0,par_verZerr,1e6);  ln->SetLineColor(kRed);  Lx->Add(ln);


  hA[3]=h=new TH1F(core+"_dR","#DeltaR(reco-gen) for best match jets; #DeltaR",200,0.,1.2);
  Lx=h->GetListOfFunctions();
  ln=new TLine(par_delRmax,0,par_delRmax,1e6);  ln->SetLineColor(kRed);  Lx->Add(ln);

  hA[4]=new TH2F(core+"_g2","gen C, D, after #eta cut; gen PT GeV/c; gen #eta",40,0.,80.,40,-2.5,2.5);


  //...........
  hA[10]=new TH1F(core+"_mC","gen boson, input (empty for QCD) ; inv mass (GeV)",150,0,150.);
  hA[11]=h=new TH2F(core+"_D1","gen decay 1 ; eta; phi",20,-2.5,2.5,20,-3.2,3.2);
  Lx=h->GetListOfFunctions();
  ln=new TLine(par_jetEtaHigh,-4,par_jetEtaHigh,4);  ln->SetLineColor(kRed);  Lx->Add(ln);
  ln=new TLine(-par_jetEtaHigh,-4,-par_jetEtaHigh,4);  ln->SetLineColor(kRed);  Lx->Add(ln);
  

  hA[12]=new TH2F(core+"_m3","gen q,qbar  matched; PT(gen) GeV/c; gen eta",40,0.,80.,40,-2.5,2.5);

  //free 13-14

  //...... full jet energy .....
  hA[15]=h=new TH1F(core+"_fr0","gen & matched q, qbar; gen PT (GeV/c)",100,0,100);
  hA[16]=h=new TH1F(core+"_fr1","reco jet PT ratio; ratio=PT(reco)/PT(gen)",100,0.0,1.8);


  hA[17]=new TH2F(core+"_fr2","reco jet E fraction; fracE=E(reco)/E(gen); gen eta",70,0.0,1.8,50,-2.5,2.5);
  hA[18]=new TH2F(core+"_fr3","reco jet #eta ; #eta(gen) - #eta(reco); gen eta",50,-.2,.2,40,-2.5,2.5);
  hA[19]=new TH2F(core+"_fr4","reco jet #phi ; #phi(gen) - #phi(reco) (deg); gen eta",50,-20,20,50,-2.5,2.5);
  hA[20]=0;
  hA[21]=h=new TH2F(core+"_fr6","reco jet E; gen E (GeV); rec E (GeV)",60,0.0,120.,60,0,120);
 hA[22]=h=new TH2F(core+"_fr7","reco jet PT; gen PT (GeV/c); rec PT (GeV/c)",50,0.0,100.,50,0,100);


  //... neutral....
  hA[26]=new TH1F(core+"_fr1n","reco jet NEUTRAL PT ratio; ratio=neutPT(reco)/PT(gen)",100,0.0,1.8);
  hA[27]=new TH2F(core+"_fr2n","reco jet NEUTRAL E ; reco jet #eta; reco jet neut E ",40,-2.,2.,60,0.0,80);
 
  //.... charged....
  hA[36]=new TH1F(core+"_fr1c","reco jet CHARGED  PT ratio; ratio=cahrgPT(reco)/PT(gen)",100,0.0,1.8);
  hA[37]=new TH2F(core+"_fr2c","reco jet CHARGED E ; reco jet #eta; reco jet charg. E ",40,-2.,2.,60,0.0,80);

  //..... eta dependent calibration matrix
  for(int i=0;i<mxEta;i++) {
    float eta1=i*0.4-1.2, eta2=eta1+0.4;
    hA[40+i]=new TH2F(core+Form("_iEta%d",i),Form("matched reco jets, #eta=[%.1f,%.1f]; gen PT (GeV/c); rec PT (GeV/c)",eta1,eta2),20,0.0,100.,20,0,100);
  }
  // free 48..63


  // add histos to the list (if provided)
  for(int i=0;i<mxHA;i++) {
    if(  hA[i]==0) continue;
    HList->Add( hA[i]);
  }
  //  HList->ls();
  LOG_INFO<<Form("%s::initHistos done1",GetName())<<endm;

}


// $Log: StMcJetCalib_histo.cxx,v $
// Revision 1.1  2010/05/01 01:31:45  balewski
// added W->JJ code & JES calibration
//
// Revision 1.1  2010/04/16 01:04:43  balewski
// start
//
