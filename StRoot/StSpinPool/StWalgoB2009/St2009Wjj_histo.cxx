// $Id: St2009Wjj_histo.cxx,v 1.1 2010/04/16 01:04:43 balewski Exp $
//
//*-- Author : Jan Balewski, MIT


#include <TH2.h>
#include <TList.h>
#include <TLine.h>
#include <TMath.h>

#include "St2009WjjMaker.h"

//________________________________________________
//________________________________________________
void
St2009WjjMaker::initHistos(){
  //  const float PI=TMath::Pi();

  //...... data histograms
  memset(hA,0,sizeof(hA));
  TList *Lx;  TLine *ln;
  TH1 *h;
  // char txt[1000], txt0[100];
  int nCase=12;

  hA[0]=h=new TH1F(core+"StatEve",core+" event count",nCase,0,nCase);
  h->GetXaxis()->SetTitleOffset(0.4);  h->GetXaxis()->SetLabelSize(0.06);  h->GetXaxis()->SetTitleSize(0.05); h->SetMinimum(0.8);
  h->SetLineColor(kBlue);h->SetLineWidth(2);
 
  const char *key[]={"inp","trig","vert","anyJ","mulJ","J1","J2","J3","DjPt", "badBx48","XBG1","XBG2","XWcut","eta","W25","Qlow","Qhigh"};
  for(int i=0;i<10;i++) h->Fill(key[i],0.); // preset the order of keys
 

  hA[1]=0;// free
  hA[2]=new TH1F(core+"bX7","Rate vs. raw bx7; bXing= raw bx7",128,-0.5,127.5);

  hA[3]=0;//free
  hA[4]=new TH1F(core+"bX7c","Rate vs. STAR IP bXing(bx7); bXing= bx7+offset",128,-0.5,127.5);

  hA[5]=new TH2F(core+"s4mon","all L2W & vertex ; bXing at STAR (7bit); spin4 from DB",128,-0.5,127.5,32,-0.5,15.5);

  float etaX=2.5;
  for(int j=1;j<=2;j++) { // use h10+h11
    hA[10+j-1]=h=new TH2F(core+Form("_J%d",j),Form("jet%d ; PT (GeV); eta ",j),50,0,100,20,-2.5,2.5);
    Lx=h->GetListOfFunctions();
    ln=new TLine(par_jetPtLow,-etaX,par_jetPtLow,etaX);  ln->SetLineColor(kRed);  Lx->Add(ln);
    ln=new TLine(par_jetPtHigh,-etaX,par_jetPtHigh,etaX);  ln->SetLineColor(kRed);  Lx->Add(ln);
    ln=new TLine(0,par_jetEtaLow,100,par_jetEtaLow);  ln->SetLineColor(kRed);  Lx->Add(ln);
    ln=new TLine(0,par_jetEtaHigh,100,par_jetEtaHigh);  ln->SetLineColor(kRed);  Lx->Add(ln);
  }
  hA[12]=h=new TH2F(core+"_DJ","di-jet ; PT (GeV); eta ",50,0,100,20,-4.,4.);
  Lx=h->GetListOfFunctions();
  ln=new TLine(par_djPtHigh,-etaX*2,par_djPtHigh,etaX*2);  ln->SetLineColor(kRed);  Lx->Add(ln);

  hA[13]=new TH2F(core+"_K2","di-jet ; invM ( GeV); |eta1+eta2| ",50,0,150,20,0,3);

  hA[14]=new TH1F(core+"_K1","di-jet ; invM (GeV)",100,0,150);
  hA[15]=new TH2F(core+"_K3","di-jet ; invM (GeV); |PZ| GeV/c",50,0,150,30,0,120);
  hA[16]=new TH2F(core+"_K4","di-jet ; invM (GeV); pT GeV/c",50,0,150,40,0,40);
  // free 17-19

  hA[20]=new TH2F(core+"_C0","di-jet ; PZ GeV/c;  eta1+eta2 ",30,-120,120,20,-3,3);
  hA[21]=new TH2F(core+"_C1","di-jet vs. all jets (nJ=3...); PZ all jets GeV/c; di-jet eta ",30,-120,120,20,-3,3);
  hA[22]=new TH2F(core+"_C2","di-jet ; PZ GeV/c; pT GeV/c  ",30,-120,120,20,0,40);
 

  // add histos to the list (if provided)
  for(int i=0;i<mxHA;i++) {
    if(  hA[i]==0) continue;
    HList->Add( hA[i]);
  }
  //  HList->ls();
  LOG_INFO<<Form("%s::initHistos done1",GetName())<<endm;

}


// $Log: St2009Wjj_histo.cxx,v $
// Revision 1.1  2010/04/16 01:04:43  balewski
// start
//
