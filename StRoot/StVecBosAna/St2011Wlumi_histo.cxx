// $Id: St2011Wlumi_histo.cxx,v 1.1 2012/10/09 15:21:20 smirnovd Exp $
//
//*-- Author : Ross Corliss, MIT

#include <TH2.h>
#include <TList.h>
#include <TLine.h>
#include <TMath.h>

#include "St2011WlumiMaker.h"

//________________________________________________
//________________________________________________
void
St2011WlumiMaker::initHistos(){
  //  const float PI=TMath::Pi();
  TString core="lumi_"; // prefix added to every histo name, to allow for multipl maker saving histos in the same root file


  //...... data histograms
  memset(hA,0,sizeof(hA));
  TList *Lx;  TLine *ln;TH1 *h;
  char txt[1000], txt0[100];
  int nCase=2;

  hA[0]=h=new TH1F(core+"EventType",core+" event type",nCase,0,nCase);
  h->GetXaxis()->SetTitleOffset(0.4);  h->GetXaxis()->SetLabelSize(0.06);  h->GetXaxis()->SetTitleSize(0.05); h->SetMinimum(0.8);
  h->SetLineColor(kBlue);h->SetLineWidth(2);

  char key[][200]={"L2W","L2Wnormal","L2Wrandom"};
  for(int i=0;i<3;i++) h->Fill(key[i],0.); // preset the order of keys
  
  // free 1-4, can be used
  hA[5]=h=new TH1F(core+"WET","  Final W selection; 2x2 cluster ET (GeV), scaled by fdet", 100,0,100);
  Lx=h->GetListOfFunctions();
  ln=new TLine(par_highET,0,par_highET,1.e6);  ln->SetLineColor(kRed);  Lx->Add(ln);

  sprintf(txt,"TPC GLOB Q/PT  ; 2x2 cluster ET (GeV); Q/PT");
  hA[6]=h=new TH2F(core+"chRecPNg", txt,100,0.,100.,100,-0.1,0.1);
  Lx=h->GetListOfFunctions();
  ln=new TLine(0,0,100,0);  ln->SetLineColor(kMagenta);  Lx->Add(ln);

  sprintf(txt,"TPC PRIM  Q/PT ; 2x2 cluster ET (GeV); Q/PT");
  hA[7]=h=new TH2F(core+"chRecPNp", txt,100,0.,100.,100,-0.1,0.1);
  Lx=h->GetListOfFunctions();
  ln=new TLine(0,0,100,0);  ln->SetLineColor(kMagenta);  Lx->Add(ln);

  //free 8-9

  //use 10-15
  char cPM[2]={'P','N'}; // Positive, Negative
  for(int ipn=0;ipn<2;ipn++){ 
    sprintf(txt0,"chWET%cg",cPM[ipn]);
    sprintf(txt,"Final W  glob sign=%c; 2x2 cluster ET ",cPM[ipn]);
    hA[10+ipn]=h=new TH1F(core+txt0, txt, 100,0,100);
    Lx=h->GetListOfFunctions();
    ln=new TLine(par_highET,0,par_highET,1.e6);  ln->SetLineColor(kRed);  Lx->Add(ln);
       
    sprintf(txt0,"chWET%cp",cPM[ipn]);
    sprintf(txt,"Final W  prim sign=%c; 2x2 cluster ET ",cPM[ipn]);
    hA[12+ipn]=h=new TH1F(core+txt0, txt, 100,0,100);
    Lx=h->GetListOfFunctions();
    ln=new TLine(par_highET,0,par_highET,1.e6);  ln->SetLineColor(kRed);  Lx->Add(ln);
       
 
    sprintf(txt0,"chCF%c0",cPM[ipn]);
    sprintf(txt,"prim sign=%c flip after V-refit; 2x2 cluster ET ",cPM[ipn]);
    hA[14+ipn]=h=new TH1F(core+txt0, txt, 100,0,100);
  }

  //use 16+
  sprintf(txt,"Integrated (per run) Lumi vs Time; run number;Luminosity (pb^-1)");
  hA[16]=h=new TH1F(core+"LvsT", txt,200000,10000000,10200000);

  sprintf(txt,"Live fraction of BEMC vs Time; run number;Fraction of towers with good status");
  hA[17]=h=new TH1F(core+"GoodvsT", txt,200000,10000000,10200000);

  sprintf(txt,"Single Beam Background Counts vs Time;run number;Background BHT3 counts");
  hA[18]=h=new TH1F(core+"SBBvsT",txt,200000,10000000,10200000);


  // add histos to the list (if provided)
  for(int i=0;i<mxHA;i++) {
    if(  hA[i]==0) continue;
    HList->Add( hA[i]);
  }
  //  HList->ls();
  LOG_INFO<<Form("%s::initHistos done1",GetName())<<endm;

}


// $Log: St2011Wlumi_histo.cxx,v $
// Revision 1.1  2012/10/09 15:21:20  smirnovd
// *** empty log message ***
//
// Revision 1.1  2011/02/10 20:33:24  balewski
// start
//
