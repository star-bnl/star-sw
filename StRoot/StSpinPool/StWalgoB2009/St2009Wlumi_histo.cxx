// $Id: St2009Wlumi_histo.cxx,v 1.2 2011/09/14 14:23:21 stevens4 Exp $
//
//*-- Author : Ross Corliss, MIT

#include <TH2.h>
#include <TList.h>
#include <TLine.h>
#include <TMath.h>

#include "St2009WlumiMaker.h"

//________________________________________________
//________________________________________________
void
St2009WlumiMaker::initHistos(){
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

  sprintf(txt,"blah");
  hA[19]=h=new TH1F(core+"SoftBHT3",txt,200000,10000000,10200000);
  hA[20]=h=new TH1F(core+"HardBHT3",txt,200000,10000000,10200000);
  hA[21]=h=new TH1F(core+"AbortGap1",txt,200000,10000000,10200000);
  hA[22]=h=new TH1F(core+"AbortGap2",txt,200000,10000000,10200000);
  hA[23]=h=new TH1F(core+"ScaledBHT3",txt,200000,10000000,10200000);
  hA[24]=h=new TH1F(core+"effLumi",txt,200000,10000000,10200000);
  hA[25]=h=new TH1F(core+"nBHT3coin_coinBin",txt,16,0,16);

  for (int i=0;i<16;i++) {
    hA[30+i]=h=new TH1F(core+Form("nBTH3coin_coinBin%d",i),txt,200000,10000000,10200000);
    hA[50+i]=h=new TH1F(core+Form("AbortGap1_coinBin%d",i),txt,200000,10000000,10200000);
    hA[70+i]=h=new TH1F(core+Form("AbortGap2_coinBin%d",i),txt,200000,10000000,10200000);
    hA[90+i]=h=new TH1F(core+Form("awaySum_coinBin%d",i),txt,400,0.,400.);
   
  }

  // add histos to the list (if provided)
  for(int i=0;i<mxHA;i++) {
    if(  hA[i]==0) continue;
    HList->Add( hA[i]);
  }
  //  HList->ls();
  LOG_INFO<<Form("%s::initHistos done1",GetName())<<endm;

}


// $Log: St2009Wlumi_histo.cxx,v $
// Revision 1.2  2011/09/14 14:23:21  stevens4
// update used for cross section PRD paper
//
// Revision 1.1  2009/11/23 23:00:18  balewski
// code moved spin-pool
//
