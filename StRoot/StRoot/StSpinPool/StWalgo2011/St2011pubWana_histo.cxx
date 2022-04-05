// $Id: St2011pubWana_histo.cxx,v 1.1 2011/02/10 20:33:26 balewski Exp $
//
//*-- Author : Jan Balewski, MIT

#include <TH2.h>
#include <TList.h>
#include <TLine.h>
#include <TMath.h>

#include "St2011WMaker.h"
#include "St2011pubWanaMaker.h"

//________________________________________________
//________________________________________________
void
St2011pubWanaMaker::initHistos(){
  //  const float PI=TMath::Pi();
  TString core="pub"; // prefix added to every histo name, to allow for multipl maker saving histos in the same root file


  //...... data histograms
  memset(hA,0,sizeof(hA));
  TList *Lx;  TLine *ln;TH1 *h;
  char txt[1000], txt0[100];
  int nCase=5;

  hA[0]=h=new TH1F(core+"StatEve",core+" event count",nCase,0,nCase);
  h->GetXaxis()->SetTitleOffset(0.4);  h->GetXaxis()->SetLabelSize(0.06);  h->GetXaxis()->SetTitleSize(0.05); h->SetMinimum(0.8);
  h->SetLineColor(kBlue);h->SetLineWidth(2);

  char key[][200]={"inp","acc","W"};
  for(int i=0;i<3;i++) h->Fill(key[i],0.); // preset the order of keys
  
  hA[1]=new TH2F(core+"CrR","ADC of track pointed tower, per carte; ADC-ped ; crate ID ",250,0.,4100.,30,0.5,30.5);
  
  // free 2-4, can be used

  hA[5]=h=new TH1F(core+"WET","  Final W selection; 2x2 cluster ET (GeV)", 100,0,100);
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

  //use 10-19
  char cPM[2]={'P','N'}; // Positive, Negative
  for(int ipn=0;ipn<2;ipn++){ 
    sprintf(txt0,"chWET%cg",cPM[ipn]);
    sprintf(txt,"Final W  glob Q=%c; 2x2 cluster ET ",cPM[ipn]);
    hA[10+ipn]=h=new TH1F(core+txt0, txt, 100,0,100);
    Lx=h->GetListOfFunctions();
    ln=new TLine(par_highET,0,par_highET,1.e6);  ln->SetLineColor(kRed);  Lx->Add(ln);
       
    sprintf(txt0,"chWET%cp",cPM[ipn]);
    sprintf(txt,"Final W  prim Q=%c; 2x2 cluster ET ",cPM[ipn]);
    hA[12+ipn]=h=new TH1F(core+txt0, txt, 100,0,100);
    Lx=h->GetListOfFunctions();
    ln=new TLine(par_highET,0,par_highET,1.e6);  ln->SetLineColor(kRed);  Lx->Add(ln);
       
 
    sprintf(txt0,"chCF%c0",cPM[ipn]);
    sprintf(txt,"prim sign=%c flip after V-refit; 2x2 cluster ET ",cPM[ipn]);
    hA[14+ipn]=h=new TH1F(core+txt0, txt, 100,0,100);      
 
    sprintf(txt0,"chEtaC%c",cPM[ipn]);
    sprintf(txt,"Final W sel. ET>%.1f GeV, prim Q=%c ; lepton eta ,event ref frame ",par_highET, cPM[ipn]);
    hA[16+ipn]=h=new TH1F(core+txt0, txt, 40,-2,2);      
  }
  

  //20-30 background study cuts
  hA[20]=h=new TH1F(core+"Joe1","Final sel, awayET<8, nearRET>0.9; 2x2 cluster ET (GeV)", 100,0,100);
  hA[21]=h=new TH1F(core+"Joe2","Final sel, awayET>8, nearRET>0.9; 2x2 cluster ET (GeV)", 100,0,100);

  hA[22]=h=new TH1F(core+"Joe3","Final sel, awayET<8, nearRET=[0.8,0.9]; 2x2 cluster ET (GeV)", 100,0,100);
  hA[23]=h=new TH1F(core+"Joe4","Final sel, awayET>8, nearRET=[0.8,0.9]; 2x2 cluster ET (GeV)", 100,0,100);

  hA[24]=h=new TH1F(core+"Joe5","Final sel, awayET<8, nearRET=[0.,0.8]; 2x2 cluster ET (GeV)", 100,0,100);
  hA[25]=h=new TH1F(core+"Joe6","Final sel, awayET>8, nearRET=[0.,0.8]; 2x2 cluster ET (GeV)", 100,0,100);

  hA[26]=h=new TH1F(core+"Joe7","Final sel, awayET<8 ; 2x2 ET/ nearET ", 110,0,1.1);
  hA[27]=h=new TH2F(core+"Joe8","Final sel, nearR>0.9 ; 2x2 cluster ET (GeV); awayET 9GeV)", 100,0,100,100,0,100);

 
  //Study global vs primary tracks //JS
  hA[28]=h=new TH2F(core+"primPT_globPT","global vs primary pT;primary pT;global pT",100,0,100,100,0,100); 
  hA[29]=h=new TH1F(core+"diffprimPT_globPT","global - primary pT; global-primary pT",100,-50,50);  
  hA[30]=h=new TH1F(core+"diffGT1_clustET"," 2x2 cluster ET for global-primary > 1 GeV",100,0,100);  
  hA[31]=h=new TH1F(core+"diffprimPT_globPT_Qflip","global - primary pT when Q flipped; global-primary pT",100,-50,50);  

  
  //eta bins for X sec
  hA[32]=h=new TH1F(core+"etaBin1sig","electron .6 < |#eta| < 1 ; 2x2 cluster ET",100,1,101);
  hA[33]=h=new TH1F(core+"etaBin2sig","electron .3 < |#eta| < .6; 2x2 cluster ET",100,1,101);
  hA[34]=h=new TH1F(core+"etaBin3sig","electron 0 < |#eta| < .3; 2x2 cluster ET",100,1,101);
  hA[35]=h=new TH1F(core+"etaBin1back","electron .6 < |#eta| < 1; 2x2 cluster ET",100,1,101);
  hA[36]=h=new TH1F(core+"etaBin2back","electron .3 < |#eta| < .6; 2x2 cluster ET",100,1,101);
  hA[37]=h=new TH1F(core+"etaBin3back","electron 0 < |#eta| < .3; 2x2 cluster ET",100,1,101);
  hA[38]=h=new TH1F(core+"etaBin1sigNoE","electron .6 < |#eta| < 1; 2x2 cluster ET",100,1,101);
  hA[39]=h=new TH1F(core+"etaBin2sigNoE","electron .3 < |#eta| < .6; 2x2 cluster ET",100,1,101);
  hA[40]=h=new TH1F(core+"etaBin3sigNoE","electron 0 < |#eta| < .3; 2x2 cluster ET",100,1,101);
  
  //charge sorted for X sec
  hA[41]=h=new TH1F(core+"clustPtBalP",Form("PT Balance > %.1f  Q=+; 2x2 Cluster ET",wMK->par_ptBalance),100,0,100);
  hA[42]=h=new TH1F(core+"clustPtBal_bckgrdP",Form("PT Balance < %.1f Q=+; 2x2 Cluster ET",wMK->par_ptBalance),100,0,100);
  hA[43]=h=new TH1F(core+"clustPtBalnoEP",Form("PT Balance > %.1f (EEMC not included) Q=+; 2x2 Cluster ET",wMK->par_ptBalance),100,0,100);
  hA[44]=h=new TH1F(core+"clustPtBalN",Form("PT Balance > %.1f  Q=-; 2x2 Cluster ET",wMK->par_ptBalance),100,0,100);
  hA[45]=h=new TH1F(core+"clustPtBal_bckgrdN",Form("PT Balance < %.1f  Q=-; 2x2 Cluster ET",wMK->par_ptBalance),100,0,100);
  hA[46]=h=new TH1F(core+"clustPtBalnoEN",Form("PT Balance > %.1f  (EEMC not included) Q=-; 2x2 Cluster ET",wMK->par_ptBalance),100,0,100);
  
  

  // add histos to the list (if provided)
  for(int i=0;i<mxHA;i++) {
    if(  hA[i]==0) continue;
    HList->Add( hA[i]);
  }
  //  HList->ls();
  LOG_INFO<<Form("%s::initHistos done1",GetName())<<endm;

}


// Log: $
