// $Id: St2011pubSpin_histo.cxx,v 1.8 2013/09/13 19:33:13 stevens4 Exp $
//
//*-- Author : Jan Balewski, MIT


#include <TH2.h>
#include <TList.h>
#include <TLine.h>
#include <TMath.h>

#include "St2011pubSpinMaker.h"

//________________________________________________
//________________________________________________
void
St2011pubSpinMaker::initHistos(){
  //  const float PI=TMath::Pi();

  //...... data histograms
  memset(hA,0,sizeof(hA));
  memset(hE,0,sizeof(hE));
  TList *Lx;  TLine *ln;
  TH1 *h;
  char txt[1000], txt0[100];
  int nCase=12;


  // *********** Barrel spin histos **************
  hA[0]=h=new TH1F(core+"StatEve",core+"Barrel event count",nCase,0,nCase);
  h->GetXaxis()->SetTitleOffset(0.4);  h->GetXaxis()->SetLabelSize(0.06);  h->GetXaxis()->SetTitleSize(0.05); h->SetMinimum(0.8);
  h->SetLineColor(kBlue);h->SetLineWidth(2);
  h->SetMarkerSize(2);//<-- large text

  const char *key[]={"inp","badBx48","noZ","BG1","BG2","Wcut","eta","W25","Qlow","Qhigh","Q +","Q -"};
  for(int i=0;i<12;i++) h->Fill(key[i],0.); // preset the order of keys
 

  hA[1]=new TH1F(core+"bX48","Barrel Rate vs. raw bx48; bXing= raw bx48",128,-0.5,127.5);
  hA[2]=new TH1F(core+"bX7","Barrel Rate vs. raw bx7; bXing= raw bx7",128,-0.5,127.5);

  hA[3]=new TH1F(core+"bX48c","Barrel Rate vs. STAR IP bXing(bx48); bXing= bx48+offset",128,-0.5,127.5);
  hA[4]=new TH1F(core+"bX7c","Barrel Rate vs. STAR IP bXing(bx7); bXing= bx7+offset",128,-0.5,127.5);

  hA[5]=new TH2F(core+"s4mon","Barrel all L2W & vertex ; bXing at STAR (7bit); spin4 from DB",128,-0.5,127.5,32,-0.5,15.5);


  hA[6]=new TH1F(core+"Y0","Barrel BG1: L2W-BHT3-rnd & vertex OK & low ET; spin4 ",16,-0.5,15.5);
  hA[7]=new TH1F(core+"Y1",Form("Barrel BG2: vertex & ET<20 &  ET 2x2 << 4x4 : %s; spin4 ",coreTitle.Data()),16,-0.5,15.5);

  hA[8]=h=new TH1F(core+"QpT","Barrel W ET>25 GeV; reco Q/PT  (1/GeV)",100,-0.099,0.099);
  hA[9]=h=new TH2F(core+"QpT2","Barrel TPC PRIM  Q/PT ; 2x2 cluster ET (GeV); Q/PT  (1/GeV)",100,0.,100.,100,-0.1,0.1);

  //use 10-19
  char cPM[2]={'P','N'}; // Positive, Negative
  char iCol[2]={46,9};
  for(int ipn=0;ipn<2;ipn++){ 
    TH1 *h;
    
    //.... J-peak 
    sprintf(txt0,"ET_%c",cPM[ipn]);
    sprintf(txt,"Final Barrel W, charge=%c : %s; 2x2 ET (GeV) ",cPM[ipn],coreTitle.Data());
    hA[10+ipn]=h=new TH1F(core+txt0,txt, 100,1,101); // shifted by 1 for nicer Rebin
    h->SetFillColor(iCol[ipn]);

    //.... 1D spin sorting         
    sprintf(txt0,"Y2_%c",cPM[ipn]);
    sprintf(txt,"Final Barrel W Q=%c, 2x2 ET=[25,50]GeV : %s; spin4  ",cPM[ipn],coreTitle.Data());
    hA[12+ipn]=h=new TH1F(core+txt0,txt,16,-0.5,15.5);
    h->SetFillColor(iCol[ipn]);

    sprintf(txt0,"Y3_%c",cPM[ipn]);
    sprintf(txt,"Final Barrel W Q=%c, 2x2 ET=[32,44]GeV : %s; spin4  ",cPM[ipn],coreTitle.Data());
    hA[14+ipn]=h=new TH1F(core+txt0,txt,16,-0.5,15.5);
    h->SetFillColor(iCol[ipn]);

    sprintf(txt0,"Y4_%c",cPM[ipn]);
    sprintf(txt,"Final Barrel QCD Q=%c, 2x2 ET=[15,20]GeV : %s; spin4  ",cPM[ipn],coreTitle.Data());
    hA[16+ipn]=h=new TH1F(core+txt0,txt,16,-0.5,15.5);
    h->SetFillColor(iCol[ipn]);

    sprintf(txt0,"Y5_%c",cPM[ipn]);
    sprintf(txt,"Final Barrel 2x2 ET  Q=%c; spin4 : %s; 2x2 cluster ET (GeV) ",cPM[ipn],coreTitle.Data());
    hA[18+ipn]=h=new TH2F(core+txt0,txt,16,-0.5,15.5,10,0,100);
    h->SetFillColor(iCol[ipn]);

    sprintf(txt0,"Y6_%c",cPM[ipn]);
    sprintf(txt,"High pT Barrel QCD Q=%c, 2x2 ET=[25,50]GeV: %s; spin4  ",cPM[ipn],coreTitle.Data());
    hA[20+ipn]=h=new TH1F(core+txt0,txt,16,-0.5,15.5);
    h->SetFillColor(iCol[ipn]);

    sprintf(txt0,"Y7_%c",cPM[ipn]);
    sprintf(txt,"High pT Barrel  QCD 2x2 ET  Q=%c; spin4 : %s; 2x2 cluster ET (GeV) ",cPM[ipn],coreTitle.Data());
    hA[22+ipn]=h=new TH2F(core+txt0,txt,16,-0.5,15.5,100,0,100);
    h->SetFillColor(iCol[ipn]);

    sprintf(txt0,"LepEta_%c",cPM[ipn]);
    sprintf(txt,"selecting Barrel Ws Q=%c : %s ; lepton LAB eta",cPM[ipn],coreTitle.Data());
    hA[29+ipn]=h=new TH1F(core+txt0,txt,400, -2.0,2.0);
    Lx=h->GetListOfFunctions();
    ln=new TLine(par_leptonEta1,0,par_leptonEta1,1e6);  ln->SetLineColor(kRed);  Lx->Add(ln);
    ln=new TLine(par_leptonEta2,0,par_leptonEta2,1e6);  ln->SetLineColor(kRed);  Lx->Add(ln);
  }
  // free 24-29

  hA[31]=h=new TH1F(core+"LumET","Lumi monitor; 2x2 ET (GeV)",100,0.,100.);
  hA[32]=h=new TH2F(core+"QpT_hits","nHitsFit vs Q/PT,Barrel W ET>25 GeV; reco Q/PT  (1/GeV); nFitPoints",100,-0.099,0.099,50,0,50);
  hA[33]=h=new TH2F(core+"QpT_hitF","nHitsFrac vs Q/PT,Barrel W ET>25 GeV; reco Q/PT  (1/GeV); nHitsFrac",100,-0.099,0.099,50,0,1.1);
  hA[34]=h=new TH2F(core+"HitsFit_Frac","Barrel nHitsFit vs nHitsFrac; nHitsFit; nHitsFrac",50,0,50,50,0,1.1);



  // *********** Endcap spin histos *************
  hE[0]=h=new TH1F(core+"EStatEve",core+" Endcap event count",nCase,0,nCase);
  h->GetXaxis()->SetTitleOffset(0.4);  h->GetXaxis()->SetLabelSize(0.06);  h->GetXaxis()->SetTitleSize(0.05); h->SetMinimum(0.8);
  h->SetLineColor(kBlue);h->SetLineWidth(2);
  h->SetMarkerSize(2);//<-- large text

  const char *keyE[]={"inp","badBx48","noZ","BG1","BG2","Wcut","eta","W25","Qlow","Qhigh","Q +","Q -"};
  for(int i=0;i<12;i++) h->Fill(keyE[i],0.); // preset the order of keys
 

  hE[1]=new TH1F(core+"EbX48","Endcap Rate vs. raw bx48; bXing= raw bx48",128,-0.5,127.5);
  hE[2]=new TH1F(core+"EbX7","Endcap Rate vs. raw bx7; bXing= raw bx7",128,-0.5,127.5);

  hE[3]=new TH1F(core+"EbX48c","Endcap Rate vs. STAR IP bXing(bx48); bXing= bx48+offset",128,-0.5,127.5);
  hE[4]=new TH1F(core+"EbX7c","Endcap Rate vs. STAR IP bXing(bx7); bXing= bx7+offset",128,-0.5,127.5);

  hE[5]=new TH2F(core+"Es4mon","Endcap all L2W & vertex ; bXing at STAR (7bit); spin4 from DB",128,-0.5,127.5,32,-0.5,15.5);


  hE[6]=new TH1F(core+"EY0","Endcap BG1: L2W-BHT3-rnd & vertex OK & low ET; spin4 ",16,-0.5,15.5);
  hE[7]=new TH1F(core+"EY1",Form("Endcap BG2: vertex & ET<20 &  ET 2x2 << 4x4 : %s; spin4 ",coreTitle.Data()),16,-0.5,15.5);

  hE[8]=h=new TH1F(core+"EQpT","Endcap reco Q/PT,W ET>25 GeV; reco Q/PT  (1/GeV)",100,-0.099,0.099);
  hE[9]=h=new TH2F(core+"EQpT2","Endcap TPC PRIM  Q/PT ; 2x2 cluster ET (GeV); Q/PT  (1/GeV)",100,0.,100.,100,-0.1,0.1);
  
  //use 10-19
  for(int ipn=0;ipn<2;ipn++){ 
    TH1 *h;
    
    //.... J-peak 
    sprintf(txt0,"E_ET_%c",cPM[ipn]);
    sprintf(txt,"Final Endcap W, charge=%c : %s; 2x2 ET (GeV) ",cPM[ipn],coreTitle.Data());
    hE[10+ipn]=h=new TH1F(core+txt0,txt, 100,1,101); // shifted by 1 for nicer Rebin
    h->SetFillColor(iCol[ipn]);

    //.... 1D spin sorting         
    sprintf(txt0,"EY2_%c",cPM[ipn]);
    sprintf(txt,"Final Endcap W Q=%c, 2x2 ET=[25,50]GeV : %s; spin4  ",cPM[ipn],coreTitle.Data());
    hE[12+ipn]=h=new TH1F(core+txt0,txt,16,-0.5,15.5);
    h->SetFillColor(iCol[ipn]);

    sprintf(txt0,"EY3_%c",cPM[ipn]);
    sprintf(txt,"Final Endcap W Q=%c, 2x2 ET=[32,44]GeV : %s; spin4  ",cPM[ipn],coreTitle.Data());
    hE[14+ipn]=h=new TH1F(core+txt0,txt,16,-0.5,15.5);
    h->SetFillColor(iCol[ipn]);

    sprintf(txt0,"EY4_%c",cPM[ipn]);
    sprintf(txt,"Final Endcap (not all QCD!) Q=%c, 2x2 ET=[15,20]GeV : %s; spin4  ",cPM[ipn],coreTitle.Data());
    hE[16+ipn]=h=new TH1F(core+txt0,txt,16,-0.5,15.5);
    h->SetFillColor(iCol[ipn]);

    sprintf(txt0,"EY5_%c",cPM[ipn]);
    sprintf(txt,"Final Endcap 2x2 ET  Q=%c; spin4 : %s; 2x2 cluster ET (GeV) ",cPM[ipn],coreTitle.Data());
    hE[18+ipn]=h=new TH2F(core+txt0,txt,16,-0.5,15.5,10,0,100);
    h->SetFillColor(iCol[ipn]);

    sprintf(txt0,"ELepEta_%c",cPM[ipn]);
    sprintf(txt,"selecting Endcap Ws Q=%c : %s ; lepton LAB eta",cPM[ipn],coreTitle.Data());
    hE[29+ipn]=h=new TH1F(core+txt0,txt,400, -2.0,2.0);
    Lx=h->GetListOfFunctions();
    ln=new TLine(parE_leptonEta1,0,parE_leptonEta1,1e6);  ln->SetLineColor(kRed);  Lx->Add(ln);
    ln=new TLine(parE_leptonEta2,0,parE_leptonEta2,1e6);  ln->SetLineColor(kRed);  Lx->Add(ln);
  }
  // free 20-29

  hE[31]=h=new TH1F(core+"ELumET","Lumi monitor; 2x2 ET (GeV)",100,0.,100.);
  hE[32]=h=new TH2F(core+"EQpT_hits","nHitsFit vs Q/PT,Endcap W ET>25 GeV; reco Q/PT  (1/GeV); nFitPoints",100,-0.099,0.099,50,0,50);
  hE[33]=h=new TH2F(core+"EQpT_hitF","nHitsFrac vs Q/PT,Endcap W ET>25 GeV; reco Q/PT  (1/GeV); nHitsFrac",100,-0.099,0.099,50,0,1.1);
  hE[34]=h=new TH2F(core+"EHitsFit_Frac","Endcap nHitsFit vs nHitsFrac; nHitsFit; nHitsFrac",50,0,50,50,0,1.1);

  // add histos to the list (if provided)
  for(int i=0;i<mxHA;i++) {
    if(  hA[i]==0) continue;
    HList->Add( hA[i]);
  }
  for(int i=0;i<mxHE;i++) {
    if(  hE[i]==0) continue;
    HList->Add( hE[i]);
  }
  //  HList->ls();
  LOG_INFO<<Form("%s::initHistos done1",GetName())<<endm;

}


// $Log: St2011pubSpin_histo.cxx,v $
// Revision 1.8  2013/09/13 19:33:13  stevens4
// Updates to code for combined 2011+2012 result presented to spin PWG 9.12.13
//
// Revision 1.7  2013/06/14 21:08:51  jlzhang
// add histo Q/pT vs. nHitsFit and Q/pT vs. nHitsPos
//
// Revision 1.7 2013/05/04/           zhang
// Add nHitsFit vs Q/pT and nHitsFit/nHitsPoss vs Q/pT , recover Q/pT 
// Revision 1.6  2012/09/26 14:21:00  stevens4
// use PtBal cos(phi) for WB and WE algos and use Q*ET/PT for barrel charge sign
//
// Revision 1.5  2012/09/17 03:29:30  stevens4
// Updates to Endcap algo and Q*ET/PT charge separation
//
// Revision 1.4  2012/08/28 14:28:28  stevens4
// add histos for barrel and endcap algos
//
// Revision 1.3  2012/08/21 21:28:22  stevens4
// Add spin sorting for endcap Ws
//
// Revision 1.2  2012/08/07 21:06:38  stevens4
// update to tree analysis to produce independent histos in a TDirectory for each eta-bin
//
// Revision 1.1  2011/02/10 20:33:26  balewski
// start
//
