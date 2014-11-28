// $Id: St2009pubSpin_histo.cxx,v 1.11 2010/05/01 01:31:45 balewski Exp $
//
//*-- Author : Jan Balewski, MIT


#include <TH2.h>
#include <TList.h>
#include <TLine.h>
#include <TMath.h>

#include "St2009pubSpinMaker.h"

//________________________________________________
//________________________________________________
void
St2009pubSpinMaker::initHistos(){
  //  const float PI=TMath::Pi();

  //...... data histograms
  memset(hA,0,sizeof(hA));
  TList *Lx;  TLine *ln;
  TH1 *h;
  char txt[1000], txt0[100];
  int nCase=12;

  hA[0]=h=new TH1F(core+"StatEve",core+" event count",nCase,0,nCase);
  h->GetXaxis()->SetTitleOffset(0.4);  h->GetXaxis()->SetLabelSize(0.06);  h->GetXaxis()->SetTitleSize(0.05); h->SetMinimum(0.8);
  h->SetLineColor(kBlue);h->SetLineWidth(2);
  h->SetMarkerSize(2);//<-- large text

  const char *key[]={"inp","badBx48","noZ","BG1","BG2","Wcut","eta","W25","Qlow","Qhigh","Q +","Q -"};
  for(int i=0;i<12;i++) h->Fill(key[i],0.); // preset the order of keys
 

  hA[1]=new TH1F(core+"bX48","Rate vs. raw bx48; bXing= raw bx48",128,-0.5,127.5);
  hA[2]=new TH1F(core+"bX7","Rate vs. raw bx7; bXing= raw bx7",128,-0.5,127.5);

  hA[3]=new TH1F(core+"bX48c","Rate vs. STAR IP bXing(bx48); bXing= bx48+offset",128,-0.5,127.5);
  hA[4]=new TH1F(core+"bX7c","Rate vs. STAR IP bXing(bx7); bXing= bx7+offset",128,-0.5,127.5);

  hA[5]=new TH2F(core+"s4mon","all L2W & vertex ; bXing at STAR (7bit); spin4 from DB",128,-0.5,127.5,32,-0.5,15.5);


  hA[6]=new TH1F(core+"Y0","BG1: L2W-BHT3-rnd & vertex OK & low ET; spin4 ",16,-0.5,15.5);
  hA[7]=new TH1F(core+"Y1","BG2: vertex & ET<20 &  ET 2x2 << 4x4 ; spin4 ",16,-0.5,15.5);

  hA[8]=h=new TH1F(core+"QpT","reco Q/PT,W ET>25 GeV; reco Q/PT  (1/GeV)",100,-0.099,0.099);
  float highCut=par_QPThighA - (par_QPThighET1-par_QPThighET0)*par_QPThighB; 
    
  if(par_QPTlow>0) { // abaility to skip all Q/PT cuts
    Lx=h->GetListOfFunctions();
    ln=new TLine(par_QPTlow,0,par_QPTlow,1e6);  ln->SetLineColor(kRed);  Lx->Add(ln);
    ln=new TLine(-par_QPTlow,0,-par_QPTlow,1e6);  ln->SetLineColor(kRed);  Lx->Add(ln);
    float avrC=(par_QPThighA+highCut)/2.; 
    ln=new TLine(-avrC,0,-avrC,1e6);  ln->SetLineColor(kRed);  Lx->Add(ln);
    ln=new TLine(avrC,0,avrC,1e6);  ln->SetLineColor(kRed);  Lx->Add(ln);
  }

  hA[9]=h=new TH2F(core+"QpT2","TPC PRIM  Q/PT ; 2x2 cluster ET (GeV); Q/PT  (1/GeV)",100,0.,100.,100,-0.1,0.1);
    Lx=h->GetListOfFunctions();
    ln=new TLine(0,0,100,0);  ln->SetLineColor(kBlue);  Lx->Add(ln);

    if(par_QPTlow>0) { // abaility to skip all Q/PT cuts
    ln=new TLine(0,par_QPTlow,100,par_QPTlow);  ln->SetLineColor(kRed);  Lx->Add(ln);  
    ln=new TLine(0,-par_QPTlow,100,-par_QPTlow);  ln->SetLineColor(kRed);  Lx->Add(ln);  
    ln=new TLine(25,-0.1, 25,0.1);  ln->SetLineColor(kRed);  Lx->Add(ln);
    ln=new TLine(par_QPThighET0,par_QPThighA,par_QPThighET1,highCut);  ln->SetLineColor(kRed);  Lx->Add(ln);
    ln=new TLine(par_QPThighET0,-par_QPThighA,par_QPThighET1,-highCut);  ln->SetLineColor(kRed);  Lx->Add(ln);
  }

  //use 10-19
  char cPM[2]={'P','N'}; // Positive, Negative
  char iCol[2]={46,9};
  for(int ipn=0;ipn<2;ipn++){ 
    TH1 *h;
    
    //.... J-peak 
    sprintf(txt0,"ET_%c",cPM[ipn]);
    sprintf(txt,"Final W, charge=%c ; 2x2 ET (GeV) ",cPM[ipn]);
    hA[10+ipn]=h=new TH1F(core+txt0,txt, 100,1,101); // shifted by 1 for nicer Rebin
    h->SetFillColor(iCol[ipn]);

    //.... 1D spin sorting         
    sprintf(txt0,"Y2_%c",cPM[ipn]);
    sprintf(txt,"Final W Q=%c, 2x2 ET=[25,50]GeV ; spin4  ",cPM[ipn]);
    hA[12+ipn]=h=new TH1F(core+txt0,txt,16,-0.5,15.5);
    h->SetFillColor(iCol[ipn]);

    sprintf(txt0,"Y3_%c",cPM[ipn]);
    sprintf(txt,"Final W Q=%c, 2x2 ET=[32,44]GeV ; spin4  ",cPM[ipn]);
    hA[14+ipn]=h=new TH1F(core+txt0,txt,16,-0.5,15.5);
    h->SetFillColor(iCol[ipn]);

    sprintf(txt0,"Y4_%c",cPM[ipn]);
    sprintf(txt,"Final QCD Q=%c, 2x2 ET=[15,20]GeV ; spin4  ",cPM[ipn]);
    hA[16+ipn]=h=new TH1F(core+txt0,txt,16,-0.5,15.5);
    h->SetFillColor(iCol[ipn]);

    sprintf(txt0,"Y5_%c",cPM[ipn]);
    sprintf(txt,"Final 2x2 ET  Q=%c; spin4 ; 2x2 cluster ET (GeV) ",cPM[ipn]);
    hA[18+ipn]=h=new TH2F(core+txt0,txt,16,-0.5,15.5,10,0,100);
    h->SetFillColor(iCol[ipn]);
  }
  // free 20-29

  hA[30]=h=new TH1F(core+"LepEta","selecting Ws ; lepton LAB eta",100, -1.5,1.5);
  Lx=h->GetListOfFunctions();
  ln=new TLine(par_leptonEta1,0,par_leptonEta1,1e6);  ln->SetLineColor(kRed);  Lx->Add(ln);
  ln=new TLine(par_leptonEta2,0,par_leptonEta2,1e6);  ln->SetLineColor(kRed);  Lx->Add(ln);

  hA[31]=h=new TH1F(core+"LumET","Lumi monitor; 2x2 ET (GeV)",100,0.,100.);



  // add histos to the list (if provided)
  for(int i=0;i<mxHA;i++) {
    if(  hA[i]==0) continue;
    HList->Add( hA[i]);
  }
  //  HList->ls();
  LOG_INFO<<Form("%s::initHistos done1",GetName())<<endm;

}


// $Log: St2009pubSpin_histo.cxx,v $
// Revision 1.11  2010/05/01 01:31:45  balewski
// added W->JJ code & JES calibration
//
// Revision 1.10  2010/03/22 16:11:42  balewski
// better computation of AL(QCD)
//
// Revision 1.9  2010/03/20 19:19:05  balewski
// added ability to drop Q/PT cut for spin analysis
//
// Revision 1.8  2010/03/18 18:46:40  balewski
// simplified sPtBalance calculation
//
// Revision 1.7  2010/03/14 22:50:31  balewski
// *** empty log message ***
//
// Revision 1.6  2010/02/04 03:48:12  balewski
// add ET for lumi monitor
//
// Revision 1.5  2010/01/30 02:02:54  balewski
// manore tunes
//
// Revision 1.4  2010/01/28 20:10:06  balewski
// added eta dependent spin sorting
//
// Revision 1.3  2010/01/28 03:42:55  balewski
// cleanup
//
// Revision 1.2  2010/01/27 22:12:25  balewski
// spin code matched to x-section code
//
// Revision 1.1  2009/11/23 23:00:18  balewski
// code moved spin-pool
//
