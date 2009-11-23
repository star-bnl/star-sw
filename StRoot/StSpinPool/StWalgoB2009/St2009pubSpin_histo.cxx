// $Id: St2009pubSpin_histo.cxx,v 1.1 2009/11/23 23:00:18 balewski Exp $
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
  core="spin"; // prefix added to every histo name, to allow for multipl maker saving histos in the same root file


  //...... data histograms
  memset(hA,0,sizeof(hA));
  // TList *Lx;  TLine *ln;
  TH1 *h;
  char txt[1000], txt0[100];
  int nCase=5;

  hA[0]=h=new TH1F(core+"StatEve",core+" event count",nCase,0,nCase);
  h->GetXaxis()->SetTitleOffset(0.4);  h->GetXaxis()->SetLabelSize(0.06);  h->GetXaxis()->SetTitleSize(0.05); h->SetMinimum(0.8);
  h->SetLineColor(kBlue);h->SetLineWidth(2);
 
  char key[][200]={"inp","noAwayET","hasAwayET","badBx48"};
  for(int i=0;i<3;i++) h->Fill(key[i],0.); // preset the order of keys
  

  hA[1]=new TH1F(core+"bX48","Rate vs. raw bx48; bXing= raw bx48",128,-0.5,127.5);
  hA[2]=new TH1F(core+"bX7","Rate vs. raw bx7; bXing= raw bx7",128,-0.5,127.5);

  hA[3]=new TH1F(core+"bX48c","Rate vs. STAR IP bXing(bx48); bXing= bx48+offset",128,-0.5,127.5);
  hA[4]=new TH1F(core+"bX7c","Rate vs. STAR IP bXing(bx7); bXing= bx7+offset",128,-0.5,127.5);

  hA[5]=new TH2F(core+"s4mon","spin4 L2W-ET>13 or Rnd & vertex OK ; bXing at STAR (7bit); spin4 from DB",128,-0.5,127.5,32,-0.5,15.5);


  hA[6]=new TH1F(core+"Y0","input: L2W-BHT3-rnd & vertex OK & low ET; spin4 ",16,-0.5,15.5);
  hA[7]=new TH1F(core+"Y1","input: L2W-ET>13 & vertex OK & track & low ET ; spin4 ",16,-0.5,15.5);

  //free 8-9

  //use 10-13
  char cPM[2]={'P','N'}; // Positive, Negative
  char iCol[2]={46,9};
  for(int ipn=0;ipn<2;ipn++){ 
    TH1 *h;

    //.... 1D spin sorting         
    sprintf(txt0,"Y2_%c",cPM[ipn]);
    sprintf(txt,"Final 2x2 ET=[30,50]GeV Q=%c; spin4  ",cPM[ipn]);
    hA[10+ipn]=h=new TH1F(core+txt0,txt,16,-0.5,15.5);
    h->SetFillColor(iCol[ipn]);

    sprintf(txt0,"Y3_%c",cPM[ipn]);
    sprintf(txt,"Final 2x2 ET=[15,20]GeV Q=%c; spin4  ",cPM[ipn]);
    hA[12+ipn]=h=new TH1F(core+txt0,txt,16,-0.5,15.5);
    h->SetFillColor(iCol[ipn]);

    sprintf(txt0,"Y4_%c",cPM[ipn]);
    sprintf(txt,"Final 2x2 ET  Q=%c; spin4 ; 2x2 cluster ET (GeV) ",cPM[ipn]);
    hA[14+ipn]=h=new TH2F(core+txt0,txt,16,-0.5,15.5,10,0,100);
    h->SetFillColor(iCol[ipn]);

}

  // add histos to the list (if provided)
  for(int i=0;i<mxHA;i++) {
    if(  hA[i]==0) continue;
    HList->Add( hA[i]);
  }
  //  HList->ls();
  LOG_INFO<<Form("%s::initHistos done1",GetName())<<endm;

}


// $Log: St2009pubSpin_histo.cxx,v $
// Revision 1.1  2009/11/23 23:00:18  balewski
// code moved spin-pool
//
