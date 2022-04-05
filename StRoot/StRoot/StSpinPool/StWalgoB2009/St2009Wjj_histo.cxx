// $Id: St2009Wjj_histo.cxx,v 1.4 2011/06/17 17:36:12 smirnovd Exp $
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
  // TList *Lx;  TLine *ln;
  TH1 *h;
  // char txt[1000], txt0[100];
  int nCase=13;

  hA[0]=h=new TH1F(core+"StatEve",core+" event count",nCase,0,nCase);
  h->GetXaxis()->SetTitleOffset(0.4);  h->GetXaxis()->SetLabelSize(0.06);  h->GetXaxis()->SetTitleSize(0.05); h->SetMinimum(0.8);
  h->SetLineColor(kBlue);h->SetLineWidth(2);
  h->SetMarkerSize(2);//<-- large text

  const char *key[]={"inp","trig","vert","anyJ","mulJ","J1","J2","J3","DjPt","DjEta", "DjPz","eta1+2","badBx48"};
  for(int i=0;i<13;i++) h->Fill(key[i],0.); // preset the order of keys
 

  hA[1]=0;// free
  hA[2]=new TH1F(core+"bX7","Rate vs. raw bx7; bXing= raw bx7",128,-0.5,127.5);

  hA[3]=0;//free
  hA[4]=new TH1F(core+"bX7c","Rate vs. STAR IP bXing(bx7); bXing= bx7+offset",128,-0.5,127.5);

  hA[5]=new TH2F(core+"s4mon","all L2W & vertex ; bXing at STAR (7bit); spin4 from DB",128,-0.5,127.5,32,-0.5,15.5);

  hA[6]=new TH2F(core+"_3J","3 jet events (PT>5Gev);  |phi1-phi3| (rad);  |phi2-phi3| (rad)",50,0,3.15,50,0,3.15);

  //  float etaX=2.5;
  for(int j=1;j<=2;j++) { // use h10+h11
    hA[10+j-1]=h=new TH2F(core+Form("_J%d",j),Form("jet%d ; PT_%d (GeV); #eta_%d ",j,j,j),50,0,100,20,-2.5,2.5);
  }


  // 12 free
  hA[13]=new TH1F(core+"phi12","di-jet opening angle; phi(1-2) (deg)",120,-90,270.);
  hA[14]=new TH1F(core+"_K1","di-jet ; invM (GeV)",70,0,210);
  hA[15]=new TH2F(core+"_K2","di-jet ; PZ GeV/c; pT GeV/c  ",30,-120,120,20,0,40);
  hA[16]=new TH2F(core+"_K3","di-jet ; invM (GeV); pT GeV/c",60,0,180,50,0,50);
  hA[17]=new TH2F(core+"_K4","di-jet E vs. E; jet-1 E (GeV); jet-2 E (GeV)",25,0,100,25,0,100);
  hA[18]=new TH2F(core+"_K5","di-jet PT vs. PT; jet-1 PT (GeV/c); jet-2 PT (GeV/c)",50,0,100,50,0,100);
  hA[19]=new TH2F(core+"_K6","di-jet eta vs. eta; #eta_1 ; #eta_2 " ,30,-3.,3.,30,-3.,3.);


  hA[20]=h=new TH2F(core+"_spinM","di-jet spin sorting; invM (GeV); spin4;",42,0,210,16,-0.5,15.5);
      

  // add histos to the list (if provided)
  for(int i=0;i<mxHA;i++) {
    if(  hA[i]==0) continue;
    HList->Add( hA[i]);
  }
  //  HList->ls();
  LOG_INFO<<Form("%s::initHistos done1",GetName())<<endm;

}


// $Log: St2009Wjj_histo.cxx,v $
// Revision 1.4  2011/06/17 17:36:12  smirnovd
// *** empty log message ***
//
// Revision 1.3  2010/05/03 17:24:37  balewski
// added spin sorting of di-jets
//
// Revision 1.2  2010/05/01 01:31:44  balewski
// added W->JJ code & JES calibration
//
// Revision 1.1  2010/04/16 01:04:43  balewski
// start
//

#if 0 // trash

    Lx=h->GetListOfFunctions();
    ln=new TLine(par_jetPtLow,-etaX,par_jetPtLow,etaX);  ln->SetLineColor(kRed);  Lx->Add(ln);
    ln=new TLine(par_jetPtHigh,-etaX,par_jetPtHigh,etaX);  ln->SetLineColor(kRed);  Lx->Add(ln);
    ln=new TLine(0,par_jetEtaLow,100,par_jetEtaLow);  ln->SetLineColor(kRed);  Lx->Add(ln);
    ln=new TLine(0,par_jetEtaHigh,100,par_jetEtaHigh);  ln->SetLineColor(kRed);  Lx->Add(ln);


  hA[12]=h=new TH2F(core+"_DJ","di-jet ; PT (GeV); eta ",25,0,50,20,-4.,4.);
  Lx=h->GetListOfFunctions();
  ln=new TLine(par_djPtLow,-etaX*2,par_djPtLow,etaX*2);  ln->SetLineColor(kRed);  Lx->Add(ln);
  ln=new TLine(par_djPtHigh,-etaX*2,par_djPtHigh,etaX*2);  ln->SetLineColor(kRed);  Lx->Add(ln);
  ln=new TLine(0,par_djEtaMin,100,par_djEtaMin);  ln->SetLineColor(kRed);  Lx->Add(ln);
  ln=new TLine(0,-par_djEtaMin,100,-par_djEtaMin);  ln->SetLineColor(kRed);  Lx->Add(ln);
  

  hA[13]=h=new TH2F(core+"_K2","di-jet ; invM ( GeV); |eta1+eta2| ",50,0,150,20,0,3);
  Lx=h->GetListOfFunctions();
  ln=new TLine(0,par_etaSumLow,150,par_etaSumLow);  ln->SetLineColor(kRed);  Lx->Add(ln);
  ln=new TLine(0,par_etaSumHigh,150,par_etaSumHigh);  ln->SetLineColor(kRed);  Lx->Add(ln);

  hA[20]=h=new TH2F(core+"_C0","di-jet ; PZ GeV/c;  eta1+eta2 ",30,-120,120,20,-3,3);


  hA[21]=new TH2F(core+"_C1","di-jet vs. all jets (nJ=3...); PZ all jets GeV/c; di-jet eta ",30,-100,100,20,-3,3);

  hA[23]=0;//new TH2F(core+"_C3","Jet 1; ET_1 (GeV); #eta_1",30,0,100,30,-3,3);
 
 Lx=h->GetListOfFunctions();
  ln=new TLine(0,par_djPzLow,150,par_djPzLow);  ln->SetLineColor(kRed);  Lx->Add(ln);
  ln=new TLine(0,par_djPzHigh,150,par_djPzHigh);  ln->SetLineColor(kRed);  Lx->Add(ln);

#endif
