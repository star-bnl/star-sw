// $Id: St2011Wlumi_histo.cxx,v 1.2 2012/09/14 21:02:29 balewski Exp $
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
  TString core="lum"; // prefix added to every histo name, to allow for multipl maker saving histos in the same root file


  //...... data histograms
  memset(hA,0,sizeof(hA));
  TH1 *h;
  int nCase=4;

  hA[0]=h=new TH1F(core+"StatEve",core+" event type",nCase,0,nCase);
  h->GetXaxis()->SetTitleOffset(0.4);  h->GetXaxis()->SetLabelSize(0.06);  h->GetXaxis()->SetTitleSize(0.05); h->SetMinimum(0.8);
  h->SetLineColor(kBlue);h->SetLineWidth(2);

  char key[][200]={"inp","ver","PT>10","B ET>1"};
  for(int i=0;i<4;i++) h->Fill(key[i],0.); // preset the order of keys
  

  hA[1]=new TH1F(core+"EleTrET","max ET of track w/ PT>10 GeV/c;barrel  2x2  ET (GeV)", 50,0,50);

  hA[2]=new TH1F(core+"Y0","input event w/ OK prim tracks; spin4 ",16,-0.5,15.5);
  hA[3]=new TH1F(core+"Y2","EleTrET [5,10] GeV; spin4 ",16,-0.5,15.5);
  hA[4]=new TH1F(core+"Y3","EleTrET [10,14] GeV; spin4 ",16,-0.5,15.5);


  // add histos to the list (if provided)
  for(int i=0;i<mxHA;i++) {
    if(  hA[i]==0) continue;
    HList->Add( hA[i]);
  }
  //  HList->ls();
  LOG_INFO<<Form("%s::initHistos done",GetName())<<endm;

}


// $Log: St2011Wlumi_histo.cxx,v $
// Revision 1.2  2012/09/14 21:02:29  balewski
// *lumi-maker re-written to accumulate alternative rel lumi monitors,
// * added spin sorting to Zs
//
// Revision 1.1  2011/02/10 20:33:24  balewski
// start
//
