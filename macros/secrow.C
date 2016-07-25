//________________________________________________________________________________
#if !defined(__CINT__) || defined(__MAKECINT__)
#include "Riostream.h"
#include <stdio.h>
#include "TROOT.h"
#include "TSystem.h"
#include "TMath.h"
#include "TH1.h"
#include "TH2.h"
#include "TH3.h"
#include "TStyle.h"
#include "TF1.h"
#include "TProfile.h"
#include "TTree.h"
#include "TChain.h"
#include "TFile.h"
#include "TNtuple.h"
#include "TCanvas.h"
#include "TString.h"
#include "TLegend.h"
#endif
TCanvas *c1 = 0;
// convert 45*(sector - 1) + row => sector & row
Int_t sector(Float_t x) {Int_t i = TMath::Nint(x-1); return i/45 + 1;}
Int_t row   (Float_t x) {Int_t i = TMath::Nint(x); return i - 45*(sector(x)-1);}
Int_t socket(Float_t x) {
  static const Int_t sockets[45] = {
    /* row           1  2  3  4  5  6  7  8  9 10 */
    /* sockets */    1, 2, 3, 3, 4, 4, 5, 6, 6, 7,
    /* sockets */    8, 8,17,
    /* sockets */             9, 9, 9, 9,10,10,10,
    /* sockets */   10,11,11,11,11,12,12,12,12,13,
    /* sockets */   13,13,13,14,14,14,14,15,15,15,
    /* sockets */   15,16,16,16,16
  };
  Int_t r = row(x);
  Int_t s = 0;
  if (r >= 1 && r <=45) s = sockets[r-1];
  return s;
}
/*
  .L secrow.C++
  TIter next(gROOT->GetListOfFiles());
  TFile *f = 0;
  while ((f = (TFile *) next())) {f->cd(); BadSecRow();}
*/
#define __SOCKETS__
//#define __R__
void BadSecRow() {
  gStyle->SetOptStat(0);
  TString Current(gDirectory->GetName());
  Current.ReplaceAll(".root","");
  TNtuple *FitP = (TNtuple *) gDirectory->Get("FitP");
  if (! FitP ) return;
  TCanvas *c1 = new TCanvas(Current,Current);
#ifndef __SOCKETS__ 
#ifdef __R__
  FitP->Draw("sector(x):row(x)>>BadV(45,0.5,45.5,24,0.5,24.5)","abs(mu)>0.2&&i&&j&&row(x)>0");
#else
  FitP->Draw("row(x):sector(x)>>BadV(24,0.5,24.5,45,0.5,45.5)","abs(mu)>0.2&&i&&j&&row(x)>0","colz");
#endif
#else /* __SOCKETS__ */
  FitP->Draw("socket(x):sector(x)>>BadV(24,0.5,24.5,17,0.5,17.5)","abs(mu)>0.2&&i&&j","colz");
#endif /* __SOCKETS__ */
  TH2 *BadV = (TH2 *) gDirectory->Get("BadV");
  if (! BadV) return;
  BadV->SetTitle(Current);

#ifndef __SOCKETS__ 
#ifdef __R__
  BadV->SetXTitle("row");
  BadV->SetYTitle("sector");
  Current += "R.png";
#else
  BadV->SetYTitle("row");
  BadV->SetXTitle("sector");
  Current += ".png";
#endif
#else /* __SOCKETS__ */
  BadV->SetYTitle("socket");
  BadV->SetXTitle("sector");
  Current += "S.png";
#endif /* __SOCKETS__ */

  BadV->Draw("colz");
  c1->Update();
  TVirtualX::Instance()->WritePixmap(c1->GetCanvasID(),-1,-1,(Char_t *)Current.Data());
}
