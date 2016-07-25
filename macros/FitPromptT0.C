/*
   root.exe TpcPromptHitsUU2012.root FitPromptT0.C+
*/

#if !defined(__CINT__)
// code that should be seen ONLY by the compiler
#else
#if !defined(__CINT__) || defined(__MAKECINT__)
// code that should be seen by the compiler AND rootcint
#else
// code that should always be seen
#endif
#endif
//________________________________________________________________________________
#if !defined(__CINT__) || defined(__MAKECINT__)
#include "Riostream.h"
#include "Ask.h"
#include "TH1.h"
#include "TH2.h"
#include "TH3.h"
#include "TStyle.h"
#include "TF1.h"
#include "TFile.h"
#include "TNtuple.h"
#include "TROOT.h"
#include "TClass.h"
#include "TSystem.h"
#include "TClassTable.h"
#include "tables/St_tpcPadrowT0_Table.h"
#endif
void FitPromptT0() {
  TNtuple* TpcHit = (TNtuple *) gDirectory->Get("TpcHit");
  if (! TpcHit) return;
  TString Out(gSystem->BaseName(gDirectory->GetName()));
  Out.ReplaceAll(".root",".Fit.root");
  TFile *fOut = new TFile(Out,"recreate");
  TH3D *TB = new TH3D("TB","prompt hit time bucket  versus sector row",45,0.5,45.5,24,0.5,24.5,100,0,10);
  TpcHit->Draw("timebucket:sector:row >> TB","","goff");
  cout << "get TB with " << TB->GetEntries() << " entries" << endl;
  Int_t nx = TB->GetXaxis()->GetNbins(); // row
  Int_t ny = TB->GetYaxis()->GetNbins(); // sec 
  struct BPoint_t {
    Float_t sec, row, mu, dmu, sigma, dsigma, chisq, NDF;
  };
  BPoint_t B;
  TNtuple *FitP = new TNtuple("FitP","prompt hit time bucket  versus sector row fit","sec:row:mu:dmu:sigma:dsigma:chisq:NDF");
  for (Int_t j = 1; j <= ny; j++) {
    for (Int_t i = 1; i <= nx; i++) {
      TH1 *proj = TB->ProjectionZ(Form("sec_%02i_row_%02i",j,i),i,i,j,j);
      if (proj->GetEntries() < 100) continue;
      proj->Fit("gaus");
      TF1 *gaus = proj->GetFunction("gaus");
      if (! gaus) continue;
      B.sec = j;
      B.row = i;
      B.mu  = gaus->GetParameter(1);
      B.dmu = gaus->GetParError(1);
      B.sigma  = gaus->GetParameter(2);
      B.dsigma = gaus->GetParError(2);
      B.chisq  = gaus->GetChisquare();
      B.NDF    = gaus->GetNDF();
      FitP->Fill(&B.sec);
      if (! gROOT->IsBatch() && Ask()) return;
    }
  }
  fOut->Write();
}
//________________________________________________________________________________
Double_t MuRow(Double_t mu, Double_t row) { // y2012 UU 200, day 131
/*
  http://www.star.bnl.gov/public/tpc/hard/tpcrings/page7.html
The anode wire position | pad plane postion | Z_offset 2004-10-24 | Gating Grid | Ground wires
Inner:    209.5         | 209.7             | 1.12                | 208.7       | 209.3
Outer:    209.7         | 210.1             | 1.62
TpcHit->Draw("abs(z):timebucket","row<=13&&timebucket<5","prof")
Inner:      z = 211.81  -0.582055*timebucket => timebucket0 = (211.81-209.5)/0.582055  => 3.9687
TpcHit->Draw("abs(z):timebucket>>ZO","row>13&&timebucket<5","prof")
Outer:      z = 212.285 -0.584628*timebucket => timebucket0 = (212.285-209.7)/0.584628 => 4.4216
 */

  if (row < 13.5) { 
    //    mu -= 2.62979e+00 + 6.27199e-03*row;
    mu -= 3.9687;
  } else {
    //    mu -= 3.16062e+00 + 1.18504e-03*row;
    mu -= 4.4216;
  }
  return mu;
}
//________________________________________________________________________________
void MaketpcPadrowT0() {
  TNtuple *FitP = (TNtuple *) gDirectory->Get("FitP");
  if (! FitP) return;
  if (gClassTable->GetID("St_TpcSecRowCor") < 0) gSystem->Load("libStDb_Tables");
  FitP->Draw("row:sec>>Mu(24,0.5,24.5,45,0.5,45.5)","-MuRow(mu,row)/9.393","colz");
  TH2 *Mu = (TH2*) gDirectory->Get("Mu");
  if (! Mu) return;
  St_tpcPadrowT0 *T0 = new St_tpcPadrowT0("tpcPadrowT0",24);
  tpcPadrowT0_st row;
  for (Int_t i = 1; i <= 24; i++) {
    memset(&row, 0, T0->GetRowSize());
    for (Int_t j = 1; j <= 45; j++) {
      row.T0[j-1] = Mu->GetBinContent(i,j);
    }
    T0->AddAt(&row);
  }
  T0->Print(0,24);
  TFile *fOut = new TFile("tpcPadrowT0.20120202.000102.root","recreate");
  T0->Write();
}
