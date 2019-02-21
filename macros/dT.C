/*
   root.exe st200*rPlots.Cut.Errors.root dT.C
*/
#include "Ask.h"
void dT() {
  Int_t NF = 0;
  TSeqCollection *files = gROOT->GetListOfFiles();
  if (! files) return;
  Int_t nn = files->GetSize();
  if (! nn) return;
  TIter next(files);
  TFile *f = 0;
  TCanvas *c1 = (TCanvas *) gROOT->GetListOfCanvases()->FindObject("c1");
  if (! c1 ) c1 = new TCanvas("c1","dT");
  c1->SetLogz(1);
  while ( (f = (TFile *) next()) ) { 
    f->cd();
    TString F(f->GetName());
    TH3 *dT = (TH3 *) gDirectory->Get("dT");
    if (! dT) continue;
    if (dT->GetEntries() < 1e3) continue;
    c1->Clear();
    dT->Project3D("zx")->Draw("colz");
    TH2 *dT_zx = (TH2*) gDirectory->Get("dT_zx");
    if (! dT_zx) continue;
    c1->Update();
    dT_zx->FitSlicesY();
    TH1 *dT_zx_1 = (TH1 *) gDirectory->Get("dT_zx_1");
    if (! dT_zx_1) continue;
    dT_zx_1->Draw("same");
    c1->Update();
    cout << F.Data() << "\tmean = " << dT_zx->GetMean(2) << "\tRMS = " << dT_zx->GetRMS(2);
    for (Int_t sector = 1; sector <= 24; sector++) {
      Double_t v = dT_zx_1->GetBinContent(sector);
      if (TMath::Abs(v) < 0.1) continue;
      Double_t dv = dT_zx_1->GetBinError(sector);
      cout << "\tsector  " << sector << "\tdT = " << v << " +/- " << dv;
    }
    cout << endl;
    if (! Ask()) continue;
  }
}
