#include "Ask.h"
void dZ() {
  Int_t NF = 0;
  TSeqCollection *files = gROOT->GetListOfFiles();
  if (! files) return;
  Int_t nn = files->GetSize();
  if (! nn) return;
  TIter next(files);
  TFile *f = 0;
  TCanvas *c1 = (TCanvas *) gROOT->GetListOfCanvases()->FindObject("c1");
  if (! c1 ) c1 = new TCanvas("c1","dZ");
  while ( (f = (TFile *) next()) ) { 
    f->cd();
    TString F(f->GetName());
    TH3 *dZ = (TH3 *) gDirectory->Get("dZ");
    if (! dZ) continue;
    c1->Clear();
    dZ->Project3D("z")->Draw();
    TH1 *dZ_z = (TH1*) gDirectory->Get("dZ_z");
    if (! dZ_z) continue;
    c1->Update();
    cout << F.Data() << "\tmean = " << dZ_z->GetMean() << "\tRMS = " << dZ_z->GetRMS() << endl;
    if (! Ask()) continue;
  }
}
