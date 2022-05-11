#include "Ask.h"
void LoopOverFitP() {
  TSeqCollection *files = gROOT->GetListOfFiles();
  if (! files) return;
  TCanvas *c1 = (TCanvas *) gROOT->GetListOfCanvases()->FindObject("c1");
  if (! c1 ) c1 = new TCanvas("c1","c1",200,10,700,800);
  c1->Clear();
  c1->Divide(1,2);
  Int_t nn = files->GetSize();
  if (! nn) return;
  TIter next(files);
  TFile *f = 0;
  while ( (f = (TFile *) next()) ) { 
    f->cd();
    TString F(f->GetName());
    TNtuple *FitP = (TNtuple *) gDirectory->Get("FitP");
    if (! FitP) continue;
    c1->cd(1);
    FitP->Draw("mu:x","i&&j");
    c1->cd(2);
    FitP->Draw("mu:y","i&&j");
    c1->Update();
    cout << f->GetName() << endl;
    if (Ask()) break;
  }
}
