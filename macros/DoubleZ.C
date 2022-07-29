#include "TString.h"
#include "TMath.h"
#include "TH1.h"
TH1D *DoubleZ(TH1D *h1) {
  if (! h1) return 0;
  TH1D *h2 = new TH1D(*h1);
  h2->Reset();
  h2->SetName(Form("%sDouble",h1->GetName()));
  Int_t N = h1->GetEntries();
  for (Int_t ev = 0; ev < N; ev++) {
    Double_t z1 = h1->GetRandom();
    Double_t z2 = h1->GetRandom();
    Double_t z  = TMath::Log(TMath::Exp(z1) + TMath::Exp(z2));
    h2->Fill(z);
  }
}
