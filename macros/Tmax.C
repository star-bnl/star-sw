#include "Names.h"
//________________________________________________________________________________
Double_t tmax(Double_t *x, Double_t *p) {
  Double_t m = Masses[0];
  Int_t h = (Int_t) p[0];
  Double_t M = Masses[h];
  Double_t mOverM = m/M;
  Double_t bgL10 = x[0];
  Double_t bg = TMath::Power(10.,bgL10);
  Double_t bg2 = bg*bg;
  Double_t gamma = TMath::Sqrt(bg2 + 1);
  return 2*m*bg2/(1. + mOverM*(2*gamma + mOverM)); 
}
//________________________________________________________________________________
void Tmax() {
  TLegend *l = new TLegend(0.5,0.6,0.8,0.9);
  TString same;
  for (Int_t h = 0; h < NHypTypes; h++) {
    TF1 *f = new TF1(HistNames[h],tmax,-1.,4.,1);
    f->SetParameter(0,h);
    f->SetLineColor(h+1);
    f->Draw(same); same = "same";
    l->AddEntry(f,f->GetName());
  }
  l->Draw();
}
