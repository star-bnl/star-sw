#include "TF1.h"
#include "TMath.h"
//________________________________________________________________________________
Double_t sat(Double_t *x, Double_t *p) {
  Double_t aRC = TMath::Exp(x[0]);
  Double_t t = (aRC + p[3]*(aRC+p[4]*aRC))/p[0];
  t = TMath::Max(-0.9999, TMath::Min(0.9999, t));
  Double_t at = p[0]*TMath::ATanH(t);
  Double_t aMC = p[2]*at + p[1];
  return TMath::Log(aMC/aRC);
}
//________________________________________________________________________________
TF1 *Sat() {
  static TF1 *f = 0;
  if (! f) {
    f = new TF1("Sat",sat,3,10,5);
    f->SetParNames("maxAdc","offset","scale","p2","p3");
    f->SetParameters(1e5,10,1,5,0);
  }
  return f;
}
//________________________________________________________________________________
Double_t tanhFunc(Double_t *x, Double_t *p) {
  return p[0] + p[1]*TMath::TanH(p[2]*(x[0] - p[3]));
}
//________________________________________________________________________________
TF1 *FTanH() {
  static TF1 *f = 0;
  if (! f) {
    f = new TF1("FTanH",tanhFunc,-5,4,4);
    f->SetParNames("offset","slope","scale","shift");
    f->SetParameters(0,1,1,0);
  }
  return f;
}
