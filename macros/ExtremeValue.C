#include "TMath.h"
Double_t ExtremeValue(Double_t *x, Double_t *p) {
  Double_t val = 0;
  Double_t mu = p[1];
  Double_t sigma = p[2];
  if (TMath::Abs(sigma) < 1e-7) return val;
  Double_t dev = (mu - x[0])/sigma;
  val = p[0]/sigma*TMath::Exp(dev - TMath::Exp(dev));
  if (p[3] > 0) val += p[0]*TMath::Exp(p[3])*TMath::Gaus(x[0],p[4],p[5],kTRUE);
  return val;
};
