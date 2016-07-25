#include "TMath.h"
Double_t Poisson(Double_t *x, Double_t *par) {
  if (par[1] <= 0) return 0;
  return par[0]*TMath::Power(par[1],x[0])*TMath::Exp(-par[1])/TMath::Gamma(x[0]+1);
}
