#include "TMath.h"
Double_t PionFrac(Double_t a0,Double_t a1,Double_t a2,Double_t a3) {
  //             (         pi,P,K,e
  Double_t pars[4] = {a0, a1, a2, a3};
  Double_t frac[4];
  frac[0] = 1;
  for (Int_t i = 1; i <= 4; i++) {
    frac[i] = TMath::Sin(pars[i-1]);
    frac[i] *= frac[i];
    frac[0] -= frac[i];
  }
  return frac[0];
}
