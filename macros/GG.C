/* 
   Skew normal distribution https://en.wikipedia.org/wiki/Skew_normal_distribution
*/
#include "TMath.h"
#include "TF1.h"
//________________________________________________________________________________
Double_t ggaus(Double_t *x, Double_t *p) {
  Double_t X = x[0];
  Double_t NormL = p[0];
  Double_t mu = p[1];
  Double_t sigma = p[2];
  Double_t alpha = p[3];
  Double_t t = (X  - mu)/sigma;
  Double_t v = t/TMath::Sqrt2();
  return TMath::Exp(NormL)*TMath::Gaus(t,0,1,kTRUE)/sigma*(1. + TMath::Erf(alpha*v));
}
//________________________________________________________________________________
TF1 *GG() {
  static TF1 *f = 0;
  if (! f) {
    f = new TF1("GG",ggaus,-5,5,4);
    f->SetParameters(0,0,1,1);
    f->SetParNames("norl","mu","sigma","alpha");
  }
  return f;
}
