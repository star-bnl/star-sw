#include "TMath.h"
#include "TF1.h"
//________________________________________________________________________________
Double_t gmp(Double_t *x, Double_t *p) {
  Double_t normL = p[0]; // t^(gamma - 1) * exp(-t); t = (x -  x_0)/beta
  Double_t mu    = p[1]; // gamma = t_max + 1; t_max = (mu - x_0)/beta = gamma - 1
  Double_t sigma = p[2]; //
  Double_t gamma = p[3];
  Double_t grass = p[4];
  Double_t sign  = p[5];
  Double_t val   = grass;
  if (sigma > 0 && gamma > 0) {
    Double_t beta = sigma*sigma*gamma*gamma;
    Double_t x0 = mu - (gamma - 1)*beta;
    Double_t t = sign*(x[0] - x0)/beta;
    if (t > 0) { 
      val += TMath::Exp(normL)*TMath::GammaDist(t,gamma,0.,1.);
    }
  }
  return val;
}
//________________________________________________________________________________
TF1 *GMP() {
  TF1 * f = new TF1("GMP",gmp,0.5,15.5,6);
  f->SetParNames("normL", "mu", "sigma", "gamma", "grass","sign");
  f->SetParameters(4.17880, 2.86452, 0.2, 6.0, 2.1);
#if 1
  f->SetParLimits(0,0,50);
  f->SetParLimits(1,10,10);
  f->SetParLimits(2,0.1,2);
  f->SetParLimits(3,0.1,50);
  f->SetParLimits(4,0,1e3);
#endif
  f->FixParameter(5,1.);
  return f;
}
//________________________________________________________________________________
TF1 *Gamma() {
  return GMP();
}
