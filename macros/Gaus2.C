#include "TMath.h"
#include "TF1.h"
Double_t gaus2(Double_t *x, Double_t *p) {
  Double_t NormL = p[0];
  Double_t mu    = p[1];
  Double_t muP   = mu + p[4];
  Double_t sigma = p[2];
  Double_t sigmaP = TMath::Sqrt(sigma*sigma + 0.101741*0.101741);
  Double_t phi   = p[3];
  Double_t frac = TMath::Sin(phi);
  frac *= frac;
  return TMath::Exp(NormL)*((1 - frac)*TMath::Gaus(x[0],mu ,sigma ,kTRUE) + 
			    frac      *TMath::Gaus(x[0],muP,sigmaP,kTRUE)); 
}
//________________________________________________________________________________
Double_t gausp(Double_t *x, Double_t *p) {
  Double_t NormL = p[0];
  Double_t mu    = p[1];
  Double_t muP   = mu + p[4];
  Double_t sigma = p[2];
  Double_t phi   = p[3];
  return TMath::Exp(NormL)*TMath::Gaus(x[0],mu ,sigma ,kTRUE) + 
    p[4] + x[0]*(p[5] + x[0]*p[6]);
}
//________________________________________________________________________________
TF1 *Gaus2() {
  TF1 *f = new TF1("Gaus2",gaus2,-3,3,5);
  f->SetParName(0,"NormL"); f->SetParLimits(0,-10.,10.);
  f->SetParName(1,"mu");    f->SetParLimits(1,-0.5,0.5);
  f->SetParName(2,"sigma"); f->SetParLimits(2, 0.2,0.5);
  f->SetParName(3,"phiP");  f->SetParLimits(3, 0.0,TMath::Pi()/4);
  f->SetParName(4,"muP");
  f->SetParameters(10,0,0.3,0.1,1.315);
  f->FixParameter(4,1.425);
  return f;
}
//________________________________________________________________________________
TF1 *GausP() {
  TF1 *f = new TF1("GausP",gausp,0.45,0.55,6);
  f->SetParName(0,"NormL"); f->SetParLimits(0,-10.,50.);
  f->SetParName(1,"mu");    f->SetParLimits(1,0.45,0.55);
  f->SetParName(2,"sigma"); f->SetParLimits(2, 0.001,0.050);
  f->SetParName(3,"#alpha_1");  
  f->SetParName(4,"#alpha_2");  
  f->SetParName(5,"#alpha_3");  
  f->SetParameters(10,0.4977,0.005,0.0,0.0,0.0);
  return f;
}
