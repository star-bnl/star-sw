#include "TMath.h"
//________________________________________________________________________________
Double_t pT(Double_t px, Double_t py) {
  return TMath::Sqrt(px*px + py*py);
}//________________________________________________________________________________
Double_t eta(Double_t px, Double_t py, Double_t pz) {
  Double_t Theta = TMath::ATan2(pT(px,py),pz);
  return - TMath::Log(TMath::Tan(Theta/2.));
}
//________________________________________________________________________________
Double_t mom(Double_t px, Double_t py, Double_t pz) {
  return TMath::Sqrt(px*px + py*py + pz*pz);
}
