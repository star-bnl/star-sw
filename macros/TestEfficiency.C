#include "TF1.h"
#include "TProfile3D.h"
#include "TRandom.h"
#include "TMath.h"
void TestEfficiency() {
  Double_t pTmin = 0, pTmax = 5;
  Double_t phiMin = 0, phiMax = TMath::TwoPi();
  Double_t etaMin = -2, etaMax = 2;
  TF1 *fpT  = new TF1("pt","1 - TMath::Exp(-x)",pTmin,pTmax);
  TF1 *fphi = new TF1("phi","TMath::Sin(x)*TMath::Sin(x)",phiMin,phiMax);
  TF1 *feta = new TF1("eta","TMath::CosH(x)",etaMin,etaMax);
  TProfile3D *prof = new TProfile3D("prof","prof",100,pTmin,pTmax,100,phiMin,phiMax,100,etaMin,etaMax);
  for (Int_t ev = 0; ev < 1000000; ev++) {
    Double_t pT = pTmin + (pTmax - pTmin)*gRandom->Rndm();
    Double_t phi = phiMin + (phiMax - phiMin)*gRandom->Rndm();
    Double_t eta = etaMin + (etaMax - etaMin)*gRandom->Rndm();
    Bool_t passed = kTRUE;
    if (fpT->Eval(pT)*fphi->Eval(phi)*feta->Eval(eta) > gRandom->Rndm()) passed = kFALSE;
    prof->Fill(pT,phi,eta,passed);
  }
}
