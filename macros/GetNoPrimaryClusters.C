#include "TFile.h"
#include "TH1.h"
#include "TMath.h"
#include "TDirectory.h"
Double_t betaGamma(Double_t px, Double_t py, Double_t pz, Double_t mass = 0.13956995) {
  return TMath::Sqrt(px*px + py*py + pz*pz)/mass;
}
Double_t GetNoPrimaryClusters(Double_t px, Double_t py, Double_t pz, Double_t mass = 0.13956995, Int_t charge = 1) {
  static TH1D *mdNdx = 0;
  TDirectory *fsave = gDirectory;
  if (! mdNdx) {
    TFile *_file0 = TFile::Open("$STAR/StarDb/dEdxModel/dNdx_Bichsel.root");
    mdNdx = (TH1D *)  _file0->Get("dNdx");
    gDirectory = fsave;
  }
  Double_t bg = betaGamma(px,py,pz,mass);
  Double_t beta = bg/TMath::Sqrt(1.0 + bg*bg);
  Double_t dNdx = mdNdx->Interpolate(bg);
  Double_t Q_eff = TMath::Abs(charge);
  if (Q_eff > 1)   {
    // Effective charge from GEANT ghion.F
    Double_t w1 = 1.034 - 0.1777*TMath::Exp(-0.08114*Q_eff);
    Double_t w2 = beta*TMath::Power(Q_eff,-2./3.);
    Double_t w3 = 121.4139*w2 + 0.0378*TMath::Sin(190.7165*w2);
    Q_eff      *= 1. -w1*TMath::Exp(-w3);
  }
  return Q_eff*Q_eff*dNdx;
}
