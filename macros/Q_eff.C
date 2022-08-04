#include "TROOT.h"
#include "TF1.h"
#include "TMath.h"
Double_t q_eff(Double_t *x, Double_t *p) {
  Double_t Z = p[0];
  //  Double_t beta = x[0];
  Double_t beta = TMath::Power(10.0,x[0]);
  //  if (Z > 1)   {
    // Effective charge from GEANT gthion.F
    Double_t w1 = 1.034 - 0.1777*TMath::Exp(-0.08114*Z);
    Double_t w2 = beta*TMath::Power(Z,-2./3.);
    Double_t w3 = 121.4139*w2 + 0.0378*TMath::Sin(190.7165*w2);
    Double_t Q_eff       = 1. -w1*TMath::Exp(-w3); // /Z
    //  }
    return Q_eff;
}
//_________________________________________________________________________
TF1 *Q_eff(Int_t Z = 1) {
  TF1 *f = (TF1 *) gROOT->GetListOfFunctions()->FindObject(Form("Q_effZ%i",Z));
  if (! f) {
    //    f = new TF1(Form("Q_effZ%i",Z),q_eff,1e-2,1.,1);
    f = new TF1(Form("Q_effZ%i",Z),q_eff,-3,0.,1);
    f->SetParameter(0, Z);
    f->SetLineColor(Z);
  }
  return f;
}
