#include "TROOT.h"
#include "TF1.h"
TF1 *pol5F = 0;
//________________________________________________________________________________
void InitPar() {
  pol5F = (TF1 *) gROOT->GetListOfFunctions()->FindObject("pol5");
  if (! pol5F) {
    TF1::InitStandardFunctions();
    pol5F = (TF1 *) gROOT->GetListOfFunctions()->FindObject("pol5");
  }
}
//________________________________________________________________________________
Double_t muPar(Double_t x, Double_t tCutL10 = 5) { // log_10{Tcut[eV]}
  if (! pol5F) InitPar();
  // tChain->Draw("mu:x>>muP(20,2.5,10.5)","i&&dmu<2e-2&&dsigma<2e-2&&da0<4&&x>2.5&&a0>0","prof")
  //  muP->Fit("pol5")
  //  Double_t pars[6] = {  -0.99981,     1.2513,   -0.38066,   0.059075, -0.0043761, 0.00012296}; // 100 keV
  //  Double_t pars[6] = {   -1.1105,     1.2167,   -0.35485,     0.0527, -0.0037333, 0.00010016};
  Double_t pars[6] = {  -0.89898,      1.016,   -0.28193,   0.040223, -0.0027295, 6.9597e-05};
  return pol5F->EvalPar(&x, pars);
}
//________________________________________________________________________________
Double_t sigmaPar(Double_t x, Double_t tCutL10 = 5) { // log_10{Tcut[eV]}
  if (! pol5F) InitPar();
  // tChain->Draw("sigma:x>>sigmaP(20,2.5,10.5)","i&&dmu<2e-2&&dsigma<2e-2&&da0<4&&a0>0","prof")
  //  Double_t pars[6] = {    1.6935,   -0.56485,   0.059062, 0.00081105, -0.00046914, 1.9862e-05};
  Double_t pars[6] = {    1.9662,   -0.74794,    0.11477, -0.0082309, 0.00027025, -3.7396e-06};
  return pol5F->EvalPar(&x, pars);
}
//________________________________________________________________________________
Double_t a0Par(Double_t x, Double_t tCutL10 = 5) { // log_10{Tcut[eV]}
  if (! pol5F) InitPar();
  // tChain->Draw("a0:x>>a0P(20,2.5,10.5)","i&&dmu<2e-2&&dsigma<2e-2&&da0<4&&a0>0","prof")
  //  Double_t pars[6] = {    5.6594,    -3.6107,     1.1259,   -0.18641,   0.015486, -0.0004892};
  //  tChain->Draw("a0-a0Par(x):x>>a0PC(20,2.5,10.5)","i&&dmu<2e-2&&dsigma<2e-2&&da0<4&&a0>0","prof")
  // FitP->Draw("a0:x>>a0P(20,2.5,10.5)","i&&dmu<2e-2&&dsigma<2e-2&&da0<4&&a0<4","prof")
  Double_t pars[6] = {    9.1532,    -6.0693,      1.783,   -0.26302,   0.018738, -0.00049388};
  return pol5F->EvalPar(&x, pars);
}
