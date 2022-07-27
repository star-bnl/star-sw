#include "TROOT.h"
#include "TF1.h"
TF1 *pol3F = 0;
TF1 *pol1F = 0;
//________________________________________________________________________________
void InitPar() {
  pol3F = (TF1 *) gROOT->GetListOfFunctions()->FindObject("pol3");
  if (! pol3F) {
    TF1::InitStandardFunctions();
    pol3F = (TF1 *) gROOT->GetListOfFunctions()->FindObject("pol3");
  }
  pol1F = (TF1 *) gROOT->GetListOfFunctions()->FindObject("pol1");
}
//________________________________________________________________________________
Double_t muPar10MeV(Double_t x) {
  if (! pol3F) InitPar();
  // muP->Fit("pol3","er","",2.5,9)
  Double_t pars[4] = {    0.2922,    0.12944, -0.0076375, 0.00025695};
  if (x > 9) x = 9;
  return pol3F->EvalPar(&x, pars);
}
//________________________________________________________________________________
Double_t sigmaPar10MeV(Double_t x) {
  if (! pol3F) InitPar();
  // sigmaP->Fit("pol3","er","",2.5,9)
  Double_t pars[4] = {    1.5739,   -0.51993,   0.062944,  -0.002641};
  if (x > 9) x = 9;
  return pol3F->EvalPar(&x, pars);
}
//________________________________________________________________________________
Double_t a0Par10MeV(Double_t x) {
  if (! pol3F) InitPar();
  // a0P->Fit("pol3","er","",2.5,9)
  Double_t pars[4] = {    2.2465,   -0.51138,   0.043366, -0.00096301};
  if (x > 9) x = 9;
  return pol3F->EvalPar(&x, pars);
}
//________________________________________________________________________________
Double_t muPar100keV(Double_t x) {
  if (! pol3F) InitPar();
  // tChain->Draw("mu:x>>muP","(dmu<2e-2&&dsigma<2e-2&&da0<2&&a0<4)","prof")
  // muP->Fit("pol3","er","",2.5,9)
  Double_t pars[4] = {   0.16285,    0.19904,  -0.019955,  0.0009979};
  if (x > 9) x = 9;
  return pol3F->EvalPar(&x, pars);
}
//________________________________________________________________________________
Double_t sigmaPar100keV(Double_t x) {
  if (! pol3F) InitPar();
  // sigmaP->Fit("pol3","er","",2.5,9)
  Double_t pars[4] = {    1.7268,   -0.59909,   0.076116, -0.0033467};
  if (x > 9) x = 9;
  return pol3F->EvalPar(&x, pars);
}
//________________________________________________________________________________
Double_t a0Par100keV(Double_t x) {
  if (! pol3F) InitPar();
  // a0P->Fit("pol3","er","",2.5,8)
  Double_t val = 0;
  Double_t par1[2] = {   -2.8753,    0.38145};
  if (x > 8) val = pol1F->EvalPar(&x, par1);
  Double_t pars[4] = {    3.5238,    -1.2228,    0.16758, -0.0076581};
  if (x > 8) x = 8;
  val += pol3F->EvalPar(&x, pars);
  return val;
}
