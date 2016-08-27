/*
  TH1D *h = 0;
  Int_t ch  = 0, sec = 0;
  Int_t i = 8*(sec-1) + ch; h = mu->ProjectionY(Form("bin%i",i), i, i); FitTH1R(h);
 */

#if !defined(__CINT__)
// code that should be seen ONLY by the compiler
#else
#if !defined(__CINT__) || defined(__MAKECINT__)
// code that should be seen by the compiler AND rootcint
#else
// code that should always be seen
#endif
#endif
#if ROOT_VERSION_CODE >= ROOT_VERSION(5,34,18)
//#define __USE_ROOFIT__
#endif
//________________________________________________________________________________
#if !defined(__CINT__) || defined(__MAKECINT__)
#include "Riostream.h"
#include <stdio.h>
#include "TROOT.h"
#include "TSystem.h"
#include "TMath.h"
#include "TH1.h"
#include "TH2.h"
#include "TGraphErrors.h"
#include "TF1.h"
#endif
TGraphErrors *gr = 0;
//________________________________________________________________________________
void FitTH1R(TH1 *hist, const Char_t *fitF = "pol1") {
  if (! hist) return;
  SafeDelete(gr);
  Int_t n = hist->GetNbinsX();
  Double_t *x = new Double_t[n];
  Double_t *y = new Double_t[n];
  Double_t *e = new Double_t[n];
  Int_t N = 0;
  for (Int_t i = 1; i <= n; i++) {
    if (hist->GetBinError(i) <= 0.0) continue;
    x[N] = hist->GetBinCenter(i);
    y[N] = hist->GetBinContent(i);
    e[N] = hist->GetBinError(i);
    N++;
  }
  gr = new TGraphErrors(N, x, y, 0, e);
  gr->Fit(fitF,"+rob=0.75");
  gr->SetMaximum(0.1);
  gr->SetMinimum(-.1)
  gr->Draw("axp");
  delete [] x;
  delete [] y;
  delete [] e;
}
