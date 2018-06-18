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
void FitTH2R(TH2 *hist, TF1 *fun) {
  if (! hist) return;
  SafeDelete(gr);
  Int_t nX = hist->GetNbinsX();
  Int_t nY = hist->GetNbinsY();
  Double_t *x = new Double_t[nX*nY];
  Double_t *y = new Double_t[nX*nY];
  Double_t *e = new Double_t[nX*nY];
  Int_t N = 0;
  for (Int_t i = 1; i <= nX; i++) {
    for (Int_t j = 1; j <= nY; j++) {
      if (hist->GetBinError(i,j) <= 0.1) continue;
      x[N] = hist->GetXaxis()->GetBinCenter(i);
      y[N] = hist->GetYaxis()->GetBinCenter(j);
      e[N] = hist->GetBinError(i,j);
      N++;
    }
  }
  cout << N << " points found" << endl;
  gr = new TGraphErrors(N, x, y, 0, e);
  gr->Fit(fun,"+rob=0.75");
#if 0
  gr->SetMaximum(0.1);
  gr->SetMinimum(-.1);
#endif
  gr->Draw("axp");
  delete [] x;
  delete [] y;
  delete [] e;
}
