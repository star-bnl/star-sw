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
#include "TNtuple.h"
#endif
TGraphErrors *gr = 0;
//________________________________________________________________________________
void FitPR(TF1 *fun = 0, const Char_t *plot="dmu:mu:x", const Char_t *histN = "M", const Char_t  *cut = "i&&j&&chisq>0&&chisq<2e3&&dmu<0.02&&sigma<0.1") {
  TNtuple *FitP = (TNtuple*) gDirectory->Get("FitP");
  if (! FitP) {
    cout << "FitP is missing" << endl;
    return;
  }
  delete gr;
  gr = 0;
  //   TTree::Draw("V1:V2:V3:V4",selection);
  Int_t nFound = FitP->Draw(Form("%s >> %s",plot,histN), cut, "goff");
  Int_t nRows  = FitP->GetSelectedRows();
  cout << "Found " << nFound << "\t with selected rows " << nRows << endl;
  Double_t *x = FitP->GetV3();
  Double_t *y = FitP->GetV2();
  Double_t *e = FitP->GetV1();
  for (Int_t i = 0; i < nRows; i++) {
    cout << i << "\tx = " << x[i] << "\t" << y[i] << " +/- " << e[i] << endl;
  }
  gr = new TGraphErrors(nRows, x, y, 0, e);
  if (fun) {
    gr->Fit(fun,"+rob=0.75");
#if 0
    gr->SetMaximum(0.1);
    gr->SetMinimum(-.1);
#endif
  }
  gr->Draw("axp");
  delete [] x;
  delete [] y;
  delete [] e;
}
