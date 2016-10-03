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
#include "TCanvas.h"
#include "Ask.h"
#endif
TGraphErrors *gr = 0;
//________________________________________________________________________________
TF1* FitTH1R(TH1 *hist, const Char_t *fitF = "pol1") {
  TF1* f  = 0;
  if (! hist) return f;
  SafeDelete(gr);
  f = (TF1 *) gROOT->GetListOfFunctions()->FindObject(fitF);
  if (! f) {
    TF1::InitStandardFunctions();
    f = (TF1 *) gROOT->GetListOfFunctions()->FindObject(fitF);
    if (! f) return f;
  }
  
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
  Int_t iok = gr->Fit(fitF,"+rob=0.75");
  if (iok < 0) f = 0;
  TCanvas *c1 = (TCanvas *) gROOT->GetListOfCanvases()->FindObject("c1");
  if (c1) c1->Clear();
  else    c1 = new TCanvas("c1","c1");
  gr->SetMaximum(0.1);
  gr->SetMinimum(-.1);
  gr->Draw("axp");
  c1->Update();
  delete [] x;
  delete [] y;
  delete [] e;
  if (! gROOT->IsBatch() && Ask()) return f;
  return f;
}
//________________________________________________________________________________
void MakeRow(TH2 *mu=0, const Char_t *fName="pol1") {
  if (! mu) return;
  Int_t n = mu->GetNbinsX();
  for (Int_t i = 1; i <= n; i++) {
    TH1D *proj = mu->ProjectionY(Form("R%i",i),i,i);
    TF1 *f = FitTH1R(proj,fName);
    ofstream out;
    TString fOut =  Form("row.%03i.C",i);
    TString Line;
    cout << "Create " << fOut << endl;
    out.open(fOut.Data());
    Line = Form("  memset(&row,0,tableSet->GetRowSize()); // %s",gDirectory->GetName()); cout << Line.Data() << endl;  out << Line.Data() << endl;
    Line = Form("  row.idx   = %3i;",i); cout << Line.Data() << endl;  out << Line.Data() << endl;
    Line = Form("  row.nrows = %3i;",n); cout << Line.Data() << endl;  out << Line.Data() << endl;
    //    Line = Form("  row.min =  %5.2f;",xmin); cout << Line.Data() << endl;  out << Line.Data() << endl;
    //    Line = Form("  row.max =  %5.2f;",xmax); cout << Line.Data() << endl;  out << Line.Data() << endl;
    if (f) {
      Line = Form("  row.npar       =            %2i;",f->GetNpar()); cout << Line.Data() << endl;  out << Line.Data() << endl;
      for (Int_t i = 0; i < f->GetNpar(); i++) {
	Line = Form("  row.a[%i]       = %13.7g;", i, f->GetParameter(i)); cout << Line.Data() << endl;  out << Line.Data() << endl;
      }
    }
    Line = Form("  tableSet->AddAt(&row); // row %3i",i); cout << Line.Data() << endl;  out << Line.Data() << endl;  
  }
}
