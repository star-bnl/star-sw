#if !defined(__CINT__) || defined(__MAKECINT__)
#include "Riostream.h"
#include "TROOT.h"
#include "TSystem.h"
#include "TMath.h"
#include "TH1.h"
#include "TH2.h"
#include "TH3.h"
#include "TLegend.h"
#include "TObjArray.h"
#include "TCanvas.h"
#endif
#include "Ask.h"
void PiDQA(const Char_t *histN="dEdx") {
  const Int_t N = 8;
  const Char_t *dirs[N] = {"/PiDQA/gamma/e+",
			   "/PiDQA/gamma/e-",
			   "/PiDQA/Ks/pi+",
			   "/PiDQA/Ks/pi-",
			   "/PiDQA/Lambda/p",
			   "/PiDQA/Lambda/pi-",
			   "/PiDQA/Lambdab/pi+",
			   "/PiDQA/Lambdab/p-"};
#if 0
  TCanvas *c1 = (TCanvas *) gROOT->GetListOfCanvases()->FindObject("c1");
  if (c1) c1->Clear();
  else    c1 = new TCanvas("c1","c1",600,600);
  c1->SetLogz(1);
#endif
  for (Int_t i = 0; i < N; i++) {
    if (! gDirectory->cd(dirs[i])) continue;
    TCanvas *c1 = new TCanvas(Form("c%i",i),dirs[i]);
    c1->SetLogz(1);
    TString name(histN);
    TH2F *h2 = (TH2F *) gDirectory->Get(name);
    cout << "Histmogram " << dirs[i] << "/" << name.Data();
    if (! h2) {
      cout  << " has not been found" << endl;
      continue;
    }
    cout << " has been found" << endl;
    TObjArray *arr = new TObjArray(4);
    h2->FitSlicesY(0,0,-1,100,"qeg3s",arr);
    TH1D *mu = (TH1D *) arr->At(1);
    TH1D *sigma = (TH1D *) arr->At(2);
    if (mu && sigma) {
      h2->Draw("colz");
      //      c1->Update();
      mu->Draw("same");
      //      c1->Update();
      sigma->Draw("same");
      c1->Update();
    }
    if (! gROOT->IsBatch() && Ask()) return;
  }
  
}
