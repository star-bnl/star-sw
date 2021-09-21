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
#include "TGraphErrors.h"
#include "TMultiGraph.h"
#include "TStyle.h"
#include "TArrayD.h"
#include "TPaveLabel.h"
#endif
#include "Ask.h"
TMultiGraph *mg = 0;
void PiDQA(const Char_t *histN="dEdx") {
  const Int_t N = 8;
  const Char_t *dirs[N] = {"/PiDQA/gamma/e+",
			   "/PiDQA/gamma/e-",
			   "/PiDQA/Ks/pi+",
			   "/PiDQA/Ks/pi-",
			   "/PiDQA/Lambdab/pi+",
			   "/PiDQA/Lambda/pi-",
			   "/PiDQA/Lambda/p",
			   "/PiDQA/Lambdab/p-"};
#if 0
  TCanvas *c1 = (TCanvas *) gROOT->GetListOfCanvases()->FindObject("c1");
  if (c1) c1->Clear();
  else    c1 = new TCanvas("c1","c1",600,600);
  c1->SetLogz(1);
#endif
  TGraphErrors *grs[N] = {0};
  mg = new TMultiGraph(Form("DEV_%s",histN),"Deviation for model");
  TLegend *l = new TLegend(0.4,0.4,0.7,0.7);
  TPaveLabel pl;
  Float_t x1=0.3, y1=0.8, x2=0.75, y2=0.85;
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
    TH1D *h1 = h2->ProjectionX();
    Double_t ymax = h1->GetMaximum();
    h2->FitSlicesY(0,0,-1,0,"qeg3s",arr);
    //    h2->FitSlicesY();
    TH1D *mu = (TH1D *) arr->At(1);
    TH1D *sigma = (TH1D *) arr->At(2);
    //    TH1D *mu = (TH1D *) gDirectory->Get(Form("%s_1",histN));
    //    TH1D *sigma = (TH1D *) gDirectory->Get(Form("%s_2",histN));
    if (mu && sigma) {
      h2->Draw("colz");
      pl.DrawPaveLabel(x1,y1,x2,y2,dirs[i],"brNDC");
      //      c1->Update();
      mu->Draw("same");
      //      c1->Update();
      sigma->Draw("same");
      c1->Update();
      Int_t nx = mu->GetNbinsX();
      TArrayD X(nx);  Double_t *x = X.GetArray();
      TArrayD Y(nx);  Double_t *y = Y.GetArray();
      TArrayD E(nx);  Double_t *e = E.GetArray();
      Int_t np = 0;
      for (Int_t j = 1; j <= nx; j++) {
	//	if (h1->GetBinContent(j) < 0.25*ymax) continue;
	if (sigma->GetBinContent(j) <= 0.0 || sigma->GetBinContent(j) > 0.10) continue;
	Double_t err = mu->GetBinError(j);
	if (err <= 0.0 || err > 0.02) continue;
	x[np] = mu->GetBinCenter(j);
	y[np] = mu->GetBinContent(j);
	e[np] = err;
	np++;
      }
      grs[i] = new TGraphErrors(np, x, y, 0, e);
      grs[i]->SetLineColor(i+1);
      grs[i]->SetMarkerColor(i+1);
      TString Dir(dirs[i]);
      Int_t marker = 20;
      if (Dir.EndsWith("e+") || Dir.EndsWith("e-")) marker = 21;
      if (Dir.EndsWith("p") || Dir.EndsWith("p-")) marker = 22;
      grs[i]->SetMarkerStyle(marker);
      mg->Add(grs[i]);
      l->AddEntry(grs[i],dirs[i],"p");
    }
    if (! gROOT->IsBatch() && Ask()) return;
  }
  TCanvas *cr = new TCanvas("cr","cr",600,600);
  mg->Draw("ap");
  l->Draw();
}
