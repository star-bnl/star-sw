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
#include "TString.h"
#endif
#include "Ask.h"
TMultiGraph *mg = 0;
TMultiGraph *sg = 0;
void PiDQA(const Char_t *histN="dEdx", Bool_t bg = kTRUE) {
  TString xTitle = "log_{10} (#beta #gamma)";
  if (! bg) xTitle = "log_{10} (p [GeV/c])";
  const Int_t N = 8;
  struct Part_t {
    const Char_t *dir;
    Double_t      mass;
  };
  Part_t Particles[N] = {
    {"/PiDQA/gamma/e+",    0.51099907e-3},   
    {"/PiDQA/gamma/e-",    0.51099907e-3},
    {"/PiDQA/Ks/pi+",      0.1395699},    
    {"/PiDQA/Ks/pi-",      0.1395699},    
    {"/PiDQA/Lambdab/pi+", 0.1395699},
    {"/PiDQA/Lambda/pi-",  0.1395699},
    {"/PiDQA/Lambda/p",    0.9382723},
    {"/PiDQA/Lambdab/p-",  0.9382723}
  };
#if 0
  TCanvas *c1 = (TCanvas *) gROOT->GetListOfCanvases()->FindObject("c1");
  if (c1) c1->Clear();
  else    c1 = new TCanvas("c1","c1",600,600);
  c1->SetLogz(1);
#endif
  TGraphErrors *grmu[N] = {0};
  TGraphErrors *grsigma[N] = {0};
  mg = new TMultiGraph(Form("DEV_%s",histN),Form("#mu for %s",histN));
  sg = new TMultiGraph(Form("SigmaV_%s",histN),Form("#sigma for %s",histN));
  TLegend *l = new TLegend(0.6,0.1,0.9,0.4);
  TLegend *ls = new TLegend(0.6,0.6,0.9,0.9);
  TPaveLabel pl;
  Float_t x1=0.3, y1=0.8, x2=0.75, y2=0.85;
  for (Int_t i = 0; i < N; i++) {
    if (! gDirectory->cd(Particles[i].dir)) continue;
    TCanvas *c1 = new TCanvas(Form("c%i",i),Particles[i].dir);
    c1->SetLogz(1);
    TString name(histN);
    TH2F *h2 = (TH2F *) gDirectory->Get(name);
    cout << "Histmogram " << Particles[i].dir << "/" << name.Data();
    if (! h2) {
      cout  << " has not been found" << endl;
      continue;
    }
    cout << " has been found" << endl;
    TObjArray *arr = new TObjArray(4);
    TH1D *h1 = h2->ProjectionX();
    Double_t ymax = h1->GetMaximum();
    //    h2->FitSlicesY(0,0,-1,0,"qeg3s",arr);
    h2->FitSlicesY(0,0,-1,0,"qe",arr);
    TH1D *mu = (TH1D *) arr->At(1);
    TH1D *sigma = (TH1D *) arr->At(2);
    if (mu && sigma) {
      h2->Draw("colz");
      //      pl.DrawPaveLabel(x1,y1,x2,y2,Particles[i].dir,"brNDC");
      pl.DrawPaveLabel(x1,y1,x2,y2,gDirectory->GetPath(),"brNDC");
      //      c1->Update();
      mu->Draw("same");
      //      c1->Update();
      sigma->Draw("same");
      c1->Update();
      Int_t nx = mu->GetNbinsX();
      TArrayD X(nx);  Double_t *x = X.GetArray();
      TArrayD Y(nx);  Double_t *y = Y.GetArray();
      TArrayD E(nx);  Double_t *e = E.GetArray();
      TArrayD S(nx);  Double_t *s = S.GetArray();
      TArrayD SE(nx); Double_t *se = SE.GetArray();
      Int_t np = 0;
      for (Int_t j = 1; j <= nx; j++) {
	//	if (h1->GetBinContent(j) < 0.25*ymax) continue;
	if (sigma->GetBinContent(j) <= 0.0 || sigma->GetBinContent(j) > 3) continue;
	Double_t err = mu->GetBinError(j);
	if (err <= 0.0 || err > 0.02) continue;
	//	x[np] = mu->GetBinCenter(j);
	x[np] = mu->GetBinCenter(j); 
	if (! bg) x[np] +=  TMath::Log10(Particles[i].mass);
	y[np] = mu->GetBinContent(j);
	e[np] = err;
	s[np] = sigma->GetBinContent(j);
	se[np] = sigma->GetBinError(j);
	np++;
      }
      grmu[i] = new TGraphErrors(np, x, y, 0, e);
      grmu[i]->SetLineColor(i+1);
      grmu[i]->SetMarkerColor(i+1);
      grsigma[i] = new TGraphErrors(np, x, s, 0, se);
      grsigma[i]->SetLineColor(i+1);
      grsigma[i]->SetMarkerColor(i+1);
      TString Dir(Particles[i].dir);
      Int_t marker = 20;
      if (Dir.EndsWith("e+") || Dir.EndsWith("e-")) marker = 21;
      if (Dir.EndsWith("p") || Dir.EndsWith("p-")) marker = 22;
      grmu[i]->SetMarkerStyle(marker);
      mg->Add(grmu[i]);
      l->AddEntry(grmu[i],Particles[i].dir,"p");
      grsigma[i]->SetMarkerStyle(marker);
      sg->Add(grsigma[i]);
      ls->AddEntry(grsigma[i],Particles[i].dir,"p");
    }
    if (! gROOT->IsBatch() && Ask()) return;
  }
  TCanvas *cr = new TCanvas("cr","cr",200,100,1600,800);
  mg->Draw("ap");
  mg->GetHistogram()->SetXTitle(xTitle);
  mg->Draw("p");
  l->Draw();
  cr->Update();
  TCanvas *csigma = new TCanvas("csigma","sigma",200,1000,1600,800);
  sg->Draw("ap");
  sg->GetHistogram()->SetXTitle(xTitle);
  sg->Draw("p");
  ls->Draw();
  csigma->Update();
}
