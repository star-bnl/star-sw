#if !defined(__CINT__) || defined(__MAKECINT__)
#include "Riostream.h"
#include <stdio.h>
#include "TROOT.h"
#include "TSystem.h"
#include "TMath.h"
#include "TH1.h"
#include "TH2.h"
#include "TH3.h"
#include "TFile.h"
#include "TCanvas.h"
#include "TLegend.h"
#endif
//________________________________________________________________________________
TH1 *GetPlot(TDirectory *dir, const Char_t *path = "/Particles/KFParticlesFinder/Particles/Ks/Parameters/z") {
  if (! dir) return 0;
  return (TH1 *) dir->Get(path);
}
//________________________________________________________________________________
void kfpPlot(const Char_t *histName = "M", const Char_t *path = "/Particles/KFParticlesFinder/Particles/Ks/Parameters", Bool_t Norm = kTRUE) {
  const Char_t *zPlotPath = "/Particles/KFParticlesFinder/PrimaryVertexQA/z";
  Int_t NF = 0;
  TSeqCollection *files = gROOT->GetListOfFiles();
  if (! files) return;
  TIter next(files);
  TFile *f = 0;
  TCanvas *c1 = (TCanvas *) gROOT->GetListOfCanvases()->FindObject("c1");
  if (! c1 ) c1 = new TCanvas("c1","c1");
  else       c1->Clear();
  Int_t color = 1;
  TLegend *l = new TLegend(0.3,0.7,0.9,0.9);
  Double_t noentries = -1;
  Double_t scale = 1;
  while ( (f = (TFile *) next()) ) { 
    f->cd();
    if (Norm) {
      TH1F *z = (TH1F *) GetPlot(gDirectory, zPlotPath);
      if (! z ) {Norm = kFALSE;
      } else {
	Double_t entries = z->GetEntries();
	if (noentries < 0) noentries = entries;
	if (entries > 0) {
	  scale = noentries/entries;
	}
      }
    }
    TString Fname(gSystem->BaseName(f->GetName()));
    Fname.ReplaceAll(".root","");
    TString Path(f->GetName());
    Path += ":";
    Path += path;  
    Path += "/"; Path += histName; cout << "Path\t" << Path << endl;
    TH1F *h = (TH1F *) GetPlot(gDirectory,Path);
    if (! h) continue;
    h->SetLineColor(color);
    h->SetMarkerColor(color);
    h->SetStats(0);
    h->Scale(scale);
    if (color == 1) h->Draw();
    else            h->Draw("sames");
    TString Line( Form("%s, etries = %10.2e, mean = %7.3f, RMS = %7.3f", Fname.Data(), h->GetEntries(),h->GetMean(), h->GetRMS()));
    if (Norm) Line += ". Scaled";
    l->AddEntry(h, Line);
    color++;
  }
  l->Draw();
}
//________________________________________________________________________________
void kfpPlot2(const Char_t *histName = "Armenteros", const Char_t *path = "/Particles/KFParticlesFinder/Particles/Ks/Parameters") {
  Int_t NF = 0;
  TSeqCollection *files = gROOT->GetListOfFiles();
  if (! files) return;
  TIter next(files);
  TFile *f = 0;
  TCanvas *c1 = (TCanvas *) gROOT->GetListOfCanvases()->FindObject("c1");
  if (! c1 ) c1 = new TCanvas("c1","c1");
  c1->Clear();
  Int_t ny =  files->GetSize();
  c1->Divide(1,ny);
  Int_t color = 1;
  while ( (f = (TFile *) next()) ) { 
    f->cd();
    TString Fname(gSystem->BaseName(f->GetName()));
    Fname.ReplaceAll(".root","");
    TString Path(f->GetName());
    Path += ":";
    Path += path;  
    Path += "/"; Path += histName; cout << "Path\t" << Path << endl;
    TH2 *h2 = (TH2 *)GetPlot(gDirectory,Path);
    if (! h2) {
      cout << "Histogram " << histName << " has not been found" << endl;
      continue;
    }
#if 0
    h2->SetLineColor(color);
    h2->SetMarkerColor(color);
#endif
    h2->SetStats(0);
    c1->cd(color)->SetLogz(1);
    h2->Draw("colz");
    TLegend *l = new TLegend(0.3,0.9,0.9,1.0);
    l->AddEntry(h2, Form("%s, etries = %10.2e, mean = %7.3f, RMS = %7.3f", Fname.Data(), h2->GetEntries(),h2->GetMean(), h2->GetRMS()));
    l->Draw();
    color++;
  }
}

/* 
   kfpPlot("M","/Particles/KFParticlesFinder/Particles/Ks/Parameters")
   kfpPlot("pullM","/Particles/KFParticlesFinder/Particles/Ks/FitQAPull")
   kfpPlot("z","/Particles/KFParticlesFinder/PrimaryVertexQA")
   kfpPlot2("Armenteros", "/Particles/KFParticlesFinder/Particles/Lambda/Parameters")
   kfpPlot2("hdEdX", "Tracks")

 */
