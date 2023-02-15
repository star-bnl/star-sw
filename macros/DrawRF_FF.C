#if !defined(__CINT__) || defined(__MAKECINT__)
//#include <map>
//#include <array>
#include "Riostream.h"
#include <stdio.h>
#include "TROOT.h"
#include "TSystem.h"
#include "TMath.h"
#include "TH1.h"
#include "TH2.h"
#include "TH3.h"
#include "TStyle.h"
#include "TFile.h"
#include "TCanvas.h"
#include "TLegend.h"
#endif
#if 0
void DrawRF_FF() {
  c1 = new TCanvas("c1","c1");
  gStyle->SetOptStat(0);
  c1->Divide(1,3);
  c1->cd(1);
  dY->Draw("colz");
  TH2F *ddY = new TH2F(*dY);
  ddY->SetName("ddY");
  _file0->cd();
  c1->cd(2);
  dY->Draw("colz");
  ddY->Add(dY,-1);
  c1->cd(3);
  ddY->Draw("colz");
}
#else
void DrawRF_FF(const Char_t *histN = "dYS2P") {
  Int_t NF = 0;
  TSeqCollection *files = gROOT->GetListOfFiles();
  if (! files) return;
  Int_t nn = files->GetSize();
  if (! nn) return;
  TFile **FitFiles = new TFile *[nn];
  TIter next(files);
  TFile *f = 0;
  while ( (f = (TFile *) next()) ) { 
    TH1 *hist = (TH1 *) f->Get(histN);
    if (! hist) continue;
    FitFiles[NF] = f; 
    NF++;
  }
  if (! NF) return;
  TCanvas *c1 = new TCanvas(histN,histN);
  TH1F *frame = c1->DrawFrame(0.5,-0.4,24.5,0.4);
  frame->SetXTitle("sector");
  frame->SetYTitle(histN);
  gStyle->SetOptStat(0);
  Int_t color = 0;
  TLegend *l = new TLegend(0.6,0.7,0.9,0.9);
  for (Int_t i = 0; i < NF; i++) {
    FitFiles[i]->cd();
    TH1 *hist = (TH1 *) gDirectory->Get(histN);
    TH3 *h3 = 0;
    TH2 *h2 = 0;
    TH1 *h1 = 0;
    Int_t dim = hist->GetDimension();
    if (dim == 3) {
      TH3F *h3 = (TH3F *) hist;
      if (! h3) continue;
      h2 = (TH2 *) h3->Project3D("zx");
    } else if (dim == 2) {
      h2 = (TH2 *) hist;
    }
    if (h2) {
      //      TObjArray* arr =  new TObjArray(4);
      //      h2->FitSlicesY(0,0,-1,10,"q",arr);
      h2->FitSlicesY();
      h1 = (TH1*) gDirectory->Get(Form("%s_1",h2->GetName()));
      //      h1 =  (TH1 *) (*arr)[1];
      //      delete arr;
    } else {
      h1 = (TH1 *) hist;
    }
    color++;
    h1->SetMarkerColor(color);
    h1->SetLineColor(color);
    h1->Draw("same"); 
    TString Title(gSystem->DirName(gDirectory->GetName()));
    Title.ReplaceAll("/FF_OO_200GeV_2021","");
    l->AddEntry(h1,Title);
  }
  l->Draw();
  c1->Update();
}
#endif
