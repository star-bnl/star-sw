//  root.exe 2*/muMc.root  muMcpT.C+
#if !defined(__CINT__) || defined(__MAKECINT__)
#include "Riostream.h"
#include <stdio.h>
#include "TROOT.h"
#include "TF1.h"
#include "TAxis.h"
#include "TMath.h"
#include "TH1.h"
#include "TH2.h"
#include "TH3.h"
#include "TNtuple.h"
#include "TFile.h"
#include "TFitResult.h"
#include "TCanvas.h"
#include "TLegend.h"
#include "TSystem.h"
#include "TString.h"
#endif
void muMcpT() {
  TSeqCollection *files = gROOT->GetListOfFiles();
  if (! files) return;
  Int_t nn = files->GetSize();
  if (! nn) return;
  TIter next(files);
  TFile *f = 0;
  TH3F *h3 = 0;
  TObjArray *arr = new TObjArray();
#if 1
  TF1 *F = new TF1("F","TMath::Sqrt([0]*[0]+[1]*[1]/(x*x*x*x) + [2]*[2]*x*x)",0,10); 
  F->SetParameters(8.03638e-03,1.32525e-04, 3.42140e-03);
  F->SetParNames("A","B","C");
  F->SetParLimits(0,0,100);
  F->SetParLimits(1,0,100);
  F->SetParLimits(2,0,100);
  F->FixParameter(1,0);
#else
  TF1 *F = new TF1("F","TMath::Sqrt([0]*[0]+ [1]*[1]*x*x)",0,10); 
  F->SetParameters(8.03638e-03,3.42140e-03);
  F->SetParNames("A","B");
#endif
  TCanvas *c1 = new TCanvas("c1","c1");
  c1->Divide(nn,1);
  TCanvas *c2 = new TCanvas("c2","c2");
  TH1F *frame = c2->DrawFrame(0.05,0.5,10.,2.5);
  c2->SetLogx(1);
  frame->SetXTitle("p_{T} (GeV/c)");
  frame->SetYTitle("#sigma(p_{T})/p_{T} (%)");
  Int_t i = 0;
  Double_t yMax = 0.5;
  TLegend *l2 = new TLegend(0.4,0.1,0.8,0.2);
  Double_t xMax = 10;
  l2->Draw();
  while ( (f = (TFile *) next()) ) { 
    f->cd(); 
    h3 = (TH3F *) f->Get("/Tracks/Primary/Rec/All/(-)/EtapT/dPtiR");
    if (! h3) continue;
    TString year(gSystem->DirName(f->GetName()));
    c1->cd(++i)->SetLogx(1);
    cout <<  i << "\t" << f->GetName() << endl;
    TH3F *h3new = new TH3F(*h3);
    h3 = (TH3F *) f->Get("/Tracks/Primary/Rec/All/(+)/EtapT/dPtiR");
    if (h3) h3new->Add(h3);
    TAxis *x = h3new->GetXaxis();
    Int_t i1 = x->FindBin(-yMax);
    Int_t i2 = x->FindBin( yMax);
    x->SetRange(i1,i2);
    TH2D *h2 = (TH2D *) h3new->Project3D("zy");
    h2->FitSlicesY(0,-1,-1,10,"req", arr);
    TH1D *sigma = (TH1D *) arr->At(2);
    sigma->SetName(year);
    sigma->SetMarkerColor(i);
    F->SetLineColor(i);
    Int_t imax = sigma->GetXaxis()->FindBin(xMax);
    sigma->GetXaxis()->SetRange(1,imax);
    sigma->Scale(100);
    sigma->SetTitle("#sigma(p_{T})/p_{T} (%)");
    sigma->SetMinimum(0.5);
    sigma->SetMaximum(2.5);
    Double_t xmin = 1.0;
    if (i == 1) xmin = 1.0;
    sigma->Fit(F,"erm","",xmin,xMax);
    TLegend *l = new TLegend(0.4,0.1,0.8,0.2);
    l->AddEntry(sigma,year);
    l->Draw();
    c2->cd();
    sigma->Draw("same");
    l2->AddEntry(sigma,year);
  }
}
