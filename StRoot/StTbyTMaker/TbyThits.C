#if !defined(__CINT__) && !defined(__CLING__) && ! defined(__MAKECINT__)
// code that should be seen ONLY by the compiler
#else
#if !defined(__CINT__) && !defined(__CLING__) || defined(__MAKECINT__)
// code that should be seen by the compiler AND rootcint
#else
// code that should always be seen
#endif
#endif
//    ROOT5
#if defined(__CINT__) && !defined(__MAKECINT__)
// source code being actually interpreted by cint
#elif defined(__MAKECINT__)
// source code seen by rootcint only
#elif defined(__ACLIC__)
// source code being actually compiled by ACLiC
#else
// source code suitable for a standalone executable
#endif
//    ROOT6 
#if defined(__CLING__) && !defined(__ROOTCLING__)
// source code being actually interpreted by Cling
#elif defined(__ROOTCLING__)
// source code seen by rootcling only
#elif defined(__ACLIC__)
// source code being actually compiled by ACLiC
#else
// source code suitable for a standalone executable
#endif
#if ROOT_VERSION_CODE >= ROOT_VERSION(5,34,18)
#define __USE_ROOFIT__
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
#include "TH3.h"
#include "THnSparse.h"
#include "TStyle.h"
#include "TF1.h"
#include "TProfile.h"
#include "TTree.h"
#include "TChain.h"
#include "TFile.h"
#include "TNtuple.h"
#include "TFitResult.h"
#include "TCanvas.h"
#include "TFileSet.h"
#include "TDataSetIter.h"
#include "TDataSet.h"
#include "TClassTable.h"
//#include "DeDxTree.C"
#include "TMinuit.h"
#include "TSpectrum.h"
#include "StBichsel/Bichsel.h"
#include "StBichsel/StdEdxModel.h"
#include "TString.h"
#include "TLine.h"
#include "TText.h"
#include "TList.h"
#include "TPolyMarker.h"
#include "TKey.h"
#include "TLegend.h"
#include "TPaletteAxis.h"
#include "TDirIter.h"
#endif
// Clusters
struct Name_t {
  const Char_t *histName;
  const Char_t *varName;
  const Char_t *cutName;
};
Name_t Names[24] = {
  {"PadFC",       "newP.pad:newP.row","oldP.sector<=0&&newP.sector!=20"},
  {"PminFC",     "newP.pmin:newP.row","oldP.sector<=0&&newP.sector!=20"},
  {"PmaxFC",     "newP.pmax:newP.row","oldP.sector<=0&&newP.sector!=20"},
  {"PadFCAll",    "newP.pad:newP.row","newP.sector!=20"},
  {"PminFCAll",  "newP.pmin:newP.row","newP.sector!=20"},
  {"PmaxFCAll",  "newP.pmax:newP.row","newP.sector!=20"},
  
  {"PadOC",       "oldP.pad:oldP.row","newP.sector<=0&&oldP.sector!=20"},
  {"PminOC",     "oldP.pmin:oldP.row","newP.sector<=0&&oldP.sector!=20"},
  {"PmaxOC",     "oldP.pmax:oldP.row","newP.sector<=0&&oldP.sector!=20"},
  {"PadOCAll",    "oldP.pad:oldP.row","oldP.sector!=20"},
  {"PminOCAll",  "oldP.pmin:oldP.row","oldP.sector!=20"},
  {"PmaxOCAll",  "oldP.pmax:oldP.row","oldP.sector!=20"},
  
  {"PadFC20",       "newP.pad:newP.row","oldP.sector<=0&&newP.sector==20"},
  {"PminFC20",     "newP.pmin:newP.row","oldP.sector<=0&&newP.sector==20"},
  {"PmaxFC20",     "newP.pmax:newP.row","oldP.sector<=0&&newP.sector==20"},
  {"PadFC20All",    "newP.pad:newP.row","newP.sector==20"},
  {"PminFC20All",  "newP.pmin:newP.row","newP.sector==20"},
  {"PmaxFC20All",  "newP.pmax:newP.row","newP.sector==20"},
  
  {"PadOC20",       "oldP.pad:oldP.row","newP.sector<=0&&oldP.sector==20"},
  {"PminOC20",     "oldP.pmin:oldP.row","newP.sector<=0&&oldP.sector==20"},
  {"PmaxOC20",     "oldP.pmax:oldP.row","newP.sector<=0&&oldP.sector==20"},
  {"PadOC20All",    "oldP.pad:oldP.row","oldP.sector==20"},
  {"PminOC20All",  "oldP.pmin:oldP.row","oldP.sector==20"},
  {"PmaxOC20All",  "oldP.pmax:oldP.row","oldP.sector==20"}
  
};
//________________________________________________________________________________
TH2 *DrawRatio(TCanvas *c1, TH2F *P, TH2F *PAll) {
  
  TH2 *R = new TH2F(*P); // (TH2 *) P->Project3D("yz");
  R->SetName(Form("R%s",R->GetName()));
  //      TH2 *RAll = (TH2 *) PAll->Project3D("yz");
  //      R->Divide(RAll);
  R->Divide(PAll);
  R->SetStats(0);
  R->SetTitle(Form("Ratio of %s", P->GetName()));
  R->Draw("colz");
  c1->Update();
  TPaletteAxis *palette = (TPaletteAxis*)R->GetListOfFunctions()->FindObject("palette");
  if (palette) {
    palette->SetX2NDC(0.94);
    c1->Update();
  }
  return R;
}
//________________________________________________________________________________
void DrawAll() {
  for (Int_t l = 0; l < 4; l++) {
    for (Int_t k = 0; k < 3; k++) {
      TCanvas *c1 = (TCanvas *) gROOT->GetListOfCanvases()->FindObject(Form("c%i",l));
      if (! c1 ) c1 = new TCanvas(Form("c%i",Names[6*l+k]));
      c1->SetLogz(1);
      TH2F *P = (TH2F *) gDirectory->Get(Names[6*l+k].histName);
      TH2F *PAll = (TH2F *) gDirectory->Get(Names[6*l+k+3].histName);
      DrawRatio(c1, P, PAll);
      if (! P || ! PAll) continue;
      TH2 *R = DrawRatio(c1, P, PAll);
    }
  }
}
//________________________________________________________________________________
void TbyThits() {
  TChain *tChain = 0;
  TFile *fOut = 0;
  //  fOut = new TFile("hit.root","update");
  if (! fOut) {
    TDirIter Dir("trackMateFilest_physics_adc*.root");
    TFile *f = 0;
    const Char_t *TreeName = "hitMateComp";
    tChain = new TChain(TreeName);
    Int_t NFiles = 0;
    ULong64_t nEvents = 0;
    ULong64_t nEvTot = 0;
    Char_t *file = 0;
    while ( (file = (Char_t *) Dir.NextFile()) ) {   
      f = new TFile(file);
      if (! f) {cout << "missing file " << file << endl; continue;}
      TTree *tree = (TTree *) f->Get(TreeName);
      cout << "#\t" << NFiles << "\t" << f->GetName();
      if (tree) {
	NFiles++;
	nEvents = tree->GetEntries();
	cout << "\tNo,Events = " << nEvents << endl;
	nEvTot += nEvents;
	tChain->Add(f->GetName());
      } else {
	cout << "\tTTree is missing" << endl;
      }
      delete f; 
    }
    cout	<< "chained " << NFiles  << " files \t" 
		<< "with total " << nEvTot << " events \t" 
		<< "chain returned pointer: " << tChain << endl;
    if (! fOut) fOut = new TFile("hit2D.root","recreate");
    TCanvas *c1 = new TCanvas();
    c1->SetLogz(1);
    for (Int_t k = 0; k < 24; k++) {
      TH2F *hist = (TH2F *) gDirectory->Get("Names[k].histName");
      if ( hist) continue;
      tChain->Draw(Form("%s>>%s(72,0.5,72.5,182,0.5,182.5)",Names[k].varName,Names[k].histName),Form("%s",Names[k].cutName),"goff",100000000);
      hist = (TH2F *) gDirectory->Get("Names[k].histName");
      if (! hist) continue;
      hist->SetXTitle("row");
      hist->SetYTitle("pad");
    }
  }
  DrawAll();
  fOut->Write();
}
/*
// 2020 
c1->SetLogz();
gStyle->SetOptStat(0)
RPadFC->SetTitle("Ratio of lost Online Clusters wrt Offline ones")
RPadFC->SetXTitle("row")
RPadFC->SetYTitle("pad")
RPadFC->GetXaxis()->SetRange(1,45);
RPadFC->Draw("colz");

RPadOC->SetTitle("Ratio of lost Offline Clusters wrt Online ones")
RPadOC->SetXTitle("row")
RPadOC->SetYTitle("pad")
RPadOC->GetXaxis()->SetRange(1,45);
RPadOC->SetMaximum(1)
RPadOC->Draw("colz")

PadFCAll->SetTitle("All offline clusters")
PadFCAll->SetXTitle("row")
PadFCAll->SetYTitle("pad")
PadFCAll->GetXaxis()->SetRange(1,45);
PadFCAll->Draw("colz")

PadOCAll->SetTitle("All online clusters")
PadOCAll->SetXTitle("row")
PadOCAll->SetYTitle("pad")
PadOCAll->GetXaxis()->SetRange(1,45);
PadOCAll->Draw("colz")

PminFCAll->SetTitle("All offline clusters")
PminFCAll->SetXTitle("row")
PminFCAll->SetYTitle("min pad")
PminFCAll->GetXaxis()->SetRange(1,45);
PminFCAll->Draw("colz")

PminOCAll->SetTitle("All online clusters")
PminOCAll->SetXTitle("row")
PminOCAll->SetYTitle("min pad")
PminOCAll->GetXaxis()->SetRange(1,45);
PminOCAll->Draw("colz")
*/
