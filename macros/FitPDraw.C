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
#include "TH3.h"
#include "TStyle.h"
#include "TF1.h"
#include "TProfile.h"
#include "TTree.h"
#include "TChain.h"
#include "TFile.h"
#include "TNtuple.h"
#include "TCanvas.h"
#include "TFileSet.h"
#include "TDataSetIter.h"
#include "TDataSet.h"
#include "TClassTable.h"
#include "TString.h"
#include "TRegexp.h"
#include "TLine.h"
#include "TText.h"
#include "TList.h"
#include "TPolyMarker.h"
#include "TKey.h"
#include "TLegend.h"
#include "TStyle.h"
#endif
//________________________________________________________________________________
void FitPDraw(const Char_t *draw="mu:y", 
	      const Char_t *ext = "P",
	      Int_t    nx = 0,   // 45,
	      Double_t xMin = 0, //  0.5,
	      Double_t xMax = 0, // 45.5,
	      const Char_t *cut = "(i&&j&&abs(mu)<1)/(dmu**2)", 
	      const Char_t *opt = "profg",
	      Double_t ymin = 0,
	      Double_t ymax = 0) {
  TString Current(gDirectory->GetName());
  TSeqCollection *fs = gROOT->GetListOfFiles();
  if (! fs) {cout << "No root files " << endl; return;}
  gStyle->SetOptStat(0);
  Int_t N = fs->GetEntries();
  if (! N) return;
  TFile **F = new TFile*[N]; memset(F, 0, N*sizeof(TFile*));
  Int_t icol = 0;
  TLegend *leg = new TLegend(0.5,0.7,1.0,1.0);
  TIter  iter(fs);
  TFile *f = 0;  
  Int_t NF = 0;
  while ((f = (TFile *) iter())) {
    TNtuple *FitP = (TNtuple *) f->Get("FitP");
    if (! FitP) continue;
    TString name(gSystem->BaseName(f->GetName()));
    name.ReplaceAll(".root","");
    //      cout << name << endl;
    F[NF] = f;
    //      cout << k << "/" << NF << "\t" << F[NF]->GetName() << endl;
    NF++;
  }
  //  gStyle->SetMarkerSize(0.4);
  for (Int_t k = 0; k < NF; k++) {
    if (! F[k]) continue;
    F[k]->cd();
    TString name(gSystem->BaseName(gDirectory->GetName()));
    name.ReplaceAll(".root","");
    cout << name << endl;
    TNtuple *FitP = (TNtuple *) gDirectory->Get("FitP");
    if (! FitP ) continue;
    icol = k + 1;
    Int_t kM = 20;
    if (icol == 3) kM = 24;
#if 0
    if      (icol == 1) kM = 20;
    else if (icol == 5) kM = 20;
    else if (icol >  7) kM = 27;
#endif
    FitP->SetMarkerStyle(kM);
    FitP->SetMarkerColor(icol);
    FitP->SetLineColor(icol);
    //    FitP->SetMarkerSize(1);
    TString same(opt);
    if (k != 0) same += "same";
    TString Draw(draw);
    TString histN(ext); histN += k;
    Draw += " >> "; Draw += histN;
    TProfile *p = 0;
    if (xMin == 0 && xMax == 0) {
      TH2 *mu = (TH2 *) gDirectory->Get("mu");
      if (mu) {
	if (Draw.Index("y") >= 0) {
	  //      Draw += Form("(%i,%f,%f)",mu->GetYaxis()->GetNbins(), mu->GetYaxis()->GetXmin(), mu->GetYaxis()->GetXmax());
	  p = new TProfile(histN,"#mu versus pad row",mu->GetYaxis()->GetNbins(), mu->GetYaxis()->GetXmin(), mu->GetYaxis()->GetXmax());
	} else {
	  p = new TProfile(histN,"#mu versus pad row",mu->GetXaxis()->GetNbins(), mu->GetXaxis()->GetXmin(), mu->GetXaxis()->GetXmax());
	}
      }
    } else {
      p = new TProfile(histN,"#mu versus pad row",nx, xMin, xMax);
    }
    p->SetMarkerColor(icol);
    p->SetLineColor(icol);
    cout << k << "\t" << F[k]->GetName() << endl;
    cout << "FitP->Draw(\"" << Draw << "\",\"" << cut << "\",\"" << same << "\")" << endl;
    FitP->Draw(Draw,cut,same);
    TH1 *hist = (TH1 *) gDirectory->Get(histN);
    if (hist) {
      if (ymin < ymax) {hist->SetMinimum(ymin); hist->SetMaximum(ymax);}
      leg->AddEntry(hist,name.Data());
    }
  }
  leg->Draw();
}
//________________________________________________________________________________
void FitPDraw(const Char_t *dir,  // ="Qcm", 
	      const Char_t *draw, //="mu:y", 
	      const Char_t *cut,  // = "(i&&j&&abs(mu)<1)/(dmu**2)", 
	      const Char_t *opt,  // = "profg",
	      const Char_t *ext,  // = "P",
	      Double_t ymin, // = 0,
	      Double_t ymax){ // = 0) {
  TString Current(gDirectory->GetName());
  TSeqCollection *fs = gROOT->GetListOfFiles();
  if (! fs) {cout << "No root files " << endl; return;}
  Int_t N = 8;
  const Char_t *Names[8] = {
    "AuAu200_production_FullFieldLL",
    "AuAu200_production_FullField",
    "LowLuminosity_2010_ReversedFullField",
    "AuAu200_production_ReversedFullField",
    "AuAu62_production_ReversedFullField",
    "AuAu39_production_ReversedFullField",
    "AuAu7_production_ReversedFullField",
    "AuAu11_production_ReversedFullField"
  };
  TFile *F[32]; memset(F, 0, sizeof(F));
  TString Dir(dir);
  TRegexp reg(dir);
  Int_t icol = 0;
  TLegend *leg = new TLegend(0.5,0.7,1.0,1.0);
  TString Draw(draw);
  TString histN(dir); histN += ext;
  Draw += " >> "; Draw += histN;
  Int_t index = histN.Index("(");
  if (index > 0) histN.Resize(index);
  //  TIter  iter(fs, kIterBackward);
  TIter  iter(fs);
  TFile *f = 0;  
  Int_t NF = 0;
  for (Int_t k = 0; k <= N; k++) {
    iter.Reset();
    while ((f = (TFile *) iter())) {
      TString name(gSystem->BaseName(f->GetName()));
      name.ReplaceAll(".root","");
      //      cout << name << endl;
      if (Dir != "" && Dir != "*" && ! name.Contains(reg)) continue; 
      if (k == 0) {
	Bool_t matched = kFALSE;
	for (Int_t l = 0; l < N; l++) {
	  if (name.EndsWith(Names[l])) {matched = kTRUE; break;}
	}
	if (matched) continue;
      }	else {
	if (! name.EndsWith(Names[k-1])) continue;
      }
      F[NF] = f;
      //      cout << k << "/" << NF << "\t" << F[NF]->GetName() << endl;
      NF++;
    }
  }
  //  gStyle->SetMarkerSize(0.4);
  for (Int_t k = 0; k < NF; k++) {
    if (! F[k]) continue;
    F[k]->cd();
    TString name(gSystem->BaseName(gDirectory->GetName()));
    name.ReplaceAll(".root","");
    cout << name << endl;
    TNtuple *FitP = (TNtuple *) gDirectory->Get("FitP");
    if (! FitP ) continue;
    icol = k + 1;
    Int_t kM = 20;
    if (icol == 3) kM = 24;
#if 0
    if      (icol == 1) kM = 20;
    else if (icol == 5) kM = 20;
    else if (icol >  7) kM = 27;
#endif
    FitP->SetMarkerStyle(kM);
    FitP->SetMarkerColor(icol);
    FitP->SetLineColor(icol);
    //    FitP->SetMarkerSize(1);
    TString same(opt);
    if (k != 0) same += "same";
    cout << k << "\t" << F[k]->GetName() << endl;
    cout << "FitP->Draw(\"" << Draw << "\",\"" << cut << "\",\"" << same << "\")" << endl;
    FitP->Draw(Draw,cut,same);
    TH1 *hist = (TH1 *) gDirectory->Get(histN);
    if (hist) {
      if (ymin < ymax) {hist->SetMinimum(ymin); hist->SetMaximum(ymax);}
      index = name.Index("Run");
      if (index < 0) index = 0;
      else           index += 3;
      leg->AddEntry(hist,name.Data()+index);
    }
  }
  leg->Draw();
}
//________________________________________________________________________________
void DrawHist(const Char_t *dir="TPoints70BG", 
	      const Char_t *histN="mu") {
  TString Current(gDirectory->GetName());
  TSeqCollection *fs = gROOT->GetListOfFiles();
  if (! fs) {cout << "No root files " << endl; return;}
  Int_t N = 8;
  const Char_t *Names[8] = {
    "AuAu200_production_FullFieldLL",
    "AuAu200_production_FullField",
    "LowLuminosity_2010_ReversedFullField",
    "AuAu200_production_ReversedFullField",
    "AuAu62_production_ReversedFullField",
    "AuAu39_production_ReversedFullField",
    "AuAu7_production_ReversedFullField",
    "AuAu11_production_ReversedFullField"
  };
  TFile *F[9]; memset(F, 0, sizeof(F));
  TString Dir(dir);
  TRegexp reg(dir);
  Int_t icol = 0;
  TLegend *leg = new TLegend(0.5,0.7,1.0,1.0);
  TIter  iter(fs);
  TFile *f = 0;  
  while ((f = (TFile *) iter())) {
    TString name(gSystem->BaseName(f->GetName()));
    name.ReplaceAll(".root","");
    if (Dir != "" && Dir != "*" && ! name.Contains(reg)) continue; 
    Int_t l = 0;
    for (Int_t k = 0; k < N; k++) {
      //      cout << name << " and " << Names[k] << endl;
      if (name.EndsWith(Names[k])) {l = k+1;}
    }
    F[l] = f;
    cout << l << "\t" << F[l]->GetName() << endl;
  }
  //  gStyle->SetMarkerSize(0.4);
  for (Int_t k = 0; k <= N; k++) {
    if (! F[k]) continue;
    F[k]->cd();
    TString name(gSystem->BaseName(gDirectory->GetName()));
    name.ReplaceAll(".root","");
    cout << name << endl;
    TH1 *hist = (TH1 *) gDirectory->Get(histN);
    if (! hist ) continue;
    icol = k + 1;
    Int_t kM = 20;
    if (icol == 3) kM = 24;
#if 0
    if      (icol == 1) kM = 20;
    else if (icol == 5) kM = 20;
    else if (icol >  7) kM = 27;
#endif
    hist->SetMarkerStyle(kM);
    hist->SetMarkerColor(icol);
    hist->SetLineColor(icol);
    TString same("");
    if (k != 0) same += "same";
    hist->Draw(same);
    Int_t index = name.Index("Run");
    if (index < 0) index = 0;
    else           index += 3;
    leg->AddEntry(hist,name.Data()+index);
  }
  leg->Draw();
}
