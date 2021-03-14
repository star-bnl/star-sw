/*
  root.exe PromptT0Freq.C+
 */
//________________________________________________________________________________
#if !defined(__CINT__) || defined(__MAKECINT__)
#include "Riostream.h"
#include <stdio.h>
#include "TROOT.h"
#include "TSystem.h"
#include "TMath.h"
#include "TProfile.h"
#include "TFile.h"
#include "TNtuple.h"
#include "TCanvas.h"
#include "TLegend.h"
#include "TLegend.h"
#endif
void PromptT0Freq(){
  struct run_t {
    const Char_t *file;
    Float_t      freq;
    //    Int_t        date;
    //    Int_t        time;
  };
  enum {NF = 7};
  run_t files[NF] = {
    {"2019/RF/AuAu200/TGPTpcHitZTMfl0.root"      ,9.3832e+06}, //   20193001 Clock Frequency [Start / End]	9383180 / 9383180 Hz, RHIC Clock
    {"2019/RF/19GeV/TGPTpcHitZTMfl0.root"        ,9.3411e+06},
    {"2020/RF/11p5GeV/TGPTpcHitZTMfl0.root"      ,9.3374e+06}, // 20191208, 100905}, //<< 21055001 Clock Frequency [Start / End]	9337359 / 9337359 Hz, RHIC Clock
    {"2020/RF/7p7GeV/TGPTpcHitZTMfl0.root"       ,9.3321e+06}, //<< 21255019 Clock Frequency [Start / End]	9332125 / 9332125 Hz, RHIC Clock
    {"2019/RF/14p5GeV/TGPTpcHitZTMfl0.root"      ,9.3071e+06},
    {"2020/RF/9p2GeV/TGPTpcHitZTMfl0.root"       ,9.1887e+06}, //   21245001 Clock Frequency [Start / End]	9188684 / 9188684 Hz, RHIC Clock
    {"2019/RF/7p7GeV/TGPTpcHitZTMfl0.root"       ,9.1045e+06}
  };
  struct tuple_t {
    Float_t freqR;
    Float_t sector;
    Float_t io;
    Float_t dT;
  };
  tuple_t B;
  TFile *fOut = new TFile("tpcSectorT0offset.root","recreate");
  TNtuple *diff = new TNtuple("diff","T0 diff versus freq","freqR:sector:io:dT");
  TCanvas *cs[2] = {new TCanvas("c0","c0"),  new TCanvas("c1","c1")};
  const Char_t *IOname[2] = {"Inner","Outer"};
  const Char_t *IOCuts[2] = {"chisq>50&&chisq<2e2&&y<40.5","chisq>50&&chisq<2e2&&y>40.5"};
  cs[0]->DrawFrame(0.5,-100,24.5,100);
  cs[1]->DrawFrame(0.5,-100,24.5,100);
  TProfile *hists[2][NF];
  
  for (Int_t i = 0; i < NF; i++) {
    TFile *f = new TFile(files[i].file);
    TNtuple *FitP = (TNtuple *) f->Get("FitP");
    if (! FitP) continue;
    fOut->cd();
    FitP->SetMarkerColor(i+1);
    for (Int_t io = 0; io < 2; io++) {
      cs[io]->cd();
      hists[io][i] = new TProfile(Form("%s%i",IOname[io],i),files[i].file,24,0.5,24.5);
      hists[io][i]->SetMarkerColor(i+1);
      FitP->Draw(Form("mu-3.31351:x>>%s",hists[io][i]->GetName()),IOCuts[io],"profsame");
      if (hists[io][i]->GetEntries() < 10) continue;
      if (i) {
	B.freqR = files[i].freq/files[0].freq;
	for (Int_t sec = 1; sec <= 24; sec++) {
	  B.sector = sec;
	  B.io     = io;
	  B.dT     = hists[io][i]->GetBinContent(sec) - hists[io][0]->GetBinContent(sec);
	  diff->Fill(&B.freqR);
	}
      }
    }
  }
}
