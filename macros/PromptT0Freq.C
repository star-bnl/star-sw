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
#include "TH2.h"
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
    {"2019/RF/AuAu200/T1400GPTpcHitZTMfl0.root"      ,9.3832e+06}, //   20193001 Clock Frequency [Start / End]	9383180 / 9383180 Hz, RHIC Clock
    {"2019/RF/19GeV/T1400GPTpcHitZTMfl0.root"        ,9.3411e+06},
    {"2020/RF/11p5GeV/T1400GPTpcHitZTMfl0.root"      ,9.3374e+06}, // 20191208, 100905}, //<< 21055001 Clock Frequency [Start / End]	9337359 / 9337359 Hz, RHIC Clock
    {"2020/RF/7p7GeV/T1400GPTpcHitZTMfl0.root"       ,9.3321e+06}, //<< 21255019 Clock Frequency [Start / End]	9332125 / 9332125 Hz, RHIC Clock
    {"2019/RF/14p5GeV/T1400GPTpcHitZTMfl0.root"      ,9.3071e+06},
    {"2020/RF/9p2GeV/T1400GPTpcHitZTMfl0.root"       ,9.1887e+06}, //   21245001 Clock Frequency [Start / End]	9188684 / 9188684 Hz, RHIC Clock
    {"2019/RF/7p7GeV/T1400GPTpcHitZTMfl0.root"       ,9.1045e+06}
  };
  struct tuple_t {
    Float_t freqR;
    Float_t freq;
    Float_t sector;
    Float_t row;
    Float_t t0;
    Float_t dt0;
  };
  tuple_t B;
  TFile *fOut = new TFile("tpcSecRowT0offset1400.root","recreate");
  TNtuple *FitP = new TNtuple("FitP","T0 versus freq, sector, row","freqR:freq:sector:row:t0:dt0");
  
  for (Int_t i = 0; i < NF; i++) {
    TFile *f = new TFile(files[i].file);
    if (! f) continue;
    TH2 *mu = (TH2*) f->Get("mu");
    if (! mu) continue;
    B.freq  = files[i].freq;
    B.freqR = files[i].freq/files[0].freq;
    Int_t nx = mu->GetXaxis()->GetNbins();
    Int_t ny = mu->GetYaxis()->GetNbins();
    for (Int_t sector = 1; sector <= nx; sector++) {
      B.sector = sector;
      for (Int_t row = 1; row <= ny; row++) {
	B.row = row;
	B.t0 = mu->GetBinContent(sector,row);
	if (B.t0 < 0 || B.t0 > 20) continue;
	B.dt0 = mu->GetBinError(sector,row);
	if (B.dt0 < 0 || B.dt0 > 0.03) continue;
	FitP->Fill(&B.freqR);
      }
    }
  }
}
