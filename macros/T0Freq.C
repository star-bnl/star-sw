/*
  root.exe T0Freq.C+
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
#include "TGraphErrors.h"
#include "TF1.h"
#endif
TGraphErrors *gr = 0;
//________________________________________________________________________________
void T0Freq(){
  struct run_t {
    const Char_t *file;
    Float_t       T0;
    Float_t       dT;
    Float_t      ddT;
    Float_t      freq;
    //    Int_t        date;
    //    Int_t        time;
  };

  enum {NF = 5};
  run_t files[NF] = {
    {"2019/RF/AuAu200"      , 2.3904,-0.0482, 0.0201, 9383180}, //   20193001 Clock Frequency [Start / End]	9383180 / 9383180 Hz, RHIC Clock
    {"2019/RF/19GeV"        , 2.3675,-0.0023, 0.0106, 9341081}, // 9.3411e+06},
    {"2019/RF/14p5GeV"      , 2.3904, 0.0070, 0.0122, 9307123}, // 9.3071e+06},
    {"2019/RF/9p2GeV"       , 2.3904, 0.0141, 0.0152, 9188688},
    {"2019/RF/7p7GeV"       , 2.3904, 0.0362, 0.0144, 9104517}  // 9.1045e+06}
  };
  Double_t x[NF], y[NF], dx[NF], dy[NF];
  for (Int_t i = 0; i < NF; i++) {
    x[i] = 1e6/files[i].freq;
    y[i] = files[i].T0 + files[i].dT;
    dx[i] = 0;
    dy[i] = files[i].ddT;
  }
  SafeDelete(gr);
  gr = new TGraphErrors(NF,x,y,dx,dy);
  gr->Draw("axp");
  gr->Fit("pol1");
  TF1* pol1 = (TF1*) gr->GetListOfFunctions()->FindObject("pol1");
  if (pol1) {
    TF1* pl1 = new TF1("pl1","[0]+[1]*x",0.1,0.12);
    pl1->SetLineColor(2);
    pl1->SetParameters(0,23);
    pl1->FixParameter(1,23.);
    //pl1->Draw("same");
    gr->Fit(pl1,"er+");
  }
  TH1F *frame = gr->GetHistogram();
  frame->SetXTitle("time bucket (#mus)");
  frame->SetYTitle("T0 (#mus)");
  frame->SetTitle("Trigger T0 versus time bucket length");
  gr->Draw();
  return;
}
