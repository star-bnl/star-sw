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
#include "TMultiGraph.h"
#include "TF1.h"
#include "TLegend.h"
#endif
TMultiGraph *mg = 0;
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
  run_t files[] = {
    //    {"2019/RF/DEV2/AuAu200",         			 0.0000, -0.013347, 0.000579, 9383180},
    {"2019/RF/DEV2/19GeV",           			 0.0000,  0.003988, 0.000116, 9341081}, 
    {"2019/RF/DEV2/14p5GeV",                             0.0000,  0.006306, 0.000170, 9307123}, 
    {"2019/RF/DEV2/9p2GeV",          			 0.0000, -0.001486, 0.000504, 9188688},
    {"2019/RF/DEV2/7p7GeV",          			 0.0000, -0.005144, 0.001492, 9104517},
#if 0
    {"2019/RF/DEV2.MySQL/14p5GeV",   			 2.3974, -0.015440, 0.000082, 9307123},
    {"2019/RF/DEV2.MySQL/19GeV",     			 2.3652,  0.007195, 0.006187, 9341081}, 
    {"2019/RF/DEV2.MySQL/7p7GeV",    			 2.4266, -0.003537, 0.001464, 9104517},
    {"2019/RF/DEV2.MySQL/9p2GeV",    			 2.4045, -0.007351, 0.000539, 9188688},
    {"2019/RF/DEV2.MySQL/AuAu200",   			 2.3422, -0.005598, 0.000229, 9383180},
    
    {"2019/RF/DEV2.StiPulls/14p5GeV",                    2.3974, -0.015793, 0.000172, 9307123},
    {"2019/RF/DEV2.StiPulls/19GeV.",                     2.3652,  0.005800, 0.000084, 9341081},
    {"2019/RF/DEV2.StiPulls/7p7GeV",                     2.4266, -0.002548, 0.001186, 9104517},
    {"2019/RF/DEV2.StiPulls/9p2GeV",         		 2.4045, -0.003028, 0.000934, 9188688},
    {"2019/RF/DEV2.StiPulls/AuAu200",        		 2.3422,  0.003679, 0.000239, 9383180}, 
#endif
    
    {"2020/RF/DEV2/11p5GeV",         			 0.0000,  0.005749, 0.000431, 9.3374e+06},
    //    {"2020/RF/DEV2/7p7GeV",          			 0.0000,  0.029763, 0.001482, 9.3321e+06},
    {"2020/RF/DEV2/9p2GeVc",         			 0.0000,  0.001152, 0.000354, 9.1887e+06},

    {"2021/RF/TFG21c.B/7p7GeV_2021",                     2.3904, -0.013218, 0.000008, 9.3321e+06}
  };
  Int_t NF = sizeof(files)/sizeof(run_t);
  Double_t *x = new Double_t[NF];
  Double_t *y = new Double_t[NF];
  Double_t *dx = new Double_t[NF];
  Double_t *dy = new Double_t[NF];
  TGraphErrors *gr[3] = {0};
  Int_t ii[3] = {0};
  if (mg) {
    SafeDelete(mg);
    for (Int_t k = 0; k < 3; k++) {
      SafeDelete(gr[k]);
      ii[k] = 0;
    }
  }
  for (Int_t i = 0; i < NF; i++) {
    x[i] = 1e9/files[i].freq;
    if (files[i].T0 > 0) {
      y[i] = files[i].T0 + files[i].dT -0.119 - 21 * 1e6/files[i].freq;
    } else {
      y[i] = files[i].dT;
    }
    y[i] *= 1e3;
    dx[i] = 0;
    dy[i] = 1e3*files[i].ddT + 1;
    TString name(files[i].file);
    Int_t k = -1;
    if      (name.Contains("2019")) k = 0;
    else if (name.Contains("2020")) k = 1;
    else if (name.Contains("2021")) k = 2;
    if (k < 0) continue;
    if (! gr[k]) {gr[k] = new TGraphErrors(); gr[k]->SetMarkerColor(k+1); ii[k] = 0;}
    gr[k]->SetPoint(ii[k],x[i],y[i]); gr[k]->SetPointError(ii[k],dx[i],dy[i]); ii[k]++;
    
  }
  mg = new TMultiGraph();
  TLegend *l = new TLegend(0.2,0.2,0.5,0.5);
  for (Int_t k = 0; k < 3; k++) {
    if (gr[0] && ii[k] > 0) {
      mg->Add(gr[k]); 
      if (k == 0) gr[k]->SetTitle("2019");
      if (k == 1) gr[k]->SetTitle("2020");
      if (k == 2) gr[k]->SetTitle("2021");
      l->AddEntry(gr[k],gr[k]->GetTitle());
    }
  }
  mg->Draw("ap");
  mg->Fit("pol1");
  TF1* pol1 = (TF1*) mg->GetListOfFunctions()->FindObject("pol1");
  if (pol1) {
    TF1* pl1 = new TF1("pl1","[0]+[1]*x",100,120);
    pl1->SetLineColor(2);
    pl1->SetParameters(435,-4);
    //    pl1->FixParameter(1,21.);
    pl1->Draw("same");
    //mg->Fit(pl1,"er+");
  }
  TH1F *frame = mg->GetHistogram();
  frame->SetXTitle("time bucket (nsec)");
  frame->SetYTitle("T0 (nsec)");
  frame->SetTitle("Trigger T0 versus time bucket length");
  mg->Draw();
  l->Draw();
  return;
}
