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
#if 0 /* mean +/- sigma */
#if 0 /* old schema */
  enum {NF = 5};
  run_t files[NF] = {
#if 0 
    {"2019/RF/AuAu200"      , 2.3904,-0.0482, 0.0201, 9383180}, //   20193001 Clock Frequency [Start / End]	9383180 / 9383180 Hz, RHIC Clock
    {"2019/RF/19GeV"        , 2.3675,-0.0023, 0.0106, 9341081}, // 9.3411e+06},
    {"2019/RF/14p5GeV"      , 2.3904, 0.0070, 0.0122, 9307123}, // 9.3071e+06},
    {"2019/RF/9p2GeV"       , 2.3904, 0.0141, 0.0152, 9188688},
    {"2019/RF/7p7GeV"       , 2.3904, 0.0362, 0.0144, 9104517}  // 9.1045e+06}
#else /* 03/28/21 */
    {"2019/RF/AuAu200"      , 2.3422,  0.0019, 0.0179, 9383180}, //   20193001 Clock Frequency [Start / End]	9383180 / 9383180 Hz, RHIC Clock
    {"2019/RF/19GeV"        , 2.3652,  0.0067, 0.0099, 9341081}, // 9.3411e+06},
    {"2019/RF/14p5GeV"      , 2.3974, -0.0146, 0.0109, 9307123}, // 9.3071e+06},
    {"2019/RF/9p2GeV"       , 2.4045, -0.0006, 0.0138, 9188688},
    {"2019/RF/7p7GeV"       , 2.4266,  0.0000, 0.0136, 9104517}  // 9.1045e+06}
#endif
  };
  /*
MySQL [Calibrations_tpc]> select * from tpcElectronicsB;
+--------+---------------------+--------+-----------+---------------------+---------------------+--------+---------------+
| dataID | entryTime           | nodeID | elementID | beginTime           | endTime             | flavor | tZero         |
+--------+---------------------+--------+-----------+---------------------+---------------------+--------+---------------+
|      1 | 2014-09-29 18:43:16 |     89 |         0 | 2000-01-01 00:00:00 | 2037-01-01 00:00:00 | ofl    | -0.1190000000 |
|      3 | 2014-09-29 20:12:49 |     89 |         0 | 2012-04-23 11:00:00 | 2037-01-01 00:00:00 | ofl    |  0.3422306861 | => 0.2478 with 23 time buckets trigger
|      2 | 2014-09-29 20:12:49 |     89 |         0 | 2012-12-10 00:00:00 | 2037-01-01 00:00:00 | ofl    | -0.1190000000 |
+--------+---------------------+--------+-----------+---------------------+---------------------+--------+---------------+
 bfc/.make/db/.const/StarDb/Calibrations/tpc/.tpcElectronicsB/tpcElectronicsB  Allocated rows: 1         Used rows: 1    Row size: 72 bytes
 Table: tpcElectronics_st            [0] :
 ======================================================================================
int     numberOfTimeBins             512 : 
double  nominalGain       82.065 : mV/fC  
double  samplingFrequency         9.3832 : MHz, not used,  overwritten by starClockOnl
double  tZero             -0.119 : us (microseconds)  
double  adcCharge           0.12 : fC/adc count  
double  adcConversion          2 : mV/adc count  
double  averagePedestal       50 : adc counts  
double  shapingTime          180 : ns  
double  tau                   55 : ns  
  */
#else /* new schema */
  enum {NF = 8};
  run_t files[NF] = {
    {"2019/RF/AuAu200"      , 0, -0.0143, 0.0166, 9383180}, //   20193001 Clock Frequency [Start / End]	9383180 / 9383180 Hz, RHIC Clock
    {"2019/RF/19GeV"        , 0,  0.0074, 0.0109, 9341081}, // 9.3411e+06},
    {"2020/RF/11p5GeV"      , 0,  0.0094, 0.0131, 9.3374e+06},
    {"2020/RF/7p7GeV"       , 0,  0.0388, 0.0235, 9.3321e+06},
    {"2019/RF/14p5GeV"      , 0,  0.0074, 0.0109, 9307123}, // 9.3071e+06},
    {"2019/RF/9p2GeV"       , 0, -0.0004, 0.0137, 9188688},
    {"2020/RF/9p2GeVc"      , 0,  0.0089, 0.0154, 9.1887e+06},
    {"2019/RF/7p7GeV"       , 0,  0.0012, 0.0134, 9104517}  // 9.1045e+06}
  };
#endif
  Double_t x[NF], y[NF], dx[NF], dy[NF];
#else /* mean +/- sigma of mean */
  run_t files[] = {
    {"2019/RF/DEV2/14p5GeV",                             0.0000,  0.006306, 0.000170, 9307123}, 
    {"2019/RF/DEV2/19GeV",           			 0.0000,  0.003988, 0.000116, 9341081}, 
    {"2019/RF/DEV2/7p7GeV",          			 0.0000, -0.005144, 0.001492, 9104517},
    {"2019/RF/DEV2/9p2GeV",          			 0.0000, -0.001486, 0.000504, 9188688},
    {"2019/RF/DEV2/AuAu200",         			 0.0000, -0.013347, 0.000579, 9383180},
#if 1
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
    
    {"2020/RF/DEV2/11p5GeV",         			 0.0000,  0.005749, 0.000431, 9.3374e+06},
    {"2020/RF/DEV2/7p7GeV",          			 0.0000,  0.029763, 0.001482, 9.3321e+06},
    {"2020/RF/DEV2/9p2GeVc",         			 0.0000,  0.001152, 0.000354, 9.1887e+06},

    {"2021/RF/TFG21c.B/7p7GeV_2021",                     2.3904, -0.013218, 0.000008, 9.3321e+06}
#endif
  };
  Int_t NF = sizeof(files)/sizeof(run_t);
  Double_t *x = new Double_t[NF];
  Double_t *y = new Double_t[NF];
  Double_t *dx = new Double_t[NF];
  Double_t *dy = new Double_t[NF];
#endif  
  for (Int_t i = 0; i < NF; i++) {
    x[i] = 1e9/files[i].freq;
    if (files[i].T0 > 0) {
      y[i] = files[i].T0 + files[i].dT -0.119 - 21 * 1e6/files[i].freq;
    } else {
      y[i] = files[i].dT;
    }
    y[i] *= 1e3;
    dx[i] = 0;
    dy[i] = files[i].ddT;
  }
  SafeDelete(gr);
  gr = new TGraphErrors(NF,x,y,dx,dy);
  gr->Draw("ap");
  gr->Fit("pol1");
  TF1* pol1 = (TF1*) gr->GetListOfFunctions()->FindObject("pol1");
  if (pol1) {
    TF1* pl1 = new TF1("pl1","[0]+[1]*x",0.1,0.12);
    pl1->SetLineColor(2);
    pl1->SetParameters(0,-4);
    //    pl1->FixParameter(1,21.);
    pl1->Draw("same");
    //gr->Fit(pl1,"er+");
  }
  TH1F *frame = gr->GetHistogram();
  frame->SetXTitle("time bucket (nsec)");
  frame->SetYTitle("T0 (nsec)");
  frame->SetTitle("Trigger T0 versus time bucket length");
  gr->Draw();
  return;
}
