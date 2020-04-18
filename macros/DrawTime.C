#include "TH1.h"
#include "TCanvas.h"
#include "TStyle.h"
#include "TDatime.h"
#include "TPolyMarker.h"
void DrawTime(TH1 *h, const Char_t *opt="%m\/%d\/%y"){//\%F2001-01-01 00:00:00") {// "%y\/%m\/%d\/%H%F1995-01-01 00:00:00") {
  //  FitP->Draw("sum_curr_1:utime-788936400","sum_curr_1>=-8&&sum_curr_1<=8&&utime>=1146339600&&utime<1146347789")
  //                               time of set
  if (! h) return;
  //  TDatime da(2001,6,1,0,0,0);
  //  TDatime da(1995,1,1,0,0,0);//0,0,0,0,0,0);
  //  gStyle->SetTimeOffset(da.Convert());
  //  gStyle->SetTimeOffset(-788936400); // - TDatime(19950101,0).Convert();  UNIX_TIMESTAMP("1995-01-01 00:00:00") = 788918400; for dEdx plots
  //  gStyle->SetTimeOffset(0);
  h->GetXaxis()->SetTimeDisplay(1);
  //  h->GetXaxis()->SetTimeFormat("%m\/%d\/%y%F2000-01-01 00:00:00");
  //  h->GetXaxis()->SetTimeFormat("%y%F1995-01-01 00:00:00");
  //  h->GetXaxis()->SetTimeFormat("%m\/%y");
  h->GetXaxis()->SetTimeFormat(opt);
  //  h->GetXaxis()->SetTimeOffset(da.Convert());   
  //  h->Draw();
  gPad->Modified();
  gPad->Update();
}
//________________________________________________________________________________
void DrawTimeU(TH1 *h, const Char_t *opt="%m\/%d\/%y"){//\%F2001-01-01 00:00:00") {// "%y\/%m\/%d\/%H%F1995-01-01 00:00:00") {
  // from unix time
  //  FitP->Draw("sum_curr_1:utime-788936400","sum_curr_1>=-8&&sum_curr_1<=8&&utime>=1146339600&&utime<1146347789")
  //                               time of set
  if (! h) return;
  //  TDatime da(1995,1,1,0,0,0);//0,0,0,0,0,0);
  TDatime da(2007,1,1,0,0,0);//0,0,0,0,0,0);
  gStyle->SetTimeOffset(da.Convert());
  //  gStyle->SetTimeOffset(-788936400); // - TDatime(19950101,0).Convert();  UNIX_TIMESTAMP("1995-01-01 00:00:00") = 788918400; for dEdx plots
  //  gStyle->SetTimeOffset(0);
  h->GetXaxis()->SetTimeDisplay(1);
  //  h->GetXaxis()->SetTimeFormat("%m\/%d\/%y%F2000-01-01 00:00:00");
  //  h->GetXaxis()->SetTimeFormat("%y%F1995-01-01 00:00:00");
  //  h->GetXaxis()->SetTimeFormat("%m\/%y");
  h->GetXaxis()->SetTimeFormat(opt);
  //  h->GetXaxis()->SetTimeOffset(da.Convert());   
  //  h->Draw();
  gPad->Modified();
  gPad->Update();
}
//________________________________________________________________________________
void TimePeriods(Int_t N, Int_t *tp) {
  TDatime t0(19950101,0);
  UInt_t u0 = t0.Convert();
  for (Int_t i = 0; i < N; i+=2) {
    TDatime t(tp[i],tp[i+1]);
    Double_t PositionX = (t.Convert() - u0);
    Double_t PositionY = -0.1;
    TPolyMarker *pm = new TPolyMarker(1, &PositionX, &PositionY);
    pm->SetMarkerStyle(20);
    //    pm->SetMarkerColor(kBlue);
    pm->SetMarkerColor(kRed);
    pm->SetMarkerSize(1.3);
    pm->Draw();
  }
}
//________________________________________________________________________________
void Run1_4() {
  Int_t tp[] = {
    20010924,000000,
    20031120,000000,
    20040104, 40500,
    20040205,180000,
    20040217,160000,
    20040324,210000,
    20040404,200000
  };
  Int_t N = sizeof(tp)/sizeof(Int_t);
  TimePeriods(N,tp);
}
//________________________________________________________________________________
void RunX() {
  Int_t tp[] = {
    20100103,      0, // AuAu200FF          day 003  
    20100107, 123300, // AuAu200FFLL        day 007
    20100204, 180000, // AuAu200RFF         day 035
    20100315, 160000, // LowLuminosity_2010 day 074
    20100318, 200000, // AuAu62RFF          day 077
    20100409,      0, // AuAu39RFF          day 098
    20100424,  40000, // AuAu7RFF           day 114
    20100527,  20000  // AuAu11RFF          day 147
  };
 Int_t N = sizeof(tp)/sizeof(Int_t);
  TimePeriods(N,tp);
}
//________________________________________________________________________________
void RunXIII() {
  Int_t tp[] = {
    20130313, 100000,
    20130403, 100000,
    20130417, 100000,
    20130503, 100000,
    20130508, 100000,
    20130522, 100000,
    20130523, 100000,
    20130605, 220000,
    20130611, 100000};
  Int_t N = sizeof(tp)/sizeof(Int_t);
  TimePeriods(N,tp);
}
//________________________________________________________________________________
void RunXX() {
  Int_t tp[] = {
    20191208,100905, 20191221,190032, 20200210,220428, 20200214,143742, 20200214,143742, // 11p5GeV                
    20191221,154021, 								         // 5p75GeV_fixedTarget    
    20200128,182912								       , // 31p2GeV_fixedTarget    
    20200130,  5840, 20200131, 50328, 20200131, 50328	                 	       , // 9p8GeV_fixedTarget     
    20200131, 12112, 20200131, 12112, 20200203, 20253				       , // 9p2GeV                 
    20200201,191904								       , // 19p5GeV_fixedTarget    
    20200202,160409								       , // 13p5GeV_fixedTarget    
    20200204, 53518								       , // 7p3GeV_fixedTarget     
    20200205,144626								       , // 9p2GeV                 
    20200213,152339, 20200213,152339						       , // 5p75GeV_fixedTarget    
    20200224,230740								         // 9p2GeVb                
  };
  Int_t N = sizeof(tp)/sizeof(Int_t);
  TimePeriods(N,tp);
}
/*
  Drift velocities:
2015 from sql tpcDriftVelocites
   T->Draw("lDvWest:utime-788936400>>DV(8000,634e6,642e6)","deactive==0&&utime>1420088400","prof")
   .x DrawTime.C(DV)
LaserPlots
     RunNT->Draw("1e-6*vWest:utime-788936400>>DV1(8000,1421e6,1432e6)","","prof") 
 */
