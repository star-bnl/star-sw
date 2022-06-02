/* 
   Skew normal distribution https://en.wikipedia.org/wiki/Skew_normal_distribution
*/
#include "TMath.h"
#include "TF1.h"
#include "TH3.h"
#include "TFile.h"
#include "TAxis.h"
#include "TCanvas.h"
#include "Ask.h"
//________________________________________________________________________________
Double_t ggauso(Double_t *x, Double_t *p) {
  Double_t X = x[0];
  Double_t NormL = p[0];
  Double_t ksi = p[1];
  Double_t w = p[2];
  Double_t alpha = p[3];
  Double_t t = (X  - ksi)/w;
  Double_t v = t/TMath::Sqrt2();
  return TMath::Exp(NormL)*TMath::Gaus(t,0,1,kTRUE)/w*(1. + TMath::Erf(alpha*v));
}
//________________________________________________________________________________
Double_t ggaus(Double_t *x, Double_t *p) {
  Double_t X = x[0];
  Double_t NormL = p[0];
  Double_t mu    = p[1]; // Mode = ksi + w *m_0(alpha); most propable value
  Double_t sigma = p[2]; // Sqrt(Variance);
  Double_t alpha = p[3];
  Double_t ksi   = mu;
  Double_t w     = sigma;
  if (TMath::Abs(alpha) > 1e-7) {
    Double_t delta =alpha/TMath::Sqrt(1 + alpha*alpha);
    Double_t muz = delta/TMath::Sqrt(TMath::PiOver2());
    Double_t sigmaz = TMath::Sqrt(1 - muz*muz);
    Double_t gamma1 = (4 - TMath::Pi())/2 * TMath::Power(delta*TMath::Sqrt(2./TMath::Pi()), 3) 
      /                                     TMath::Power(1 - 2*delta*delta/TMath::Pi(), 1.5);
    Double_t m_0 = muz - gamma1*sigmaz/2 - TMath::Sign(1.,alpha)/2*TMath::Exp(-2*TMath::Pi()/TMath::Abs(alpha));
    w   = sigma/TMath::Sqrt(1 - 2* delta*delta/TMath::Pi()); 
    ksi = mu - w*m_0;
    //    Double_t mean = ksi + w * muz;
  }
  Double_t par[4] = {NormL, ksi, w, alpha};
  return ggauso(x, par);
}
//________________________________________________________________________________
TF1 *GGO() {
  static TF1 *f = 0;
  if (! f) {
    f = new TF1("GG",ggauso,-5,5,4);
    f->SetParameters(0,0,1,1);
    f->SetParNames("norl","mu","sigma","alpha");
  }
  return f;
}
//________________________________________________________________________________
TF1 *GG() {
  static TF1 *f = 0;
  if (! f) {
    f = new TF1("GG",ggaus,-5,5,4);
    f->SetParameters(0,0,1,1);
    f->SetParNames("norl","mu","sigma","alpha");
  }
  return f;
}
//________________________________________________________________________________
void Fit() {
  TF1 *gg = GG();
  TSeqCollection *files = gROOT->GetListOfFiles();
  if (! files) return;
  TIter next(files);
  TFile *f = 0;
  TCanvas *c1 = new TCanvas();
  while ( (f = (TFile *) next()) ) { 
    TString F(f->GetName());
    f->cd();
    TH3F *SecRow3C = (TH3F*) gDirectory->Get("SecRow3C");
    if (! SecRow3C) continue;
    cout << "file " << gDirectory->GetName() << endl;
    TH1D *SecRow3C_I = SecRow3C->ProjectionZ("I",0,-1,1,40);
    gg->SetParameter(1,SecRow3C_I->GetMean());
    SecRow3C_I->Fit(gg);
    c1->Update();
    if (Ask()) break;
    TH1D *SecRow3C_O = SecRow3C->ProjectionZ("O",0,-1,41,72);
    gg->SetParameter(1,SecRow3C_O->GetMean());
    SecRow3C_O->Fit(gg);
    c1->Update();
    if (Ask()) break;
  }
}
/*
root.exe [6] Fit()
file pion.root
 FCN=8088.79 FROM MIGRAD    STATUS=CONVERGED     305 CALLS         306 TOTAL
                     EDM=5.71861e-08    STRATEGY= 1      ERROR MATRIX ACCURATE 
  EXT PARAMETER                                   STEP         FIRST   
  NO.   NAME      VALUE            ERROR          SIZE      DERIVATIVE 
   1  norl         1.02592e+01   1.32353e-03   5.81265e-05  -2.74621e-02
   2  mu          -3.64091e-01   8.93121e-04   2.17691e-05   1.52104e-01
   3  sigma        6.57698e-01   1.08724e-03   2.89455e-05   4.16729e-01
   4  alpha        3.07007e+00   1.51039e-02   3.81546e-04  -2.68403e-03
ask (enter - next, r - don't ask, q - quit) >

 FCN=6974.51 FROM MIGRAD    STATUS=CONVERGED     266 CALLS         267 TOTAL
                     EDM=4.34939e-10    STRATEGY= 1  ERROR MATRIX UNCERTAINTY   1.5 per cent
  EXT PARAMETER                                   STEP         FIRST   
  NO.   NAME      VALUE            ERROR          SIZE      DERIVATIVE 
   1  norl         1.03398e+01   1.26811e-03  -9.07630e-07  -1.59961e-02
   2  mu          -3.41180e-01   8.42612e-04  -6.48368e-07  -5.86287e-02
   3  sigma        5.92697e-01   9.69501e-04   4.56398e-07  -5.13053e-02
   4  alpha        2.72469e+00   1.31387e-02   4.67244e-06  -8.87658e-04
ask (enter - next, r - don't ask, q - quit) >

file proton.root
 FCN=9141.32 FROM MIGRAD    STATUS=CONVERGED     228 CALLS         229 TOTAL
                     EDM=6.45913e-10    STRATEGY= 1  ERROR MATRIX UNCERTAINTY   2.4 per cent
  EXT PARAMETER                                   STEP         FIRST   
  NO.   NAME      VALUE            ERROR          SIZE      DERIVATIVE 
   1  norl         1.14187e+01   7.39594e-04   1.17661e-07  -3.74220e-02
   2  mu           1.01632e+00   4.76467e-04   4.49992e-08   2.90923e-02
   3  sigma        4.12448e-01   4.44282e-04  -1.35829e-07   1.59917e-01
   4  alpha        1.94506e+00   6.48417e-03   2.46872e-06  -9.00326e-03
ask (enter - next, r - don't ask, q - quit) >

 FCN=3674.45 FROM MIGRAD    STATUS=CONVERGED     256 CALLS         257 TOTAL
                     EDM=2.057e-07    STRATEGY= 1  ERROR MATRIX UNCERTAINTY   1.4 per cent
  EXT PARAMETER                                   STEP         FIRST   
  NO.   NAME      VALUE            ERROR          SIZE      DERIVATIVE 
   1  norl         1.11715e+01   8.43217e-04  -6.51347e-06  -8.66594e-01
   2  mu           1.08069e+00   5.88821e-04   1.57977e-06  -2.04191e+00
   3  sigma        3.43048e-01   4.65428e-04  -1.48984e-06  -1.29106e+00
   4  alpha        1.53515e+00   6.75541e-03  -5.00595e-06  -9.25151e-02
ask (enter - next, r - don't ask, q - quit) >

file electron.root
 FCN=7296.93 FROM MIGRAD    STATUS=CONVERGED     443 CALLS         444 TOTAL
                     EDM=2.19498e-09    STRATEGY= 1  ERROR MATRIX UNCERTAINTY   2.2 per cent
  EXT PARAMETER                                   STEP         FIRST   
  NO.   NAME      VALUE            ERROR          SIZE      DERIVATIVE 
   1  norl         1.02431e+01   1.29913e-03  -8.49012e-07  -1.98068e-02
   2  mu          -8.16461e-02   8.79437e-04  -3.82366e-06   8.70753e-02
   3  sigma        5.79787e-01   1.01020e-03   3.27019e-06   1.38725e-01
   4  alpha        2.68315e+00   1.42507e-02   5.46869e-05  -5.35566e-03
ask (enter - next, r - don't ask, q - quit) >

 FCN=3093.2 FROM MIGRAD    STATUS=CONVERGED     266 CALLS         267 TOTAL
                     EDM=1.24981e-07    STRATEGY= 1  ERROR MATRIX UNCERTAINTY   2.2 per cent
  EXT PARAMETER                                   STEP         FIRST   
  NO.   NAME      VALUE            ERROR          SIZE      DERIVATIVE 
   1  norl         1.01560e+01   1.39112e-03  -7.56400e-06   4.66316e-01
   2  mu          -3.70489e-02   8.83731e-04  -3.01263e-06   3.84405e-01
   3  sigma        5.36703e-01   9.17251e-04   1.53570e-06   2.72956e-01
   4  alpha        2.51769e+00   1.34143e-02   1.81343e-05  -2.93722e-04
ask (enter - next, r - don't ask, q - quit) >

file kaon.root
 FCN=11094.3 FROM MIGRAD    STATUS=CONVERGED     217 CALLS         218 TOTAL
                     EDM=4.78714e-08    STRATEGY= 1  ERROR MATRIX UNCERTAINTY   4.3 per cent
  EXT PARAMETER                                   STEP         FIRST   
  NO.   NAME      VALUE            ERROR          SIZE      DERIVATIVE 
   1  norl         1.07043e+01   1.05524e-03  -2.20983e-07   5.52234e-02
   2  mu           1.06022e-01   6.95070e-04  -8.08972e-07  -1.04931e+00
   3  sigma        5.71735e-01   8.00100e-04   1.14307e-06  -4.35938e-01
   4  alpha        2.66183e+00   1.08429e-02   1.13274e-05  -3.39177e-02
ask (enter - next, r - don't ask, q - quit) >

 FCN=6482.04 FROM MIGRAD    STATUS=CONVERGED     215 CALLS         216 TOTAL
                     EDM=3.22889e-07    STRATEGY= 1  ERROR MATRIX UNCERTAINTY   2.8 per cent
  EXT PARAMETER                                   STEP         FIRST   
  NO.   NAME      VALUE            ERROR          SIZE      DERIVATIVE 
   1  norl         1.06193e+01   1.10023e-03   5.39103e-06   4.25719e-01
   2  mu           1.52493e-01   6.68133e-04  -5.92729e-06   7.05178e-01
   3  sigma        5.16117e-01   7.31214e-04   7.65259e-06   1.61074e+00
   4  alpha        2.53074e+00   1.07130e-02   9.07561e-05   1.07480e-02
ask (enter - next, r - don't ask, q - quit) >

*/
