/*
root.exe 'Load.C("St_base,StChain")' st_laser_adc_14046081_raw.F.root Peaks2D.C+ 
root.exe 'Load.C("St_base,StChain")' st_laser_adc_14072106_raw.RF.root Peaks2D.C+ 
root.exe 'Load.C("St_base,StChain")' st_laser_adc_14044071_raw.ZF.root Peaks2D.C+ 

FPE_OFF
root.exe -q -b 'Load.C("St_base,StChain")' st_laser_14158028.ClnoW.root Peaks2D.C+ >& st_laser_14158028.log &
root.exe -q -b 'Load.C("St_base,StChain")' st_laser_14161023.ClnoW.root Peaks2D.C+ >& st_laser_14161023.log &
FPE_OFF
root.exe -q -b 'Load.C("St_base,StChain")' st_laser_14158028.ClnoWB.root Peaks2D.C+ >& st_laser_14158028.log &
root.exe -q -b 'Load.C("St_base,StChain")' st_laser_14161023.ClnoWB.root Peaks2D.C+ >& st_laser_14161023.log &
*/
//#define __Membrane__Only__
#if !defined(__CINT__) || defined(__MAKECINT__)
#include "Riostream.h"
#include <stdio.h>
#include "TMath.h"
#include "TH1.h"
#include "TF1.h"
#include "TArrayD.h"
#include "TSpectrum2.h"
#include "TCanvas.h"
#include "TRandom.h"
#include "TH2.h"
#include "TF2.h"
#include "TMath.h"
#include "TROOT.h"
#include "TFile.h"
#include "TSystem.h"
//#define __SPARSE__
#ifdef __SPARSE__
#include "THnSparse.h"
#else
#include "TH3.h"
#endif
#include "TFile.h"
#include "TNtuple.h"
#include "StEvtHddr.h"
#include "TGraphErrors.h"
#include "TGraph2DErrors.h"
#include "TVirtualFitter.h"
#endif
#include "Ask.h"
using namespace std;
Int_t maxpeaks = 500;
TFile *fOut = 0;
TNtuple *FitP = 0;
TSpectrum2 *spectr = 0;
Float_t FitParArray[40];
/* FitParArray[0] = sector + 100*tokens
              [1] = row
	      [2] = xfound
	      [3] = yfound
	      [4] = zp
	      [5] = ep
	      [6] - [5+2*nparX] parX +/- dparX
	      [6+2*nparX] = chisqx
	      [7+2*nparX] = NDFx
	      [8+2*nparXL3:7+2*NparX+2*NparY] parY +/- dparY
	      [8+2*NparX+2*NparY] = chisqy
	      [9+2*NparX+2*NparY] = NDFy
*/	      
// struct BPoint_t {
//   Float_t sector, row, x, y, 
//     NormX, dNormX, MuX, dMuX, SigmaX, dSigmaX, deltaX, ddeltaX, chisqX, NDFX, 
//     NormY, dNormY, MuY, dMuY, SigmaY, dSigmaY, deltaY, ddeltaY, chisqY, NDFY;
// };
static Int_t ngroup = 3; // 3
//const Char_t *vnames = "sector:row:x:y:NormX:dNormX:MuX:dMuX:SigmaX:dSigmaX:deltaX:ddeltaX:chisqX:NDFX:NormY:dNormY:MuY:dMuY:SigmaY:dSigmaY:deltaY:ddeltaY:chisqY:NDFY";
//BPoint_t BPoint;
//________________________________________________________________________________
Double_t freqF(Double_t *x, Double_t *par) {
  Double_t norm = par[0];
  Double_t mu   = par[1];
  Double_t sigma = par[2];
  Double_t delta = par[3];
  return TMath::Exp(norm)*(TMath::Freq((x[0]-mu+delta)/sigma) - TMath::Freq((x[0]-mu-delta)/sigma));
}
//________________________________________________________________________________
TF1 *FreqF() {
  TF1 *f = new TF1("FreqF",freqF,0,400,4);
  f->SetParNames("NormL","mu","sigma","delta");
  f->SetParameters(10,0,1,0.1);
  f->SetParLimits(2,1e-7,2);
  f->SetParLimits(3,1e-7,2);
  return f;
}
//________________________________________________________________________________
Double_t freqG(Double_t *x, Double_t *par) {
  Double_t norm = par[0];
  Double_t mu   = par[1];
  Double_t sigma = par[2];
  Double_t delta = par[3];
  if (TMath::Abs(delta) < 1e-7) delta = 1e-7;
  return TMath::Exp(norm)*(TMath::Freq((x[0]-mu+delta)/sigma) - TMath::Freq((x[0]-mu-delta)/sigma))*sigma/(2*delta) + par[4];
    //    TMath::Exp(par[4])*TMath::Gaus(x[0],mu,par[5]);
}
//________________________________________________________________________________
TF1 *FreqG() {
  TF1 *f = new TF1("FreqG",freqG,0,400,5);
  f->SetParNames("NormL","mu","sigma","delta","ped");//,"normL","sigma2");
  static Double_t dx = ((Double_t) ngroup)/2;
  f->SetParameters(10,0,1,dx,0);
  f->SetParLimits(0,-20,80);
  f->SetParLimits(2,1e-7,20);
#if 0
  f->FixParameter(3,dx);
#else
  f->SetParLimits(3,1e-7,20);
  f->SetParLimits(4,0.,1.);
#endif
  return f;
}
//________________________________________________________________________________
Double_t student(Double_t *x, Double_t *par) {
  Double_t norm = par[0];
  Double_t mu   = par[1];
  Double_t sigma = par[2];
  Double_t ndf   = par[3];
  if (ndf <= 0) ndf = 1;
  return TMath::Exp(norm)*TMath::Student((x[0]-mu)/sigma,ndf) + par[4];
}
//________________________________________________________________________________
TF1 *Student() {
  TF1 *f = new TF1("Student",student,0,400,5);
  f->SetParNames("NormL","mu","sigma","ndf","noise");//,"normL","sigma2");
  f->SetParameters(10,0,1,80,0);
  f->SetParLimits(0,-20,80);
  f->SetParLimits(2,0.5,20);
  f->SetParLimits(3,0.1,180);
  //  f->FixParameter(4,0);
  return f;
}
//________________________________________________________________________________
Double_t fpeaks2(Double_t *x, Double_t *par) {
  Double_t norm   = par[0];
  Double_t mean1  = par[1];
  Double_t sigma1 = par[2];
  Double_t mean2  = par[3];
  Double_t sigma2 = par[4];
  return norm*TMath::Gaus(x[0],mean1,sigma1)*TMath::Gaus(x[1],mean2,sigma2);
}
//________________________________________________________________________________
TF1 *Z() {
  TF1 *f = new TF1("Z","[0]*TMath::GammaDist([2]-x,[1],0,[3]+[4])",0,400);
  f->SetParNames("Norm","gamma","mu","beta","offset");
  f->SetParameters(1e4,2.45,240,0.25,0);
  return f;
}
//________________________________________________________________________________
Double_t gammaB(Double_t *x, Double_t *par) {
  // Gamma function distribution integrated over bin 1
  Double_t normL = par[0];
  Double_t x0    = par[1]; // start of 
  Double_t a     = par[2];
  Double_t noise = par[3];
  static Double_t dx = ((Double_t) ngroup)/2;
  Double_t x1    = x[0] - x0 - dx;
  Double_t x2    = x1 + 2*dx;
  Double_t G     = 0;
  if (x2 > 0) {
    if (x1 < 0) x1 = 0;
    G = TMath::Gamma(a,x2) - TMath::Gamma(a,x1);
  }
  return noise + TMath::Exp(normL)*G;
}
//________________________________________________________________________________
TF1 *GammaB() {
  static TF1 *f = 0;
  if (! f)  {
    f = new TF1("GammaB",gammaB,0,400,4);
    f->SetParNames("NormL","x0","#alpha","noise");
    f->SetParameters(0,10,3,0.);
    f->SetParLimits(2,1,10);
    f->FixParameter(3,0.);
  }
  return f;
}
//________________________________________________________________________________
Double_t gammaB2(Double_t *x, Double_t *par) {
  // Gamma function distribution integrated over bin 1
  Double_t normL = par[0];
  Double_t MuX   = par[1]; // Maximum 
  Double_t x0    = par[2]; // start of 
  Double_t frac  = TMath::Sin(par[3])*TMath::Sin(par[3]);
  Double_t a2    = par[4];
  Double_t noise = par[5];
  Double_t tau1  = par[6];
  Double_t tau2  = par[7];
  Double_t tmax = (MuX - x0)/tau1;
  Double_t a    = tmax - 1;
  if (a < 1) return 0;
  static Double_t dx = ((Double_t) ngroup)/2;
  Double_t x1    = x[0] - x0 - dx;
  Double_t x2    = x1 + 2*dx;
  Double_t G     = 0;
  if (x2 > 0) {
    if (x1 < 0) x1 = 0;
    G = (1 - frac)*(TMath::Gamma(a,x2/tau1) - TMath::Gamma(a,x1/tau2));
    if (frac > 0) {
      G += frac*(TMath::Gamma(a2,x2/tau2) - TMath::Gamma(a2,x1/tau2));
    }
  }
  return TMath::Exp(normL)*G + noise;
}
//________________________________________________________________________________
TF1 *GammaB2() {
  static TF1 *f = 0;
  if (! f)  {
    f = new TF1("GammaB2",gammaB2,0,400,8);
    f->SetParNames("Norm","Mu","x0","frac","alpha2","noise","tau1","tau2");
    f->SetParameters(0.,10.,3.,0.,30.,0,1.,1.);
    f->SetParLimits(0,-10,10);
    //    f->SetParLimits(2,2,5);
    f->SetParLimits(3,0,TMath::Pi()/2);
    f->SetParLimits(4,5,100);
    f->SetParLimits(5,0,10);
    f->SetParLimits(6,0.1,100.0);
    f->SetParLimits(7,1,100);
    f->FixParameter(3,0);
    f->FixParameter(4,30);
    f->FixParameter(5,0);
    f->FixParameter(7,1);
  }
  return f;
}
//________________________________________________________________________________
Double_t gausgamma(Double_t *x, Double_t *par) {
  // Gamma function distribution integrated over bin 1
  static Double_t delta = ((Double_t) ngroup)/2;
  Double_t NormL = par[0];
  Double_t MuX   = par[1]; // Maximum 
  Double_t sigma = par[2];
  Double_t G     = TMath::Exp(NormL)*(TMath::Freq((x[0]-MuX+delta)/sigma) - TMath::Freq((x[0]-MuX-delta)/sigma));
  Double_t frac  = TMath::Exp(NormL)*TMath::Sin(par[3])*TMath::Sin(par[3]);
  Double_t x0    = par[4]; // start of 
  Double_t alpha = par[5];
  Double_t tau  = par[6];
  if (alpha < 1) return G;
  if (frac > 0) {
    Double_t x1    = x[0] - x0 - delta;
    Double_t x2    = x1 + 2*delta;
    if (x2 > 0) {
      if (x1 < 0) x1 = 0;
      G += frac*(TMath::Gamma(alpha,x2/tau) - TMath::Gamma(alpha,x1/tau));
    }
  }
  return G;
}
//________________________________________________________________________________
TF1 *GausGamma() {
  static TF1 *f = 0;
  if (! f)  {
    f = new TF1("GausGamma",gausgamma,0,400,7);
    f->SetParNames("Norm","Mu","sigma","frac","x0","alpha","tau");
    f->SetParameters(0.,0.,1., 0., 0., 1., 1.);
    f->SetParLimits(0,-10,10);
    f->SetParLimits(2,0.2,5);
    f->SetParLimits(3,0,TMath::Pi()/2);
    f->SetParLimits(5,0.1,100.0);
    f->SetParLimits(6,1,100);
    f->FixParameter(3,0);
    f->FixParameter(4,30);
    f->FixParameter(5,1);
    f->FixParameter(7,1);
  }
  return f;
}
//________________________________________________________________________________
Double_t expB2(Double_t *x, Double_t *par) {
  // Two exponential function distribution integrated over bin 1
  Double_t normL = par[0];
  Double_t x0    = par[1]; // start of 
  Double_t a     = par[2];
  Double_t Sin   = TMath::Sin(par[3]);
  Double_t frac  = Sin*Sin;
  Double_t a2    = par[4];
  static Double_t dx = ((Double_t) ngroup)/2;
  Double_t x1    = x[0] - x0 - dx;
  Double_t x2    = x1 + 2*dx;
  Double_t G     = 0;
  if (x2 > 0) {
    if (x1 < 0) x1 = 0;
    G = (1 - frac)*(TMath::Exp(-a*x1) - TMath::Exp(-a*x2))/a;
    if (frac > 0) {
      G += frac*(TMath::Exp(-a2*x1) - TMath::Exp(-a2*x2))/a2;
    }
  }
  return TMath::Exp(normL)*G;
}
//________________________________________________________________________________
TF1 *ExpB2() {
  static TF1 *f = 0;
  if (! f)  {
    f = new TF1("ExpB2",expB2,0,400,5);
    f->SetParNames("NormL","x0","#alpha","frac","#alpha2");
    f->SetParameters(0,10,3,0.,30.);
    f->SetParLimits(0,-10,10);
    f->SetParLimits(2,0.1,5);
    f->SetParLimits(3,0,TMath::Pi()/2);
    f->SetParLimits(4,5,100);
  }
  return f;
}
//________________________________________________________________________________
Double_t logNorm(Double_t *x, Double_t *par) {
  // Log-Normal distribution 
  Double_t normL = par[0];
  Double_t mu    = par[1];
  Double_t sigma = par[2];
  Double_t G = TMath::Gaus(TMath::Log(x[0]),TMath::Log(mu),sigma,kTRUE)/x[0];
  return TMath::Exp(normL)*G;
}
//________________________________________________________________________________
TF1 *LogNorm() {
  static TF1 *f = 0;
  if (! f)  {
    f = new TF1("LogNorm",logNorm,0,400,3);
    f->SetParNames("NormL","#mu","#sigma");
    f->SetParameters(0,10,3,0.,30.);
    f->SetParLimits(0,-10,10);
    f->SetParLimits(2,0.1,5);
  }
  return f;
}
//________________________________________________________________________________
TF1 *gausF() {
  static TF1 *f = 0;
  if (! f)  {
    f = new TF1("gauss","[0]*TMath::Gaus(x,[1],[2])",0,400);
    f->SetParNames("Norm","mu","sigma");
  }
  return f;
}
//________________________________________________________________________________
Double_t gammaI(Double_t *x, Double_t *par) {
  // Gamma function distribution integrated over bin 1
  // Control constants
  static Double_t np = 100.0;      // number of convolution steps
  static Double_t sc =   5.0;      // convolution extends to +-sc Gaussian sigmas
  Double_t normL = par[0];
  Double_t x0    = par[1]; // start of 
  Double_t alpha = par[2];
  Double_t tau   = par[3];
  Double_t sigma = par[4];
  // Range of convolution integral
  Double_t xlow = x[0] - sc * sigma;
  Double_t xupp = x[0] + sc * sigma;
  Double_t step = (xupp-xlow) / np;
  Double_t sum = 0;
  // Convolution integral of Gamma and Gaussian by sum
  Double_t xx;
  for(Int_t i = 1.0; i <= np/2; i++) {
    xx = xlow + (i-.5) * step;
    if (xx > x0) sum += TMath::GammaDist(xx,alpha,x0,tau) * TMath::Gaus(x[0],xx,sigma,kTRUE);
    xx = xupp - (i-.5) * step;
    if (xx > x0) sum += TMath::GammaDist(xx,alpha,x0,tau) * TMath::Gaus(x[0],xx,sigma,kTRUE);
  }
  return TMath::Exp(normL) * step *sum;
 }
//________________________________________________________________________________
TF1 *GammaI() { // Gamma function convoluted with Gaus
  static TF1 *f = 0;
  if (! f)  {
    f = new TF1("GammaB",gammaI,0,400,5);
    f->SetParNames("NormL","Mu","#alpha","tau","sigma");
    f->SetParameters(0,10,3,1.,1.);
    f->SetParLimits(0,-10,10);
    f->SetParLimits(2,0.5,10);
    f->SetParLimits(3,1.0,10);
    f->SetParLimits(4,1.0,10);
  }
  return f;
}
//________________________________________________________________________________
Double_t langauI(Double_t *x, Double_t *par) {
  // Gamma function distribution integrated over bin 1
  // Control constants
  static Double_t np = 1000.0;      // number of convolution steps
  static Double_t sc =   5.0;      // convolution extends to +-sc Gaussian sigmas
  Double_t normL = par[0];
  Double_t mvp   = par[1]; 
  Double_t sigmaL= par[2];
  Double_t sigmaG= par[3];
  // Range of convolution integral
  Double_t xlow = x[0] - sc * sigmaG;
  Double_t xupp = x[0] + sc * sigmaG;
  Double_t step = (xupp-xlow) / np;
  Double_t sum = 0;
  // Convolution integral of Landau and Gaussian by sum
  Double_t xx;
  for(Int_t i = 1.0; i <= np/2; i++) {
    xx = xlow + (i-.5) * step;
    sum += TMath::Landau(xx,mvp,sigmaL,kTRUE) * TMath::Gaus(x[0],xx,sigmaG,kTRUE);
    xx = xupp - (i-.5) * step;
    sum += TMath::Landau(xx,mvp,sigmaL,kTRUE) * TMath::Gaus(x[0],xx,sigmaG,kTRUE);
  }
  return TMath::Exp(normL) * step *sum + par[4];
 }
//________________________________________________________________________________
TF1 *LangauI() { // Landau function convoluted with Gaus
  static TF1 *f = 0;
  if (! f)  {
    f = new TF1("LangauB",langauI,0,400,5);
    f->SetParNames("NormL","mvp","#sigmaL","sigma","ped");
    f->SetParameters(0,10,3,1.,0.);
    f->SetParLimits(0,-10,80);
    f->SetParLimits(2,0.01,10);
    f->SetParLimits(3,1.0,5.0);
    f->SetParLimits(4,0.0,5.0);
  }
  return f;
}
//________________________________________________________________________________
Double_t expI(Double_t *x, Double_t *par) {
  // Exponential function convoluted with Gaussina
  Double_t normL = par[0];
  Double_t MuX   = par[1]; // start of 
  Double_t tau   = par[2];
  Double_t sigma = par[3];
  Double_t frac  = TMath::Sin(par[4])*TMath::Sin(par[4]);
  Double_t tau2  = par[5];
  Double_t y = x[0] - MuX;
#if 0
  static Double_t SqPi = TMath::Sqrt(TMath::Pi());
  static Double_t Sq2  = TMath::Sqrt(2.);
  Double_t sigma2    = sigma*sigma;
  Double_t sOvetY = (sigma2)/(2*tau*tau) - y/tau; 
  Double_t EsOvetY = TMath::Exp(sOvetY);
  Double_t erfarg = Sq2*(tau*y - sigma2)/(2*sigma*tau);
  // *(limit  TMath::Erf((sqrt(2)*tau*y  + sqrt(2)*tau*x0 - sqrt(2)*tau*x - sqrt(2)*sigma2)/(2*sigma*tau))) => 1
  Double_t G =  (SqPi*sigma*EsOvetY*TMath::Erf(erfarg)/Sq2 - (SqPi*sigma*EsOvetY )/Sq2)/(Sq2*SqPi*sigma*tau);
#else
  Double_t st = sigma/tau;
  Double_t rexp = -y/tau + 0.5*st*st;
  if (rexp > 80) rexp = 80;
  Double_t  G  = TMath::Exp(rexp)/tau*(0.5*(TMath::Erfc((-y + sigma*st)+1)/TMath::Sqrt(2.)/sigma));
  if (frac > 0) {
    G *= (1. - frac);
    Double_t st2 = sigma/tau2;
    G += frac*(TMath::Exp(rexp)/tau2*(0.5*(TMath::Erfc((-y + sigma*st2)+1)/TMath::Sqrt(2.)/sigma)));
  }
#endif
  return TMath::Exp(normL) * G;
 }
//________________________________________________________________________________
TF1 *ExpI() { // Gamma function convoluted with Gaus
  static TF1 *f = 0;
  if (! f)  {
    f = new TF1("ExpI",expI,0,400,6);
    f->SetParNames("NormL","Mu","tau","sigma","frac","tau2");
    f->SetParameters(0,10,1.,1.,0.,10.);
    f->SetParLimits(0,-10,10);
    f->SetParLimits(2,0.1,100);
    f->SetParLimits(3,0.1,100);
    f->SetParLimits(4,0,TMath::Pi()/2);
    //    f->FixParameter(4,0);
    f->SetParLimits(5,2.0,100);
    //    f->FixParameter(5,10);
  }
  return f;
}
#if 0
TF1 *gfreqG = FreqG();
TF1 *g1 = new TF1("g1","gausn",0,400);
#else
//  TF1 *g1[2] = {GammaB(), Student()};
//  TF1 *g1[2] = {GammaB2(), FreqG()};
//  TF1 *g1[2] = {ExpB2(), FreqG()};
//  TF1 *g1[2] = {LogNorm(), FreqG()};
//  TF1 *g1[2] = {GausGamma(), FreqG()};
//  TF1 *g1[2] = {GammaI(), FreqG()};
//  TF1 *g1[2] = {ExpI(), FreqG()};
//TF1 *g1[2] = {Student(), Student()};
//TF1 *g1[2] = {Student(), FreqG()};
TF1 *g1[2] = {LangauI(), FreqG()};
#endif
//________________________________________________________________________________
Bool_t FindPeaks(TH2D *h2 = 0) {
  if (! h2) return kFALSE;
  if (! spectr) spectr = new TSpectrum2(2*maxpeaks,1);
  Double_t xmin   = h2->GetXaxis()->GetXmin();
  Double_t xmax   = h2->GetXaxis()->GetXmax();
  Double_t ymin   = h2->GetYaxis()->GetXmin();
  Double_t ymax   = h2->GetYaxis()->GetXmax();
  Int_t nbinsx    = h2->GetXaxis()->GetNbins();
  Int_t nbinsy    = h2->GetYaxis()->GetNbins();
  Double_t dx     = (xmax-xmin)/nbinsx;
  Double_t dy     = (ymax-ymin)/nbinsy;
#if 0
  f2->SetNpx(5*nbinsx);
  f2->SetNpy(5*nbinsy);
#endif
  //   f2->SetParameters(par);
  TCanvas *c1 = 0;
  TCanvas *c2 = 0;
  Bool_t ask = kTRUE;
  if (! gROOT->IsBatch()) {
    c1 = (TCanvas*)gROOT->GetListOfCanvases()->FindObject("c1");
    if (!c1) c1 = new TCanvas("c1","c1");
    c1->cd();
    c1->SetLogz(1);
    c2 = (TCanvas*)gROOT->GetListOfCanvases()->FindObject("c2");
    if (!c2) c2 = new TCanvas("c2","c2",800,600);
  }
  //now the real stuff: Finding the peaks
  //   Int_t nfound = spectr->Search(h2,2,"colnobackgroundnomarkov");
  //  Int_t nfound = spectr->Search(h2,2,"colnomarkov",0.01);
  Int_t nfound = spectr->Search(h2,2,"colnomarkov");
  if (c1) {c1->cd(); h2->Draw("colz"); c1->Update();}
  //searching good and ghost peaks (approximation)
  Float_t *xpeaks = spectr->GetPositionX();
  Float_t *ypeaks = spectr->GetPositionY();
  //searching good and ghost peaks (approximation)
  cout << "Found " << nfound << endl;
  spectr->Print();
  Double_t defPar[2][20];
  g1[0]->GetParameters(defPar[0]);
  g1[1]->GetParameters(defPar[1]);
  const Int_t window = 10;
  for (Int_t pf = 0; pf < nfound; pf++) {
    // Check significance
    Float_t xp = xpeaks[pf];
    Int_t binx = h2->GetXaxis()->FindBin(xp);
    Float_t yp = ypeaks[pf];
    Int_t biny = h2->GetYaxis()->FindBin(yp);
    Double_t zp = h2->GetBinContent(binx,biny);
    Double_t ep = h2->GetBinError(binx,biny);
    //    if (zp  < 1) continue;
    if (zp-1.25*ep < 0) continue;
    Double_t x = xpeaks[pf];
    Double_t y = ypeaks[pf];
#if 0
    if (! pf) tuple = &BPoint.NormX;
    else      tuple = &BPoint.NormY;
    BPoint.x = x;
    BPoint.y = y;
#else
    FitParArray[2] = x;
    FitParArray[3] = y;
    FitParArray[4] = zp;
    FitParArray[5] = ep;
#endif
    Double_t norm = h2->GetBinContent(binx,biny);
    if (norm < 1) continue;
#ifdef __Menbrane_Only__
    if (x < 350) continue; // Membrane only 
#endif /* __Menbrane_Only__ */
    Int_t binxMin = binx-window/dx;
    Int_t binxMax = binx+window/dx;
    Int_t binyMin = biny-window/dy;
    Int_t binyMax = biny+window/dy;
    for (Int_t pa = 0; pa < nfound; pa++) {
      if (pa == pf) continue;
      if (TMath::Abs(xpeaks[pf]-xpeaks[pa]) < window && TMath::Abs(ypeaks[pf]-ypeaks[pa]) < window) {
	Int_t bx = 0.5*(xpeaks[pf]+xpeaks[pa])/dx;
	Int_t by = 0.5*(ypeaks[pf]+ypeaks[pa])/dy;
	if (xpeaks[pf] < xpeaks[pa] && bx < binxMax) binxMax = bx;
	if (xpeaks[pf] > xpeaks[pa] && bx > binxMin) binxMin = bx;
	if (ypeaks[pf] < ypeaks[pa] && by < binyMax) binyMax = by;
	if (ypeaks[pf] > ypeaks[pa] && by > binyMin) binyMin = by;
      }
    }
    Int_t binx1, binx2;
    for (binx1 = binx-1; binx1 >       0 && binx1 > binxMin; binx1--) if (h2->GetBinContent(binx1,biny) < 0.1) break;
    for (binx2 = binx+1; binx2 <= nbinsx && binx2 < binxMax; binx2++) if (h2->GetBinContent(binx2,biny) < 0.1) break;
    binx1 = binxMin;
    binx2 = binxMax;
    Int_t biny1, biny2;
    for (biny1 = biny-1; biny1 >       0 && biny1 > binyMin; biny1--) if (h2->GetBinContent(binx,biny1) < 0.1) break;
    for (biny2 = biny+1; biny2 <= nbinsy && biny2 < binyMax; biny2++) if (h2->GetBinContent(binx,biny2) < 0.1) break;
    biny1 = binyMin;
    biny2 = binyMax;
    cout << "Peak " << pf 
	 << " x = " << x << "[" << binx1 << "," << binx2 << "] = [" << xmin + dx*(binx1-1) << "," << xmin + dx*binx2 << "]" 
	 << " y = " << y << "[" << biny1 << "," << biny2 << "] = [" << ymin + dy*(biny1-1) << "," << ymin + dy*biny2 << "]" << endl;
    if (binx1 >= binx2 - 1) continue;
    if (biny1 >= biny2 - 1) continue;
      h2->GetXaxis()->SetRange(binx1,binx2); 
      h2->GetYaxis()->SetRange(biny1,biny2);
    if (c2) {
      c2->Clear();
      c2->Divide(2,1);
    }
    TH1D *proj[2] = {0, 0};
    //    TGraphErrors *grafs1D[2] = {0, 0};
    Int_t    NpXY[2] = {g1[0]->GetNpar(), g1[1]->GetNpar()};
    Float_t *tuple[2] = {&FitParArray[6], &FitParArray[8+2*NpXY[0]]};
    Int_t fitStatus[2];
    for (Int_t xy = 0; xy < 2; xy++) {
      if (! xy) cout << "x Fit ===========" << endl;
      else      cout << "y Fit ===========" << endl;
      if (c2) c2->cd(xy+1);
      Double_t var;
      Double_t varMin, varMax;
      memset(tuple[xy], 0, 2*(NpXY[xy]+1)*sizeof(Float_t));
      g1[xy]->SetParameters(defPar[xy]);
      if (! xy) proj[xy] = h2->ProjectionX();
      else      proj[xy] = h2->ProjectionY();
      if (! proj[xy]) continue;
      proj[xy]->SetName(Form("%s_%i",proj[xy]->GetName(),pf)); 
      Double_t varMean = proj[xy]->GetMean();
      Double_t varRMS  = proj[xy]->GetRMS();
      if (! xy) {
	var = varMean;
	varMin = varMean-3*varRMS;
	varMax = varMean+5*varRMS;
      } else {
	var = y;
	varMin = varMean-3*varRMS;
	varMax = varMean+3*varRMS;
      }
      if (c2) {proj[xy]->Draw(); c2->Update();}
      TFitResultPtr r;
      for (Int_t k = 0; k < 1; k++) {
	if (k == 0) {
	  g1[xy]->SetParameter(0, TMath::Log(norm));
	  g1[xy]->SetParameter(1,var);
	  g1[xy]->SetParameter(1,varMean);
	  g1[xy]->SetParameter(2,varRMS);
	  fitStatus[xy] = proj[xy]->Fit(g1[xy],"er","",varMin,varMax);
	  if (fitStatus[xy]) break;
	  for (Int_t j = 0; j < NpXY[xy]; j++) {
	    tuple[xy][2*j  ] = g1[xy]->GetParameter(j);
	    tuple[xy][2*j+1] = g1[xy]->GetParError(j);
	  }
	  tuple[xy][2*NpXY[xy]  ] =  g1[xy]->GetChisquare();
	  tuple[xy][2*NpXY[xy]+1] =  g1[xy]->GetNDF();
	}
	if (c2) 	c2->Update();
      }
    }
    //    FitP->Fill(&BPoint.sector);
    if (! fitStatus[0] || ! fitStatus[0]) FitP->Fill(FitParArray);
    if (! gROOT->IsBatch() && (ask = Ask())) return ! ask;
    if (proj[0]) {proj[0]->Write(); SafeDelete(proj[0]);}
    if (proj[1]) {proj[1]->Write(); SafeDelete(proj[1]);}
  }
  g1[0]->SetParameters(defPar[0]);
  g1[1]->SetParameters(defPar[1]);
  h2->GetXaxis()->SetRange(0,0);
  h2->GetYaxis()->SetRange(0,0);
  if (! gROOT->IsBatch() && ask) return ! ask;
   
  return kTRUE;
}
//________________________________________________________________________________
void Peaks2D(Int_t sector = 0, Int_t padrow = 0, Int_t tokensS = -2) {
  if (! gDirectory) return;
  TVirtualFitter::SetDefaultFitter("Minuit");
  //  TVirtualFitter::SetDefaultFitter("Minuit2");
  TDirectory *ff = gDirectory;
  TString Name(gSystem->BaseName(gDirectory->GetName()));
#if 0
  Double_t NoEvents = 2592;
  if (Name.Contains("14044071")) NoEvents = 1000;
  if (Name.Contains("14047009")) NoEvents = 2000;
  if (Name.Contains("14047011")) NoEvents = 4998;
  if (Name.Contains("14047013")) NoEvents = 4998;
  if (Name.Contains("14047014")) NoEvents = 1985;
  if (Name.Contains("14047039")) NoEvents = 2000;
  if (Name.Contains("14069042")) NoEvents = 2087;
  if (Name.Contains("14070047")) NoEvents = 1992;
  if (Name.Contains("14072091")) NoEvents = 2152;
#else
  Double_t NoEvents = 1;
#endif
#if 1
  StEvtHddr *header = 0;
  StEvtHddr *hC = 0;
  Int_t l = 1;
  ULong_t uS = 1970614020;
  while ((hC = (StEvtHddr *) ff->Get(Form("EvtHddr;%i",l)))) {
    ULong_t u = hC->GetUTime();
    if (u < uS) {
      uS = u;
      header = hC;
    }
    l++;
  }
#endif
  Name.ReplaceAll(".root","");
  Name += Form(".Fit.g%i.LangauIFreQ.",ngroup);
  if (sector > 0) {
    Name += Form("sec%02i0",sector);
  } else {
    Name += "All";
  }
  if (padrow > 0) Name += Form(".r%i",padrow);
  if (tokensS >= 0) Name += Form(".t%i",tokensS);
  Name += ".root";
  fOut = new TFile(Name,"recreate");
#if 1
  if (header) header->Write();
#endif
  TString Vnames("sector:row:X:Y:zP:eP");
  const Char_t *XY[2] = {"X","Y"};
  for (Int_t xy = 0; xy < 2; xy++) {
    Int_t N = g1[xy]->GetNpar();
    for (Int_t i = 0; i < N; i++) {
      Vnames += ":";  Vnames += g1[xy]->GetParName(i); Vnames += XY[xy];
      Vnames += ":d"; Vnames += g1[xy]->GetParName(i); Vnames += XY[xy];
    }
    Vnames += ":chisq"; Vnames += XY[xy];
    Vnames += ":NDF";   Vnames += XY[xy];
  }
  Vnames.ReplaceAll("#","");
  FitP = new TNtuple("FitP","Fit results",Vnames.Data());
  Int_t secMin = 1;
  Int_t secMax = 24;
  Int_t rowMin = 1;
  Int_t rowMax = 45;
  if (sector) { secMin = secMax = sector;}
  if (padrow) { rowMin = rowMax = padrow;}
  //  ff->cd();
  Bool_t ok = kTRUE;
  for (Int_t sec = secMin; sec <= secMax; sec++) {
    Int_t t1 =  -1;
    Int_t t2 = 110;
    if (tokensS > -2) {t1 = t2 = tokensS;}
    for (Int_t tokens = t1; tokens < t2; tokens++) {
      TString hName(Form("AvLaser_%02i",sec));
      if (tokens >= 0) hName += Form("_%03i",tokens); 
#ifdef __SPARSE__
      THnSparseT<TArrayF> *AvLaser = (THnSparseT<TArrayF> *) ff->Get(hName);
#else
      TH3F *AvLaser = (TH3F *) ff->Get(hName);
#endif
      if (! AvLaser) {
	cout << "sector = " << sec << " no histogram" << endl;
	continue;
      } else {
	cout << "sector = " << sec << " found histogram " << AvLaser->GetName() << endl;
      }
      FitParArray[0] = sec + 100*(tokens+1);
      TH1F *events = (TH1F *) ff->Get(tokens < 0 ? "SectorCounts_000" : Form("SectorCounts_%03i",tokens));
      if (events) {
	//      NoEvents = events->GetBinContent(sec+1);
	NoEvents = events->GetBinContent(1);
	cout << events->GetName() << "\t" << NoEvents << " events" << endl;
	if (NoEvents < 1) continue; 
      }
      //    BPoint.sector = sec;
      for (Int_t row = rowMin; row <= rowMax; row++) {
	FitParArray[1] = row;
	//      BPoint.row = row;
#ifdef __SPARSE__
	AvLaser->GetAxis(0)->SetRange(row,row);
	TH2D *h2 = (TH2D *) AvLaser->Projection(1,2);
#else
	AvLaser->GetXaxis()->SetRange(row,row);
	TH2D *h2 = (TH2D *) AvLaser->Project3D("yz");
#endif
	h2->SetName(Form("AvLaser_%02i_%02i",sec,row));
	//      Double_t fmax = h2->GetMaximum()/NoEvents;
	//      Double_t fentries = h2->GetEntries();
	Double_t cut = h2->GetSumOfWeights()/h2->GetEntries()/NoEvents;
	// Averaging
	Int_t ngroup2 = ngroup/2;
	const Int_t npads[45] = 
	  {      88,    96,   104,   112,   118,   126,   134,   142,   150,   158,
		 166,   174,   182,
		 98 ,  100,   102,   104,   106,   106,   108,   110,   112,   112,
		 114,  116,   118,   120,   122,   122,   124,   126,   128,   128,
		 130,  132,   134,   136,   138,   138,   140,   142,   144,   144,    
		 144,  144};
	TH2D *h2new = 0;
	if (ngroup == 1) {
	  h2new = new TH2D(*h2);
	  h2new->SetName(Form("%s_new",h2->GetName()));
	} else {
	  h2new = new TH2D(Form("%s_new",h2->GetName()), Form("%s at row = %i",h2->GetTitle(),row),
			   h2->GetXaxis()->GetNbins(),h2->GetXaxis()->GetXmin(),h2->GetXaxis()->GetXmax(),
			   npads[row-1],0.5,0.5+npads[row-1]);
	  Int_t nx1 = h2new->GetXaxis()->GetFirst();
#ifdef __Membrane__Only__
	  nx1 = 350;
#endif
	  Int_t nx2 = h2new->GetXaxis()->GetLast();
	  Int_t ny1 = h2new->GetYaxis()->GetFirst();
	  Int_t ny2 = TMath::Min(h2new->GetYaxis()->GetLast(),npads[row-1]);
	  for (Int_t ix = nx1 + ngroup2; ix <= nx2 - ngroup2; ix++) {
	    for (Int_t iy = ny1 + ngroup2; iy <= ny2 - ngroup2; iy++) {
	      Double_t sum = 0;
	      Double_t sum2w = 0;
	      Int_t    nent = 0;
	      Int_t    iX = 0; 
	      Int_t    iY = 0;
	      for (Int_t ixx = TMath::Max(1,ix - ngroup2);  ixx <= TMath::Min(nx2, ix + ngroup2); ixx++) {
		for (Int_t iyy = TMath::Max(1,iy - ngroup2);  iyy <= TMath::Min(ny2, iy + ngroup2); iyy++) {
		  nent++;
		  iX += ixx;
		  iY += iyy;
		  
		  sum += h2->GetBinContent(ixx,iyy);
		  Double_t e = h2->GetBinError(ixx,iyy);
		  sum2w += e*e;
		}
	      }
	      if (! nent) continue;
	      iX /= nent;
	      iY /= nent;
	      sum /= nent;
	      sum /= NoEvents;
	      //	  if (sum < 0.25) continue;
	      if (sum < cut) continue;
	      sum2w /= nent;
	      sum2w = TMath::Sqrt(sum2w);
	      sum2w /= NoEvents;
	      sum2w += 0.01;
	      h2new->SetBinContent(iX,iY,sum);
	      h2new->SetBinError(iX,iY,sum2w);
	    }
	  }
	}
	//      h2->Write();
	h2new->SetStats(0);
	h2new->SetYTitle(h2->GetYaxis()->GetTitle());
	h2new->SetXTitle(h2->GetXaxis()->GetTitle());
	delete h2;
	h2 = h2new;
	//      return;
	ok = FindPeaks(h2);
	if (! ok) break;;
	h2->Write();
	delete h2;
      }
      if (! ok) break;
      delete AvLaser;
      delete events;
    }
    if (! ok) break;
  }
  fOut->Write();
}
/********************************************************************************
03/08/13
gStyle->SetOptStat(0) 
TF1 *f = new TF1("f","[0]+[1]*TMath::Sin(TMath::DegToRad()*(30*x+[2]))",0.4,24.5);
f->SetParameters(369,0.1,0)
FitP->Draw("mvpX+T0X(sector,row):sector>>So(24,0.5,24.5,100,367,372)","dmuY>0&&dmuY<0.1&&dmvpX<0.1&&mvpX>350&&row>13","colz")
So->FitSlicesY()
FitP->Draw("mvpX+T0X(sector,row):sector>>Si(24,0.5,24.5,100,367,372)","row<=13","colz")
FitP->Draw("mvpX:sector>>Si(24,0.5,24.5,100,369,370)","row<=13 && NormLX > 1 && NormLY > 1 && NDFX > 1 && NormLY > 1 && chisqX/NDFX < 2000 && chisqY/NDFY < 2000 && dmvpX < 0.04 && dmuY < 0.04","colz")
FitP->Draw("mvpX:sector>>So(24,0.5,24.5,100,370,371)","row> 13 && NormLX > 1 && NormLY > 1 && NDFX > 1 && NormLY > 1 && chisqX/NDFX < 2000 && chisqY/NDFY < 2000 && dmvpX < 0.04 && dmuY < 0.04","colz")

********************************************************************************/
