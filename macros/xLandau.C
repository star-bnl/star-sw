#include "TMath.h"
#include "TF1.h"
#include "TF2.h"
#include "TH1.h"
#include "TDirectory.h"
#include "TGraphErrors.h"
#include "Math/WrappedTF1.h"
#include <Math/SpecFuncMathCore.h>
#include "Math/BrentRootFinder.h"
#include "Riostream.h"
TF1 * fitFunc = 0;  // fit function pointer (need to be globel since it is used by myFuncGradient )
const int NPAR = 2; // number of function parameters;

Double_t landauZ(Double_t *x, Double_t *par) {
  Double_t xd = x[0];
  Double_t meand = par[0];
  Double_t sigmad = par[1];
  Double_t mpshift  = -0.22278298; // 1.0844535734 + 0.61417;       // LandauZ maximum location
  // MP shift correction
  Double_t mpc = TMath::Exp(meand) - mpshift * sigmad; 
  Double_t xx = TMath::Exp(xd);
  return xx*TMath::Landau(xx,mpc,sigmad);
}
//________________________________________________________________________________
TF1 *LandauZ(Double_t xmin = -2, Double_t xmax = 5) {
  TF1 *f = new TF1("LandauZ",landauZ,xmin, xmax,2);
  f->SetParameter(0,0); f->SetParName(0,"#mu");
  f->SetParameter(1,0.07); f->SetParName(1,"#sigma");
  f->SetNpx(1000);
  fitFunc = f;
  return f;
}
//________________________________________________________________________________
Double_t df_dPar(Double_t *x, Double_t *p) {
  // use calculated derivatives from TF1::GradientPar
  Double_t grad[NPAR]; 
  // p is used to specify for which parameter the derivative is computed 
  Int_t ipar = Int_t(p[0] ); 
  assert (ipar >=0 && ipar < NPAR );

   assert(fitFunc);
   fitFunc->GradientPar(x, grad);


   return grad[ipar]; 
}
//________________________________________________________________________________
TF1 *dmuLandauZ() {
  TF1 *f = new TF1("dLandauZ",df_dPar,-5,5,2);
  f->SetParameter(0,1); f->SetParName(0,"par");
  return f;
}
//________________________________________________________________________________
Double_t  dfdz(Double_t *x, Double_t *p) {
  return fitFunc->Derivative(x[0]);
}
//________________________________________________________________________________
TF1 *dLandauZdZ() {
  TF1 *f = new TF1("dLandauZdZ",dfdz,-5,5);
  return f;
}
//________________________________________________________________________________
Double_t  NumericalRootFinder(Double_t xmin = -1, Double_t xmax = 1) {
  if (! fitFunc) LandauZ();
  TF1 f("dLandauZdZ",dfdz,xmin, xmax);
  
  ROOT::Math::WrappedTF1 wf1(f);
  
  // Create the Integrator
  ROOT::Math::BrentRootFinder brf;
  
  // Set parameters of the method
  brf.SetFunction( wf1, xmin, xmax );
  brf.Solve();
  
  cout << brf.Root() << endl;
  return  brf.Root();
}
//________________________________________________________________________________
Double_t mlandau(Double_t *x, Double_t *p) {
  //  return ROOT::Math::landau_cdf(x[0]);
  return TMath::Landau(x[0]);
}
//________________________________________________________________________________
TF1 *LandauF() {
  static TF1* f = 0;
  if (! f) f = new TF1("LandauF",mlandau,-2,5,0);
  return f;
}
//________________________________________________________________________________
Double_t landau(Double_t *x, Double_t *p) {
  //  return ROOT::Math::landau_cdf(x[0]);
  Double_t lNorm = p[0];
  Double_t mu    = p[1];
  Double_t sigma = p[2];
  return TMath::Exp(lNorm)*TMath::Landau(x[0],mu,sigma,kFALSE);
}
//________________________________________________________________________________
TF1 *Landau() {
  static TF1* f = 0;
  if (! f) f = new TF1("Landau",landau,0,10,3);
  f->SetParameters(1,2.,5.);
  f->SetParNames("l","#mu","#sigma");
  f->SetParLimits(0,-50,50);
  f->SetParLimits(2,1e-3,10);
  return f;
}
//________________________________________________________________________________
Double_t LadauDeriv(Double_t *t, Double_t *p) {
  Double_t sigma = p[0];
  Double_t mu    = p[1];
  Double_t val = sigma*LandauF()->Eval(t[0]) + (mu*sigma*t[0])*LandauF()->Derivative(t[0]);
}
//________________________________________________________________________________
Double_t RootFinder(Double_t *x, Double_t *p) {
  TF1 f("dLandauZdZ",LadauDeriv,-1, 1,2);
  f.SetParameter(0,x[0]);
  f.SetParameter(1,x[1]);
  ROOT::Math::WrappedTF1 wf1(f);
  // Create the Integrator
  ROOT::Math::BrentRootFinder brf;
  
  // Set parameters of the method
  brf.SetFunction( wf1, -0.25, 0.25 );
  brf.Solve();
  
  cout << "sigma = " << x[0] << "\tmu = " << x[1] << " root = " << brf.Root() << endl;
  return  brf.Root();
}
//________________________________________________________________________________
Double_t Y0(Double_t *x, Double_t *p) {
  TF1 f("dLandauZdZ",LadauDeriv,-1, 1,2);
  f.SetParameter(0,x[0]);
  f.SetParameter(1,x[1]);
  return f.GetX(0.-0.25,0.25);
}
//________________________________________________________________________________
TF2 *MuSigma() {
  //  return new TF2("MuSigma",RootFinder,0.,1.,-1.,1.,0);
  return new TF2("MuSigma",RootFinder,0.,1.,-1.,1.,0);
}
//________________________________________________________________________________
TGraphErrors *Hist2Graph(TH1D *hist, TGraphErrors **grr2) {
  Int_t Nx = hist->GetNbinsX();
  Double_t *x = new Double_t[Nx];
  Double_t *y = new Double_t[Nx];
  Double_t *e = new Double_t[Nx];
  Double_t *y2 = new Double_t[Nx];
  Double_t *e2 = new Double_t[Nx];
  Int_t N = 0;
  TString name(hist->GetName());
  name.ReplaceAll("_1","_2");
  TH1D *hist2 = (TH1D *) gDirectory->Get(name); 
  for (Int_t ix = 1; ix <= Nx; ix++) {
    if (hist2) {
      y2[N] = hist2->GetBinContent(ix);
      if (y2[N] < 0.1) continue;
      e2[N] = hist->GetBinError(ix);
      if (e2[N] < 1e-4 || e2[N] > 0.1) continue;
    }
    y[N] = hist->GetBinContent(ix);
    if (y[N] < 1.0 || y[N] > 3.0) continue;
    e[N] = hist->GetBinError(ix);
    if (e[N] < 1e-4 || e[N] > 0.1) continue;
    x[N] = hist->GetBinCenter(ix);
    N++;
  }
  TGraphErrors *grr = new TGraphErrors(N, x, y, 0, e);
  if (grr2) {
    *grr2 = new TGraphErrors(N, x, y2, 0, e2);
  }
  delete [] x;
  delete [] y;
  delete [] e;
  delete [] y2;
  delete [] e2;
  return grr;
}
//________________________________________________________________________________
void xLandau() {}
/*
  TH2D *nPdT = new TH2D(*nPdTO);
  nPdT->SetName("nPdT");
  nPdT->Add(nPdTI);
  nPdT->FitSlicesY(L,0,-1,100,"qnrs5");
  TGraphErrors *gr2;
  .L xLandau.C+
  TGraphErrors *gr1 = Hist2Graph(nPdT_1,&gr2);
  gr1->Fit("pol2","rob=0.75");
  gr1->Fit("pol2","e");

 FCN=371.65 FROM MINOS     STATUS=SUCCESSFUL     40 CALLS         213 TOTAL
                     EDM=1.73206e-14    STRATEGY= 1      ERROR MATRIX ACCURATE 
  EXT PARAMETER                                   STEP         FIRST   
  NO.   NAME      VALUE            ERROR          SIZE      DERIVATIVE 
   1  p0           7.78840e-02   2.64074e-02  -2.96420e-04   5.28108e-05
   2  p1           5.05311e-01   1.21984e-02   1.38648e-04   2.14641e-04
   3  p2          -2.35543e-02   1.40059e-03   1.40059e-03   5.90905e-03

  gr2->Fit("pol1","rob=0.75");
  gr2->Fit("pol1","e");

 FCN=258.99 FROM MINOS     STATUS=SUCCESSFUL     10 CALLS          55 TOTAL
                     EDM=2.92497e-19    STRATEGY= 1      ERROR MATRIX ACCURATE 
  EXT PARAMETER                                   STEP         FIRST   
  NO.   NAME      VALUE            ERROR          SIZE      DERIVATIVE 
   1  p0           3.68953e-01   4.78212e-03   7.78614e-12  -6.39244e-09
   2  p1          -2.36952e-02   1.08578e-03   1.08578e-03   2.81543e-08

root.exe [82] gr1->Fit("pol4","e")
 FCN=1347.57 FROM MINOS     STATUS=FAILURE       504 CALLS        2874 TOTAL
                     EDM=3.04912e-08    STRATEGY= 1      ERR MATRIX NOT POS-DEF
  EXT PARAMETER                APPROXIMATE        STEP         FIRST   
  NO.   NAME      VALUE            ERROR          SIZE      DERIVATIVE 
   1  p0           1.40288e+00   5.31911e-04  -4.43186e-04  -3.59028e-05
   2  p1          -9.23883e-01   3.76130e-04   8.36314e-04  -1.65182e-05
   3  p2           1.13463e+00   1.70839e-04  -5.78104e-04   7.10740e-05
   4  p3          -3.39133e-01   7.24330e-05   1.73451e-04  -2.50536e-04
   5  p4           3.32941e-02   1.90608e-05   1.90608e-05   6.10459e+01
root.exe [105] gr2->Fit("pol5","er","",1.5,3)
 FCN=249.931 FROM MINOS     STATUS=FAILURE       613 CALLS        4160 TOTAL
                     EDM=4.68746e-08    STRATEGY= 1      ERR MATRIX NOT POS-DEF
  EXT PARAMETER                APPROXIMATE        STEP         FIRST   
  NO.   NAME      VALUE            ERROR          SIZE      DERIVATIVE 
   1  p0          -3.83245e+00   5.41036e-04  -3.85114e-04  -3.85172e-02
   2  p1           7.85250e+00   3.60830e-04   5.04231e-04   3.15202e-01
   3  p2          -5.40561e+00   1.71457e-04  -9.38538e-05  -1.00825e+00
   4  p3           1.56341e+00   7.61825e-05  -1.28635e-04   1.57877e+00
   5  p4          -1.42479e-01   3.14225e-05   6.77928e-05  -9.63637e-01
   6  p5          -6.64725e-03   9.49021e-06   9.49021e-06   1.66063e+02
 */
