#include "Riostream.h"
#include <stdio.h>
#include "TROOT.h"
#include "TSystem.h"
#include "TAxis.h"
#include "TH1.h"
#include "TH2.h"
#include "TSystem.h"
#include "TMath.h"
#include "StdEdxModel.h"
#include "Bichsel.h"
//#include "StMessMgr.h" 
using namespace std;
ClassImp(StdEdxModel)
StdEdxModel  *StdEdxModel::fgStdEdxModel = 0;
TH1D         *StdEdxModel::mdNdx = 0;  
Double_t      StdEdxModel::fScale = 1; //TMath::Exp(9.12015e-02); // Bichsel
Int_t         StdEdxModel::_debug   = 1;
TF1          *StdEdxModel::fGGaus = 0;
TF1          *StdEdxModel::fGausExp = 0;
Double_t      StdEdxModel::fTmaxL10eV = 5; // Tcut = 100 keV
Double_t      StdEdxModel::shift2keV = 0;
Double_t      StdEdxModel::shift2GeV = 0;
Double_t      StdEdxModel::shift2eV = 0;
Double_t StdEdxModel::GeVperElectron               = 43.5e-9;  //39.41e-9;  // 24.95e-9; // deposited energy per conducting electron 
Double_t StdEdxModel::LogGeVperElectron = TMath::Log(43.5e-9); //39.41e-9); // TMath::Log(24.95e-9);
TF1 *StdEdxModel::fpol2F = 0;
TF1 *StdEdxModel::fpol5F = 0;
TF1 *StdEdxModel::fpol6F = 0;
Bool_t StdEdxModel::fOld = kFALSE;
//________________________________________________________________________________
StdEdxModel* StdEdxModel::instance() {
  if (! fgStdEdxModel) new StdEdxModel();
  return fgStdEdxModel;
}
//________________________________________________________________________________
StdEdxModel::StdEdxModel() {
  //  LOG_INFO << "StdEdxModel:: use StTpcRSMaker model for dE/dx calculations" << endm;
  cout << "StdEdxModel:: use StTpcRSMaker model for dE/dx calculations" << endl;
  if (! fgStdEdxModel) {
    TDirectory *dir = gDirectory;
    fgStdEdxModel = this;
    const Char_t *path  = ".:./StarDb/dEdxModel:$STAR/StarDb/dEdxModel";
    const Char_t *Files[1] = {"dNdx_Bichsel.root"};
    for (Int_t i = 0; i < 1; i++) { // files
      Char_t *file = gSystem->Which(path,Files[i],kReadPermission);
      if (! file) Fatal("StdEdxModel","File %s has not been found in path %s",Files[i],path);
      else        Warning("StdEdxModel","File %s has been found as %s",Files[i],file);
      TFile       *pFile = new TFile(file);
      mdNdx    = (TH1D *)         pFile->Get("dNdx");     if (mdNdx)    mdNdx->SetDirectory(0);
      assert(mdNdx);
      delete pFile;
      delete [] file;
    }
    dir->cd();
  }
  fGGaus = new TF1("GGaus",ggaus, -1., 5., 5);
  fGGaus->SetParNames("NormL","mu","sigma","alpha","k");
  fGGaus->SetParameters(0,0,0.3,0.1,0);
  fGGaus->FixParameter(4,0.0);

  fGausExp = new TF1("GausExp",gausexp, -5., 5., 5);
  fGausExp->SetParNames("NormL","mu","sigma","k","l");
  fGausExp->SetParameters(0,0,0.3,5.,0);
  fGausExp->FixParameter(4,0.0);
  shift2eV  = Bichsel::Instance()->MostProbableZShift() - 7.26742600141722234e-02;
  shift2keV = TMath::Log(1e-3) + shift2eV;
  shift2GeV = TMath::Log(1e-9) + shift2eV;
}
//________________________________________________________________________________
StdEdxModel::~StdEdxModel() {
  fgStdEdxModel = 0;
  SafeDelete(mdNdx);
  SafeDelete(fGGaus);
}
//________________________________________________________________________________
Double_t StdEdxModel::dNdx(Double_t poverm, Double_t charge) {
  if (!fgStdEdxModel) instance();
  fTmaxL10eV = tmaxL10eV(poverm);
  if (mdNdx)    return fScale*charge*charge*mdNdx->Interpolate(poverm);
  return 0;
}
//________________________________________________________________________________
Double_t StdEdxModel::gausw(Double_t *x, Double_t *p) {
  // Skew normal distribution https://en.wikipedia.org/wiki/Skew_normal_distribution
  Int_t    k = p[4]; // switch between value and derivatives
  Double_t X = x[0];
  Double_t NormL = p[0];
  Double_t ksi = p[1];
  Double_t w = p[2];
  Double_t alpha = p[3];
  Double_t t = (X  - ksi)/w;
  Double_t v = t/TMath::Sqrt2();
  Double_t G = TMath::Exp(NormL)*TMath::Gaus(t,0,1,kTRUE);
  Double_t E = (1. + TMath::Erf(alpha*v));
  Double_t V = G/w*E;
  if (k == 0) return V;
  Double_t dVdNormL = V;
  if (k == 1) return dVdNormL;
  /*
    V            =  G / w * E
    dt           = - 1/w * dksi  - t/w *dw = - (dksi + dw)/w
    dG 		 =  G * dt                 = - G * (dksi + dw)/w  = - V * (dksi + dw)/E
    dv           = dt/sqrt(2)              = - (dksi + dw)/w/sqrt(2)
    GA           = Gaus(alpha*v)
    dE           = GA*(dalpha * v + alpha*dv) = GA  * ( dalpha *v - alpha * (dksi + dw)/w/sqrt(2))
    dV           =   dG                 / w * E - G / w * E * dw/ w + G / w * dE 
                 = - G * (dksi + dw)/w  / w * E -        V * dw / w + G / w * GA  * ( dalpha *v - alpha * (dksi + dw)/w/sqrt(2))
		 = V ( - (dksi + dw)/w          -            dw / w + GA/E         *( dalpha *v - alpha * (dksi + dw)/w/sqrt(2)))
		 = V ( - (dksi + 2* dw)/w                           + GA/E         *( dalpha *v - alpha * (dksi + dw)/w/sqrt(2)))
		 
    dV/dksi      = V ( -  dksi          /w                          - GA/E                       *alpha *  dksi      /w/sqrt(2))
                 = V ( -  1             /w                          - GA/E                       *alpha              /w/sqrt(2))
                 = - V/w*(1 + GA/E*alpha/sqrt(2))
    dV/dw        = V ( -         2* dw /w                           - GA/E                        alpha *       + dw /w/sqrt(2))
                 = - V/w *(2 + GA/E*alpha/sqrt(2))
    dV/dalpha    = GA*v

  */
/* maxima
t(ksi,w) := (x - ksi)/w;
v(ksi,w) := t(ksi,w)/sqrt(2);
G(ksi,w) := gaussprob(t(ksi,w))/w;
E(ksi,w,alpha) := 1 + erf(alpha*v(ksi,w));
Val(ksi,w,alpha):= gaussprob(t(ksi,w))/w * ( 1 + erf(alpha*v(ksi,w)));
G : jacobian([Val(ksi,w,alpha)], [ksi,w,alpha]);
trigsimp(%);
fortran(%);

*/
  Double_t GA = TMath::Gaus(alpha*v,0,1,kTRUE);
  Double_t dVdksi = - V/w*(1 + GA/E*alpha/TMath::Sqrt2());
  if (k == 2) return dVdksi;
  Double_t dVdw   = - V/w*(2 + GA/E*alpha/TMath::Sqrt2());
  if (k == 3) return dVdw;
  Double_t dVdalpha = GA*v;
  return dVdalpha;
}
//_______________________________________________________________________________
Double_t StdEdxModel::ggaus(Double_t *x, Double_t *p) {
  //  Int_t    k     = p[4];
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
  return gausw(x, par);
#if 0 /* Derivatives */
  /* Maxima 
     load (f90) $
     :lisp (setq *f90-output-line-length-max* 1000000000)
     stardisp: true$
     delta(alpha) := alpha/sqrt(1 + alpha*alpha);
     muz(alpha) := delta(alpha)/sqrt(%pi/2);
     sigmaz(alpha) := sqrt(1 - muz(alpha)*muz(alpha));
     gamma1(alpha) := (4 - %pi)/2 * (delta(alpha)/sqrt(%pi/2))**3 / (1 - 2*delta(alpha)**2 /%pi)**1.5;
     signn(x) := x/abs(x);
     m_0(alpha) := muz(alpha) - gamma1(alpha)/sigmaz(alpha)/2 - signn(alpha)/2*exp(-2*%pi/abs(alpha));
     w(sigma,alpha):= sigma/sqrt(1 - 2*delta(alpha)**2/%pi);
     ksi(mu,sigma,alpha):= mu - w(sigma,alpha)*m_0(alpha);
     
     F : jacobian([  ksi(mu,sigma,alpha), w(sigma,alpha), alpha], [mu, sigma, alpha]);
     trigsimp(%);
     f90(%);
     
     t(ksi,w) := (x - ksi)/w;
     v(ksi,w) := t(ksi,w)/sqrt(2);
     G(ksi,w) := 1./sqrt(2*%pi)*exp(-t(ksi,w)**2/2);
     E(ksi,w,alpha) := 1 + erf(alpha*v(ksi,w));
     Val(ksi,w,alpha):= G(ksi,w)/w *E(ksi,w,alpha);
     
     G : jacobian([Val(ksi,w,alpha)], [ksi,w,alpha]);
     trigsimp(%);
     f90(%);
     
     T: G . F; 
     trigsimp(%);
     A : %;
     f90(A);
     with_stdout ("A.txt",  f90(A));
  */
#endif
}
//_______________________________________________________________________________
Double_t StdEdxModel::gausexp(Double_t *x, Double_t *p) {
  // Souvik Das, "A simple alternative to the Crystal Ball function"
  // https://arxiv.org/pdf/1603.08591.pdf
  //  Int_t    l     = p[4];// 
  Double_t normL = p[0];
  Double_t mu    = p[1];
  Double_t sigma = p[2];
  Double_t kh    = p[3];
  Double_t t     = (x[0] - mu)/sigma;
  Double_t V     = 0.;
  Double_t k     = kh;
  if (kh < 0) {
    k = - kh;
    t = - t;
  }
  if (t < k) {
    V = TMath::Exp(-t*t/2); // der = - V * t => - k * V
  } else {
    V = TMath::Exp(k*k/2 - k*t); // der =       - k * V 
  }
// (%i5) integrate(exp(((-t)*t)/2),t,minf,k)
//                                        k
//                       sqrt(%pi) erf(-------)
//                                     sqrt(2)    sqrt(%pi)
// (%o5)                 ---------------------- + ---------
//                              sqrt(2)            sqrt(2)
// _

// (%i6) integrate(exp((k*k)/2-k*t),t,k,inf)
// Is k positive, negative or zero?

// positive
// ;
//                                          2
//                                         k
//                                       - --
//                                         2
//                                     %e
// (%o6)                               ------
//                                       k
  static Double_t SQ2pi = TMath::Sqrt(TMath::Pi())/TMath::Sqrt2();
  Double_t D = SQ2pi*(1 + TMath::Erf(k/TMath::Sqrt2()));
  Double_t C = TMath::Exp(-k*k/2)/k;
  Double_t N = (D + C)*sigma;
  return TMath::Exp(normL)*V/N;
}
//________________________________________________________________________________
/* CERN-77-09 Sauli Yellow report
   Gas  dens(g/cm**3) Wi (ev)   dE/dx (keV/cm) np (1/cm)  nT(1/cm)     nT/np          
   Ar   1.66e-3        26       2.44           29.4       94           3.2
   CH4  6.70e-4        13.1     2.21           16         53           3.3
   P10  1.561ee-3      25.44    2.43           28.82      92.24        3.2     TpcRS = <nT/np> = 2.47       
   P10: (dE/dx)/nT   = 2.43/92.24 = 26.3  eV : W = 25.4 eV                     TpcRS W = 24.3 
   Ar:  (dE/dx)/nT   = 2.44/94    = 25.96 eV : W = 26   eV
*/
//________________________________________________________________________________
void StdEdxModel::Parameters(Double_t Np, Double_t *parameters, Double_t *derivatives) {
  parameters[0] = parameters[1] = parameters[2] = 0;
  if (Np <= 1.0) return;
  Double_t &mu    = *&parameters[0];
  Double_t &sigma = *&parameters[1];
  Double_t &k     = *&parameters[2];
  if (fOld) {
    // Most Probable log (ne/Np) versus log of Np
    static Double_t parsMu1[7]  = {    3.0767,  -0.054267,   -0.69433,    0.71462,    0.12861,   -0.29135,   0.072728};
    static Double_t parsMu2[7]  = {   0.33622,     3.2863,    -1.3061,    0.27857,  -0.032664,  0.0019922, -4.944e-05};
    static Double_t pars6Sig[7] = {   0.82995,   0.033141,   -0.36552,   -0.18875,     0.2694,  -0.073444,  0.0038834}; // log(log(fitX))
    static Double_t parsk[7]    = {    -8.959,     13.454,    -6.6549,     1.6064,   -0.20527,     0.0133, -0.00034269};
    if (! fpol5F || ! fpol6F) InitPar();
    Double_t fitX = TMath::Log(Np);
    if (fitX < 0.5) fitX = 0.5;
    Double_t X = TMath::Log(fitX);
    if (fitX < 2) mu = fpol6F->EvalPar(&fitX, parsMu1);
    else          mu = fpol6F->EvalPar(&fitX, parsMu2);
    sigma = fpol6F->EvalPar(&X, pars6Sig);
    k = 6.70823e-01;
    if (fitX > 2) {
      k = fpol6F->EvalPar(&fitX, parsk);
    }
  } else { // new version based on 100 keV Tcut
    Double_t x = TMath::Log(Np);
    mu    = muPar(x);
    sigma = sigmaPar(x);
    k     = a0Par(x);
  }
  return;
}
//________________________________________________________________________________
Double_t StdEdxModel::Parameter(Double_t Np, Int_t l) {
  Double_t params[3];
  Parameters(Np, params);
  return params[l];
}
//________________________________________________________________________________
Double_t StdEdxModel::Prob(Double_t /* log(nE/Np) */ ee, Double_t Np) {
  Double_t params[3] = {0};
  Parameters(Np, &params[1]);
  Double_t V = gausexp(&ee, params);
  return V;
}
//________________________________________________________________________________
Double_t StdEdxModel::ProbdEGeVlog(Double_t dEGeVLog, Double_t Np) {
  Double_t params[3] = {0};
  Parameters(Np, &params[1]);
  Double_t ee = dEGeVLog - shift2GeV - TMath::Log(Np);
  Double_t V = gausexp(&ee, params);
  return V;
}
//________________________________________________________________________________
Double_t StdEdxModel::zMPold(Double_t *x, Double_t *p) {
  fOld = kTRUE;
  Double_t log10bg = x[0];
  Double_t pOverM  = TMath::Power(10., log10bg);
  Double_t log2dx  = p[0];
  Double_t charge  = p[1];
  Double_t dx      = TMath::Power( 2., log2dx);
  Double_t dNdx = StdEdxModel::instance()->dNdx(pOverM, charge);
  Double_t Np = dNdx*dx;
  Double_t dEkeVLog = StdEdxModel::instance()->LogdEMPVkeV(Np); 
  Double_t dEdxLog  = dEkeVLog - TMath::Log(dx);
  return   dEdxLog;
}
//________________________________________________________________________________
TF1 *StdEdxModel::ZMPold(Double_t log2dx) {
  TF1 *f = 0;
  if (! f) {
    f = new TF1(Form("N%iold",(int)log2dx+2),zMPold,-2,5,2);
    f->SetParName(0,"log2dx");
    f->SetLineStyle(2);
    f->SetParameter(0,log2dx);
    f->SetParameter(1, 1.0); // charge
  }
  return f;
}
//________________________________________________________________________________
Double_t StdEdxModel::zMP(Double_t *x, Double_t *p) {
  fOld = kFALSE;  
  Double_t log10bg = x[0];
  Double_t pOverM  = TMath::Power(10., log10bg);
  Double_t log2dx  = p[0];
  Double_t charge  = p[1];
  Double_t dx      = TMath::Power( 2., log2dx);
  Double_t dNdx = StdEdxModel::instance()->dNdx(pOverM, charge);
  Double_t Np = dNdx*dx;
  Double_t dEkeVLog = StdEdxModel::instance()->LogdEMPVkeV(Np); 
  Double_t dEdxLog  = dEkeVLog - TMath::Log(dx);
  return   dEdxLog;
}
//________________________________________________________________________________
TF1 *StdEdxModel::ZMP(Double_t log2dx) {
  TF1 *f = 0;
  if (! f) {
    f = new TF1(Form("N%i",(int)log2dx+2),zMP,-2,5,2);
    f->SetParName(0,"log2dx");
    f->SetLineStyle(3);
    f->SetParameter(0,log2dx);
    f->SetParameter(1, 1.0); // charge
  }
  return f;
}
//________________________________________________________________________________
Double_t StdEdxModel::zMPR(Double_t *x, Double_t *p) {
  fOld = kFALSE;  
  Double_t log10bg = x[0];
  Double_t pOverM  = TMath::Power(10., log10bg);
  Double_t log2dx  = p[0];
  Double_t charge  = p[1];
  Double_t dx      = TMath::Power( 2., log2dx);
  Double_t dNdx = StdEdxModel::instance()->dNdx(pOverM, charge);
  static Double_t alpha = 2e-3;
  Double_t recom = (1. - alpha*dNdx)/(1. - alpha*30);
  if (recom < 0.8) recom = 0.8;
  Double_t dNdxR = dNdx*recom;
  if (dNdxR <= 0.0) return 0;
  Double_t Np = dNdxR*dx;
  Double_t dEkeVLog = StdEdxModel::instance()->LogdEMPVkeV(Np); 
  Double_t dEdxLog  = dEkeVLog - TMath::Log(dx);
  return   dEdxLog;
}
//________________________________________________________________________________
TF1 *StdEdxModel::ZMPR(Double_t log2dx) {
  TF1 *f = 0;
  if (! f) {
    f = new TF1(Form("R%i",(int)log2dx+2),zMPR,-2,5,2);
    f->SetParName(0,"log2dx");
    f->SetLineStyle(4);
    f->SetParameter(0,log2dx);
    f->SetParameter(1, 1.0); // charge
  }
  return f;
}
//________________________________________________________________________________
void  StdEdxModel::InitPar() {
  fpol2F = (TF1 *) gROOT->GetListOfFunctions()->FindObject("pol2");
  fpol5F = (TF1 *) gROOT->GetListOfFunctions()->FindObject("pol5");
  fpol6F = (TF1 *) gROOT->GetListOfFunctions()->FindObject("pol6");
  if (! fpol2F || ! fpol5F || ! fpol6F) {
    TF1::InitStandardFunctions();
    fpol2F = (TF1 *) gROOT->GetListOfFunctions()->FindObject("pol2");
    fpol5F = (TF1 *) gROOT->GetListOfFunctions()->FindObject("pol5");
    fpol6F = (TF1 *) gROOT->GetListOfFunctions()->FindObject("pol6");
  }
}
//________________________________________________________________________________
Double_t StdEdxModel::muPar(Double_t x, Double_t tCutL10) {
  if (! fpol5F || !fpol2F) InitPar();
  // tChain->Draw("mu:x>>muP(20,2.5,10.5)","i&&dmu<2e-2&&dsigma<2e-2&&da0<4&&x>2.5&&a0>0","prof")
  //  muP->Fit("pol5")
  //  Double_t pars[6] = {  -0.99981,     1.2513,   -0.38066,   0.059075, -0.0043761, 0.00012296}; // 100 keV
  //  Double_t pars[6] = {   -1.1105,     1.2167,   -0.35485,     0.0527, -0.0037333, 0.00010016};
  Double_t val = 3.27959195495368894e+00; // 26.56 eV
  static Double_t parsTmax[3] = {  -0.53035,    0.52464,  -0.043713};
  if (fTmaxL10eV < 5) val += (fpol2F->EvalPar(&fTmaxL10eV, parsTmax) - 1.0);
  static Double_t pars[6] = {  -0.89898,      1.016,   -0.28193,   0.040223, -0.0027295, 6.9597e-05};
  return val + fpol5F->EvalPar(&x, pars);
}
//________________________________________________________________________________
Double_t StdEdxModel::sigmaPar(Double_t x, Double_t tCutL10) { 
  if (! fpol5F) InitPar();
  // tChain->Draw("sigma:x>>sigmaP(20,2.5,10.5)","i&&dmu<2e-2&&dsigma<2e-2&&da0<4&&a0>0","prof")
  //  Double_t pars[6] = {    1.6935,   -0.56485,   0.059062, 0.00081105, -0.00046914, 1.9862e-05};
  static Double_t pars[6] = {    1.9662,   -0.74794,    0.11477, -0.0082309, 0.00027025, -3.7396e-06};
  return fpol5F->EvalPar(&x, pars);
}
//________________________________________________________________________________
Double_t StdEdxModel::a0Par(Double_t x, Double_t tCutL10) {
  if (! fpol5F) InitPar();
  // tChain->Draw("a0:x>>a0P(20,2.5,10.5)","i&&dmu<2e-2&&dsigma<2e-2&&da0<4&&a0>0","prof")
  //  Double_t pars[6] = {    5.6594,    -3.6107,     1.1259,   -0.18641,   0.015486, -0.0004892};
  //  tChain->Draw("a0-a0Par(x):x>>a0PC(20,2.5,10.5)","i&&dmu<2e-2&&dsigma<2e-2&&da0<4&&a0>0","prof")
  // FitP->Draw("a0:x>>a0P(20,2.5,10.5)","i&&dmu<2e-2&&dsigma<2e-2&&da0<4&&a0<4","prof")
  static Double_t pars[6] = {    9.1532,    -6.0693,      1.783,   -0.26302,   0.018738, -0.00049388};
  return fpol5F->EvalPar(&x, pars);
}
//________________________________________________________________________________
Double_t StdEdxModel::tmaxL10eV(Double_t bg) {
  static Double_t Tcut = 1e-4; // 100 keV maximum cluster size (~80 keV)
  static Double_t m_e  = 0.51099907e-3;
  Double_t bg2 = bg*bg;
  //  Double_t gamma = TMath::Sqrt(bg2 + 1);
  Double_t tMax =  2*m_e*bg2; // /(1. + mOverM*(2*gamma + mOverM)); 
  return TMath::Log10(1e9*TMath::Min(Tcut, tMax));
}
