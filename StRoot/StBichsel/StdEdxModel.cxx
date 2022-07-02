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
TH1D         *StdEdxModel::mdNdxL10 = 0;  
TH1D         *StdEdxModel::mdNdx = 0;  
Double_t      StdEdxModel::fScale = 1; //TMath::Exp(9.12015e-02); // Bichsel
Int_t         StdEdxModel::_debug   = 1;
TF1          *StdEdxModel::fGGaus = 0;
TF1          *StdEdxModel::fGausExp = 0;

Double_t      StdEdxModel::shift2keV = 0;
Double_t      StdEdxModel::shift2GeV = 0;
Double_t      StdEdxModel::shift2eV = 0;
Double_t StdEdxModel::GeVperElectron               = 43.5e-9;  //39.41e-9;  // 24.95e-9; // deposited energy per conducting electron 
Double_t StdEdxModel::LogGeVperElectron = TMath::Log(43.5e-9); //39.41e-9); // TMath::Log(24.95e-9);

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
  SafeDelete(mdNdxL10);
  SafeDelete(fGGaus);
}
//________________________________________________________________________________
Double_t StdEdxModel::dNdx(Double_t poverm, Double_t charge) {
  if (!fgStdEdxModel) instance();
  if (mdNdx)    return fScale*charge*charge*mdNdx->Interpolate(poverm);
  if (mdNdxL10) return fScale*charge*charge*mdNdxL10->Interpolate(TMath::Log10(poverm));
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
  Double_t k     = p[3];
  Double_t t     = (x[0] - mu)/sigma;
  Double_t V     = 0.;
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
  // Most Probable log (ne/Np) versus log of Np
  static Double_t parsMu1[7]  = {    3.0767,  -0.054267,   -0.69433,    0.71462,    0.12861,   -0.29135,   0.072728};
  static Double_t parsMu2[7]  = {   0.33622,     3.2863,    -1.3061,    0.27857,  -0.032664,  0.0019922, -4.944e-05};
  static Double_t pars6Sig[7] = {   0.82995,   0.033141,   -0.36552,   -0.18875,     0.2694,  -0.073444,  0.0038834}; // log(log(fitX))
  static Double_t parsk[7]    = {    -8.959,     13.454,    -6.6549,     1.6064,   -0.20527,     0.0133, -0.00034269};
  static TF1 *pol6 = 0, *pol5 = 0;
  if (! pol6) {
    pol6 = (TF1 *) gROOT->GetListOfFunctions()->FindObject("pol6");
    if (! pol6) {
      TF1::InitStandardFunctions();
      pol6 = (TF1 *) gROOT->GetListOfFunctions()->FindObject("pol6");
    }
    pol5 = (TF1 *) gROOT->GetListOfFunctions()->FindObject("pol5");
    assert(pol5 && pol6);
  }
  parameters[0] = parameters[1] = parameters[2] = 0;
  Double_t &mu    = *&parameters[0];
  Double_t &sigma = *&parameters[1];
  Double_t &k     = *&parameters[2];
  if (Np <= 1.0) return;
  Double_t fitX = TMath::Log(Np);
  if (fitX < 0.5) fitX = 0.5;
  Double_t X = TMath::Log(fitX);
  if (fitX < 2) mu = pol6->EvalPar(&fitX, parsMu1);
  else          mu = pol6->EvalPar(&fitX, parsMu2);
  sigma = pol6->EvalPar(&X, pars6Sig);
  k = 6.70823e-01;
  if (fitX > 2) {
    k = pol6->EvalPar(&fitX, parsk);
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
