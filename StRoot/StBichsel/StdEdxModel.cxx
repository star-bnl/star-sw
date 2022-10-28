#include <assert.h>
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
TF1 *StdEdxModel::fpol7F = 0;
StdEdxModel::EParameterizationType StdEdxModel::fParametrization = StdEdxModel::kNewBG;
//________________________________________________________________________________
StdEdxModel* StdEdxModel::instance() {
  if (! fgStdEdxModel) {
    new StdEdxModel();
    InitPar();
  }
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
  Double_t Q_eff = TMath::Abs(charge);
  if (Q_eff > 1)   {
    Double_t beta = poverm/TMath::Sqrt(1.0 + poverm*poverm);
    // Effective charge from GEANT gthion.F
    Double_t w1 = 1.034 - 0.1777*TMath::Exp(-0.08114*Q_eff);
    Double_t w2 = beta*TMath::Power(Q_eff,-2./3.);
    Double_t w3 = 121.4139*w2 + 0.0378*TMath::Sin(190.7165*w2);
    Q_eff      *= 1. -w1*TMath::Exp(-w3);
  }
  if (mdNdx)    return fScale*Q_eff*Q_eff*mdNdx->Interpolate(poverm);
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
  if (fParametrization == kOld) {
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
  
  SetParametrization(kOld);
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
  TString fName(Form("N%iold",(int)(2*(log2dx+2))));
  TF1 *f = (TF1 *) gROOT->GetListOfFunctions()->FindObject(fName);
  if (! f) {
    f = new TF1(fName,zMPold,-2,5,2);
    f->SetParName(0,"log2dx");
    f->SetLineStyle(2);
    f->SetParameter(0,log2dx);
    f->SetParameter(1, 1.0); // charge
  }
  return f;
}
//________________________________________________________________________________
Double_t StdEdxModel::zMP(Double_t *x, Double_t *p) {
  SetParametrization(kNew);  
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
  TString fName(Form("N%i",(int)(2*(log2dx+2))));
  TF1 *f = (TF1 *) gROOT->GetListOfFunctions()->FindObject(fName);
  if (! f) {
    f = new TF1(fName,zMP,-2,5,2);
    f->SetParName(0,"log2dx");
    f->SetLineStyle(3);
    f->SetParameter(0,log2dx);
    f->SetParameter(1, 1.0); // charge
  }
  return f;
}
#if 0
//________________________________________________________________________________
Double_t StdEdxModel::zMPnew(Double_t *x, Double_t *p) {  // 09/27/2022
  SetParametrization(kNew);  
  Double_t log10bg = x[0];
  Double_t pOverMRC  = TMath::Power(10., log10bg);
  Double_t log2dx  = p[0];
  Double_t charge  = p[1];
  Double_t dx      = TMath::Power( 2., log2dx);
  Double_t dNdx = StdEdxModel::instance()->dNdxEff(pOverMRC, charge); // */dNdxVsBgC*.root [-1.5,5]
  Double_t Np = dNdx*dx;
  /* 09/26/2022 */
  Double_t NpLog = TMath::Log(Np);
  Double_t NpLogX = TMath::Max(3.2, NpLog);
  //  Double_t NpLogX = TMath::Max(2.3, NpLog);
  Double_t xx = TMath::Log(NpLogX);
  Double_t parsS[3] = {    2.1392,    -1.7129,    0.34465}; // log(x)
  Double_t sigma = parsS[0] + xx * ( parsS[1] + xx * parsS[2]);
  Double_t parsA[4] = {   -55.709,     106.35,    -62.082,     11.565}; // log(x)
  Double_t alpha  = parsA[0] + xx * ( parsA[1] + xx * (parsA[2] + xx * parsA[3]));
  Double_t parsM[6] = {    78.047,    -199.11,     203.54,    -103.58,     26.358,    -2.6949}; //log(x)
  Double_t mu     = parsM[0] + xx * ( parsM[1] + xx * (parsM[2] + xx * (parsM[3] + xx * (parsM[4] + xx * parsM[5]))));
  mu = TMath::Min(1.08, mu);
  Double_t dEkeVLog = NpLog + mu -3.13746587897608142e+00;// + 7.02725079814016507e+00;// - 3.13746587897608142e+00;// 43.4 eV/conducting electron 
  Double_t dEdxLog  = dEkeVLog - TMath::Log(dx);
  return   dEdxLog;
}
#else /* 10/27/2022 */
//________________________________________________________________________________
Double_t StdEdxModel::zMPnew(Double_t *x, Double_t *p) {  
  SetParametrization(kNew);  
  Double_t log10bg = x[0];
  Double_t pOverMRC  = TMath::Power(10., log10bg);
  Double_t log2dx  = p[0];
  Double_t charge  = p[1];
  Double_t dx      = TMath::Power( 2., log2dx);
  Double_t dNdx = StdEdxModel::instance()->dNdxEff(pOverMRC, charge); // */dNdxVsBgC*.root [-1.5,5]
  Double_t Np = dNdx*dx;
  Double_t NpLog = TMath::Log(Np);
  Double_t fitX = NpLog;

  Double_t parsA[2] = {    5.4634,   -0.57598}; //alpha x
  Double_t alpha  = parsA[0] + fitX *  parsA[1];
  Double_t parsS[3] = {    1.6924,    -1.2912,    0.24698}; //sigma versus log(x)	 
  Double_t xx = (fitX > 0) ? TMath::Log(fitX) : 0;
  Double_t sigma = parsS[0] + xx * ( parsS[1] + xx * parsS[2]);
  Double_t parsM[8] = {   -4.3432,     4.6327,    -1.9522,     0.4691,  -0.066615,  0.0055111, -0.00024531, 4.5394e-06}; //mu pol7
  Double_t mu = fpol7F->EvalPar(&fitX, parsM);
  Double_t dEkeVLog = NpLog + mu -3.13746587897608142e+00 +1.78334647296254700e-01;// + 7.02725079814016507e+00;// - 3.13746587897608142e+00;// 43.4 eV/conducting electron 
  Double_t dEdxLog  = dEkeVLog - TMath::Log(dx);
  return   dEdxLog;
}
#endif
//________________________________________________________________________________
TF1 *StdEdxModel::ZMPnew(Double_t log2dx) {
  SetParametrization(kNewBG);  
  TString fName(Form("New%i",(int)(2*(log2dx+2))));
  TF1 *f = (TF1 *) gROOT->GetListOfFunctions()->FindObject(fName);
  if (! f) {
    f = new TF1(fName,zMPnew,-2,5,2);
    f->SetParName(0,"log2dx");
    f->SetLineStyle(4);
    f->SetParameter(0,log2dx);
    f->SetParameter(1, 1.0); // charge
    cout << "Create ZMPNew with name " << f->GetName() << " for log2dx = " << log2dx << endl;
  }
  return f;
}
//________________________________________________________________________________
Double_t StdEdxModel::zMPR(Double_t *x, Double_t *p) {
  SetParametrization(kNew);  
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
  TString fName(Form("R%i",(int)(2*(log2dx+2))));
  TF1 *f = (TF1 *) gROOT->GetListOfFunctions()->FindObject(fName);
  if (! f) {
    f = new TF1(fName,zMPR,-2,5,2);
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
  fpol7F = (TF1 *) gROOT->GetListOfFunctions()->FindObject("pol7");
  if (! fpol2F || ! fpol5F || ! fpol6F || ! fpol7F) {
    TF1::InitStandardFunctions();
    fpol2F = (TF1 *) gROOT->GetListOfFunctions()->FindObject("pol2");
    fpol5F = (TF1 *) gROOT->GetListOfFunctions()->FindObject("pol5");
    fpol6F = (TF1 *) gROOT->GetListOfFunctions()->FindObject("pol6");
    fpol7F = (TF1 *) gROOT->GetListOfFunctions()->FindObject("pol7");
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
//________________________________________________________________________________
Double_t StdEdxModel::saturationTanH(Double_t *x, Double_t *p) { // nP saturation versus beta*gamma from TpcRS (nP/dX - dN/dx_model) 
  //  TF1 *s8 = new TF1("s8","[0]+[1]*TMath::TanH([2]+x*([3]+x*([4] + x*([5] +x*([6] + x*[7])))))",-2,5)
  return p[0]+p[1]*TMath::TanH(p[2]+x[0]*(p[3]+x[0]*(p[4] + x[0]*(p[5] +x[0]*(p[6] + x[0]*p[7])))));
}
//________________________________________________________________________________
TF1 *StdEdxModel::SaturTanH() {
  static TF1 *f = 0;
  if (! f) {
    f = new TF1("SaturTanHF",StdEdxModel::saturationTanH,-5,5,8);
    Double_t pars[8] = { -0.060944,  -0.014597,     1.6066,    -3.4821,     3.4131,    -1.3879,    0.26295,  -0.019545}; //
    f->SetParNames("offset","slope","a0","a1","a2","a3","a4","a5","a6");
    f->SetParameters(pars);
  }
  return f;
}
//________________________________________________________________________________
Double_t StdEdxModel::saturationFunc(Double_t *x, Double_t *p) { // nP saturation versus beta*gamma from TpcRS (nP/dX - dN/dx_model) 
  return (p[0] + p[1]*TMath::TanH(p[2]*(x[0] - p[3])))*(1 + x[0]*(p[4] + x[0]*(p[5] + x[0]*p[6])));
}
//________________________________________________________________________________
TF1 *StdEdxModel::Saturation(Int_t particle) {
  static TF1 *f = 0;
  //                       "offset","slope","scale","shift","decay",decay2","decay3"
  Double_t params[1][7] = {{      0.,     1.,    1.,      0.,  0.0, 0.0, 0.0}}; 
  /*
//    muon+ muon- pion+ pion- electron- electron+ kaon+ kaon- proton+ proton- deuteron triton He3 alpha  HE6 Li5 Li6 Li7 Be7 Be9 Be10 B11
  Double_t pars[6] = { -0.069754,  0.0096439,     3.3618,    0.34362,   0.080825, 0.0}; //pion
  Double_t pars[6] = { -0.070392,  0.0092189,     3.2517,    0.35073,   0.071063, 0.0}; //pion+
  Double_t pars[6] = { -0.069003,   0.010194,      3.476,    0.33636,   0.093771, 0.0}; //pion-

  Double_t pars[6] = { -0.070037,  0.0084161,     3.3316,    0.43196,   0.060741, 0.0}; //muon
  Double_t pars[6] = { -0.070581,  0.0077046,     3.1618,    0.44949,   0.048593, 0.0}; //muon+
  Double_t pars[6] = { -0.069342,  0.0093344,      3.473,     0.4168,   0.077696, 0.0}; //muon-
  
  Double_t pars[6] = { -0.077527,  0.0030426,     3.1981,      2.924,  -0.018933, 0.0}; //electron [1.5,5.0]
  Double_t pars[6] = { -0.077226,  0.0031247,      2.812,     2.9768,  -0.016703, 0.0}; //electron+ [1.5,5.0]
  Double_t pars[6] = { -0.076561,  0.0032965,     3.3269,     2.8795,  -0.015984, 0.0}; //electron- [1.5,5.0]

  Double_t pars[6] = { -0.068983,  -0.010225,    -3.6599,    0.14705,    0.10115, 0.0}; //kaon
  Double_t pars[6] = { -0.069257,   -0.01048,    -3.5196,     0.1541,    0.10816, 0.0}; //kaon+
  Double_t pars[6] = { -0.068765, -0.0094219,     -4.006,    0.14183,   0.080608, 0.0}; //kaon-

  Double_t pars[6] = { -0.069263,    0.01079,     3.2997,   0.098691,    0.11402, 0.0}; //proton
  Double_t pars[6] = { -0.069434,   0.011068,     3.2247,    0.10424,    0.12358, 0.0}; //proton+
  Double_t pars[6] = { -0.069086,   0.010355,     3.4201,   0.094182,    0.10077, 0.0}; //proton-

  Double_t pars[6] = { -0.069804,  0.0063906,     4.9205,   0.092017,   0.019268, 0.0}; //deuteron
  Double_t pars[6] = { -0.069985,  0.0049321,     5.7371,   0.088682,  -0.019409, 0.0}; //triton

  Double_t pars[6] = {  -0.26751,    0.23006,    0.78369,    -1.5566,    0.61911, 0.0}; //He3
  Double_t pars[6] = {  -0.12966,   0.085683,     1.0917,   -0.71112,    0.61608, 0.0}; //alpha
  Double_t pars[6] = {  -0.14076,   0.093768,     1.1577,   -0.78164,    0.60239, 0.0}; //He6

  Double_t pars[6] = { -0.056214,          0,          1,          0,          0,          0}; //Li5
  Double_t pars[6] = { -0.072191,          0,          1,          0,          0,          0}; //Li6
  Double_t pars[6] = { -0.073465,          0,          1,          0,   -0.02401,   -0.36052}; //Li7
  Double_t pars[6] = { -0.069281,          0,          1,          0,          0,          0}; //Be7
  Double_t pars[6] = { -0.068158,          0,          1,          0,          0,          0}; //Be9
  Double_t pars[6] = { -0.068274,          0,          1,          0,          0,          0}; //Be10
  Double_t pars[6] = { -0.067544,          0,          1,          0,          0,          0}; //B11


// /hlt/cephfs/fisyak/dEdx202100keV	Tue Sep 20 17:53:18 2022
  Double_t pars[7] = { -0.073601,    0.75141,    0.01337,   0.062348,   0.063945,          0,          0}; // alpha	chisq = 484.892639 / NDF = 21
  Double_t pars[7] = { -0.073747,    0.59382,   0.010598,   0.076641,   0.013279, -0.0082442,          0}; // alpha	chisq = 480.020891 / NDF = 20
  Double_t pars[7] = { -0.073136,     1.5345,   0.027354,   0.024053,    0.49131,    0.30569,    0.19983}; // alpha	chisq = 21.290233 / NDF = 19
  Double_t pars[7] = { -0.079363,    0.88696,  -0.030323,    0.41884,   -0.44251,          0,          0}; // Be10	chisq = 69.678232 / NDF = 9
  Double_t pars[7] = { -0.080956,     1.0472,  -0.035763,    0.34609,   -0.60403,    0.19983,          0}; // Be10	chisq = 61.518572 / NDF = 8
  Double_t pars[7] = { -0.094642,     1.3531,  -0.044628,    0.44296,   -0.90129,    0.78034,   -0.97989}; // Be10	chisq = 22.730086 / NDF = 7
  Double_t pars[7] = { -0.068737,      1.041,  -0.022125,  -0.049352,   -0.31066,          0,          0}; // Be7	chisq = 15.053611 / NDF = 9
  Double_t pars[7] = { -0.068401,     1.3249,  -0.028126,  -0.039017,   -0.52284,    0.19577,          0}; // Be7	chisq = 11.074288 / NDF = 8
  Double_t pars[7] = { -0.068082,     1.5828,  -0.033582,  -0.032613,   -0.74306,     0.5157,   -0.38286}; // Be7	chisq = 9.538102 / NDF = 7
  Double_t pars[7] = { -0.062668,     1.0199,  -0.020292,   -0.30786,   -0.26803,          0,          0}; // Be9	chisq = 96.902501 / NDF = 7
  Double_t pars[7] = { -0.059955,     1.4917,  -0.029628,   -0.20143,   -0.61779,    0.44373,          0}; // Be9	chisq = 66.734790 / NDF = 6
  Double_t pars[7] = { -0.057759,     1.8793,  -0.037307,   -0.15765,   -0.93501,     1.1297,    -1.7592}; // Be9	chisq = 7.831630 / NDF = 5
  Double_t pars[7] = { -0.071336,  -0.004178,    -4.6637,   0.075488,  0.0025174,          0,          0}; // deuteron	chisq = 2495.768345 / NDF = 42
  Double_t pars[7] = { -0.071883, -0.0029722,    -6.0932,   0.047572,  -0.028641,   -0.02862,          0}; // deuteron	chisq = 2182.997226 / NDF = 41
  Double_t pars[7] = { -0.071863, -0.0022604,    -7.4294,   0.057791,  -0.059625,  -0.025715,    0.03694}; // deuteron	chisq = 1957.552463 / NDF = 40
  Double_t pars[7] = { -0.076162,  0.0028375,     3.8266,     2.8381,  -0.011619,          0,          0}; // electron-	chisq = 1077.707662 / NDF = 57
  Double_t pars[7] = { -0.058508,  0.0035046,     2.9347,     2.7793,    0.16362,   -0.02408,          0}; // electron-	chisq = 823.242003 / NDF = 56
  Double_t pars[7] = {   -0.0925,   0.012058,      1.963,     2.7701,   -0.42068,    0.19256,  -0.023926}; // electron-	chisq = 686.085038 / NDF = 55
  Double_t pars[7] = { -0.081233,   0.042534,   0.063869,     1.7527,  -0.020328,          0,          0}; // electron	chisq = 10556.978217 / NDF = 59
  Double_t pars[7] = { -0.020628,  0.0034742,     1.4362,     2.5744,     1.5498,   -0.19651,          0}; // electron	chisq = 2840.783492 / NDF = 58
  Double_t pars[7] = { -0.034596,   0.002144,     2.7584,     2.7582,    0.89959,   -0.21835,   0.016411}; // electron	chisq = 2334.467584 / NDF = 57
  Double_t pars[7] = { -0.078616,  0.0020549,     4.1571,     2.9287,  -0.020356,          0,          0}; // electron+	chisq = 1745.477073 / NDF = 59
  Double_t pars[7] = { -0.054616,  0.0027916,     3.0399,     2.8348,    0.23179,  -0.035896,          0}; // electron+	chisq = 1090.069902 / NDF = 58
  Double_t pars[7] = { -0.087318,  0.0096707,     2.0515,      2.827,   -0.37628,     0.1809,  -0.023231}; // electron+	chisq = 955.346917 / NDF = 57
  Double_t pars[7] = { -0.074372,     0.1709,   0.070609,   0.013178,    0.09813,          0,          0}; // He3	chisq = 1795.781570 / NDF = 26
  Double_t pars[7] = { -0.081081,   0.014953,    0.99332,   -0.47341,   0.093059,  -0.065326,          0}; // He3	chisq = 1036.039834 / NDF = 25
  Double_t pars[7] = { -0.078447,  0.0051381,     2.8885,   -0.36878,   0.013826,   -0.13149,   0.057101}; // He3	chisq = 91.436056 / NDF = 24
  Double_t pars[7] = { -0.074038,    0.71338,   0.015905,  -0.017015,   0.071685,          0,          0}; // HE6	chisq = 322.485188 / NDF = 21
  Double_t pars[7] = {  -0.07399,    0.56099,   0.012562,  -0.020485,   0.013213,  -0.010264,          0}; // HE6	chisq = 317.488559 / NDF = 20
  Double_t pars[7] = { -0.074361,      1.396,   0.031224,  -0.014293,    0.50475,    0.33328,    0.24132}; // HE6	chisq = 42.913616 / NDF = 19
  Double_t pars[7] = { -0.069836,  -0.008358,    -3.8113,    0.11371,   0.088091,          0,          0}; // kaon-	chisq = 1003.343228 / NDF = 48
  Double_t pars[7] = { -0.069453,  -0.007588,     -4.119,    0.13623,   0.065963,   0.018019,          0}; // kaon-	chisq = 778.855773 / NDF = 47
  Double_t pars[7] = { -0.068673,  -0.013415,    -3.0211,    0.11656,    0.22962,   0.076787,  -0.088963}; // kaon-	chisq = 381.538322 / NDF = 46
  Double_t pars[7] = { -0.070177, -0.0079598,    -3.8416,    0.12605,   0.079716,          0,          0}; // kaon	chisq = 1561.369639 / NDF = 49
  Double_t pars[7] = { -0.069986, -0.0075879,    -3.9865,    0.13779,   0.068826,  0.0091704,          0}; // kaon	chisq = 1442.803731 / NDF = 48
  Double_t pars[7] = { -0.069191,   -0.01394,     -2.894,    0.11541,    0.24216,   0.070801,  -0.092271}; // kaon	chisq = 617.202756 / NDF = 47
  Double_t pars[7] = { -0.070509, -0.0077028,    -3.8248,    0.13737,   0.074229,          0,          0}; // kaon+	chisq = 665.545095 / NDF = 48
  Double_t pars[7] = { -0.070475, -0.0076415,    -3.8474,    0.13949,   0.072397,  0.0016176,          0}; // kaon+	chisq = 663.688018 / NDF = 47
  Double_t pars[7] = { -0.069695,  -0.014384,     -2.789,     0.1144,    0.25175,   0.064514,  -0.093955}; // kaon+	chisq = 277.972539 / NDF = 46
  Double_t pars[7] = { -0.068107,     1.4558,  -0.031939,   -0.13614,   -0.67474,          0,          0}; // Li5	chisq = 1528.425350 / NDF = 7
  Double_t pars[7] = { -0.075783,     1.0908,  -0.059209,    0.02118,   -0.95215,    0.43123,          0}; // Li5	chisq = 1004.816487 / NDF = 6
  Double_t pars[7] = {  -0.07558,    0.89535,  -0.048682,   0.025404,   -0.67003,  -0.046156,    0.39288}; // Li5	chisq = 936.675106 / NDF = 5
  Double_t pars[7] = { -0.048749,     1.4441,   0.030852,    0.56134,    0.54716,          0,          0}; // Li6	chisq = 2597.699313 / NDF = 14
  Double_t pars[7] = { -0.056064,     1.1095,   0.023699,    0.67206,     0.2915,      -0.22,          0}; // Li6	chisq = 2551.528794 / NDF = 13
  Double_t pars[7] = { -0.055483,     1.1493,    0.02457,    0.64679,    0.31998,   -0.20637,  -0.023835}; // Li6	chisq = 2547.965416 / NDF = 12
  Double_t pars[7] = { -0.066122,  -0.025065,    -1.4473,    0.20196,    0.35495,          0,          0}; // Li7	chisq = 119.786609 / NDF = 15
  Double_t pars[7] = { -0.081217,   -0.06161,    -1.1441,   -0.11373,    0.84387,    0.31613,          0}; // Li7	chisq = 102.816620 / NDF = 14
  Double_t pars[7] = { -0.077592,  -0.021138,    -1.8127,   -0.11779,    0.39463,   -0.26869,   -0.32607}; // Li7	chisq = 76.740215 / NDF = 13
  Double_t pars[7] = { -0.070404,  0.0078884,     3.5324,    0.40205,   0.064262,          0,          0}; // muon-	chisq = 1282.602144 / NDF = 56
  Double_t pars[7] = { -0.069543,   0.010323,     2.9705,    0.36291,    0.14564,  -0.026972,          0}; // muon-	chisq = 1124.241131 / NDF = 55
  Double_t pars[7] = { -0.066119,   0.017606,     2.1586,    0.35782,    0.25951,    0.13777,  -0.083934}; // muon-	chisq = 242.927919 / NDF = 54
  Double_t pars[7] = { -0.070868,  0.0070857,     3.5032,     0.4237,   0.052866,          0,          0}; // muon	chisq = 2194.271282 / NDF = 57
  Double_t pars[7] = { -0.069832,  0.0096729,     2.8846,    0.37943,    0.13917,  -0.028511,          0}; // muon	chisq = 1827.724279 / NDF = 56
  Double_t pars[7] = { -0.066283,   0.017608,     2.0468,    0.36435,     0.2665,    0.12683,  -0.080257}; // muon	chisq = 447.107626 / NDF = 55
  Double_t pars[7] = { -0.071269,  0.0064184,     3.4553,    0.44737,   0.044061,          0,          0}; // muon+	chisq = 933.884599 / NDF = 54
  Double_t pars[7] = { -0.070067,  0.0090864,     2.8041,    0.39862,    0.13328,  -0.029336,          0}; // muon+	chisq = 738.071444 / NDF = 53
  Double_t pars[7] = { -0.066284,   0.017726,     1.9469,    0.37386,    0.27318,    0.12364,  -0.079441}; // muon+	chisq = 217.300272 / NDF = 52
  Double_t pars[7] = { -0.070095,  0.0088641,     3.4964,    0.32256,   0.081893,          0,          0}; // pion-	chisq = 1320.830372 / NDF = 54
  Double_t pars[7] = { -0.069669,   0.012476,     2.8361,    0.26856,     0.1923,   -0.03869,          0}; // pion-	chisq = 1012.596163 / NDF = 53
  Double_t pars[7] = {   -0.0674,   0.024185,     2.0107,    0.21972,    0.43126,    0.14851,   -0.11584}; // pion-	chisq = 126.535495 / NDF = 52
  Double_t pars[7] = {   -0.0682,     2.1625,  0.0010472,     2.0745,  -0.031068,          0,          0}; // pion	chisq = 67755.078969 / NDF = 57
  Double_t pars[7] = { -0.075258, -0.0014257, 6.9559e-07,     2.1993,   -0.20179,   0.094268,          0}; // pion	chisq = 22458.927436 / NDF = 56
  Double_t pars[7] = {  -0.04016,   -0.12179,    -0.2486,     1.2591,    0.19597,   -0.15774,     0.3494}; // pion	chisq = 3189.524934 / NDF = 55
  Double_t pars[7] = { -0.071355,  0.0072479,     3.5443,    0.34468,   0.051769,          0,          0}; // pion+	chisq = 1026.769917 / NDF = 53
  Double_t pars[7] = { -0.070613,   0.011347,     2.7157,    0.28021,    0.17479,  -0.043166,          0}; // pion+	chisq = 661.762814 / NDF = 52
  Double_t pars[7] = { -0.068516,   0.023561,     1.8697,    0.21984,    0.40777,    0.11349,  -0.099351}; // pion+	chisq = 163.566087 / NDF = 51
  Double_t pars[7] = { -0.070389, -0.0077741,    -3.5279,   0.068167,   0.078742,          0,          0}; // proton-	chisq = 2103.762827 / NDF = 44
  Double_t pars[7] = { -0.070561, -0.0077177,     -3.535,    0.06075,   0.077959, -0.0070446,          0}; // proton-	chisq = 2076.958600 / NDF = 43
  Double_t pars[7] = {  -0.07066, -0.0057095,     -4.174,   0.068798,   0.018758,  -0.017705,   0.037162}; // proton-	chisq = 2015.458793 / NDF = 42
  Double_t pars[7] = { -0.070562, -0.0077168,    -3.4873,   0.077743,   0.079321,          0,          0}; // proton	chisq = 4008.217571 / NDF = 43
  Double_t pars[7] = { -0.070848, -0.0076799,    -3.4825,   0.064966,    0.07951,  -0.011741,          0}; // proton	chisq = 3873.636898 / NDF = 42
  Double_t pars[7] = {  -0.07101, -0.0052236,    -4.3182,   0.074543,  0.0074196,  -0.025585,   0.045856}; // proton	chisq = 3684.400936 / NDF = 41
  Double_t pars[7] = {  -0.07072, -0.0083803,    -3.2265,   0.084608,   0.094851,          0,          0}; // proton+	chisq = 2055.338021 / NDF = 43
  Double_t pars[7] = { -0.071159, -0.0079663,    -3.3295,   0.066829,   0.087892,  -0.017291,          0}; // proton+	chisq = 1912.431139 / NDF = 42
  Double_t pars[7] = { -0.071408, -0.0045683,    -4.5718,   0.080685,  -0.010314,  -0.035956,   0.061825}; // proton+	chisq = 1751.488703 / NDF = 41
  Double_t pars[7] = { -0.073334,   0.046675,    0.59824,  -0.025063,    0.24963,          0,          0}; // triton	chisq = 3288.188698 / NDF = 36
  Double_t pars[7] = { -0.072183,  0.0019079,      7.248,   0.038967,  -0.058785,  -0.052737,          0}; // triton	chisq = 1207.289343 / NDF = 35
  Double_t pars[7] = { -0.072138,  0.0018391,     7.4457,   0.044421,  -0.062916,  -0.048811,  0.0092688}; // triton	chisq = 1189.783216 / NDF = 34
09/24/2022
// /hlt/cephfs/fisyak/Fit	Sat Sep 24 11:19:27 2022
  Double_t pars[7] = { -0.077712, -0.0033412,    -3.3287,     2.8284,   -0.02002,          0,          0}; // electron-COL	chisq = 400.355859 / NDF = 59
  Double_t pars[7] = { -0.038331, -0.0039637,    -2.1902,     2.7107,    0.56673,   -0.08022,          0}; // electron-COL	chisq = 172.642030 / NDF = 58
  Double_t pars[7] = { -0.045392,  -0.006256,    -1.9007,     2.6929,    0.18645,   0.062376,  -0.015684}; // electron-COL	chisq = 168.361785 / NDF = 57
  Double_t pars[7] = { -0.079957, -0.0026383,    -3.3327,     2.9211,  -0.026865,          0,          0}; // electron+COL	chisq = 490.739132 / NDF = 59
  Double_t pars[7] = { -0.043914, -0.0035152,     -2.273,     2.7878,    0.43439,  -0.064819,          0}; // electron+COL	chisq = 192.651659 / NDF = 58
  Double_t pars[7] = { -0.047536, -0.0041229,    -2.1841,     2.7883,    0.29741,  -0.019738,  -0.004555}; // electron+COL	chisq = 192.011784 / NDF = 57
  Double_t pars[7] = { -0.078978, -0.0029429,    -3.3389,     2.8729,  -0.024103,          0,          0}; // electronCOL	chisq = 843.389038 / NDF = 59
  Double_t pars[7] = {   -0.0415, -0.0037696,    -2.2214,      2.749,    0.48601,  -0.070646,          0}; // electronCOL	chisq = 336.681579 / NDF = 58
  Double_t pars[7] = { -0.047724, -0.0049973,    -2.0704,     2.7477,    0.23655,   0.012412, -0.0084929}; // electronCOL	chisq = 332.928617 / NDF = 57
  Double_t pars[7] = { -0.079857, -0.0020054,    -5.3142,      2.801,  -0.024112,          0,          0}; // electron-FXT	chisq = 453.043418 / NDF = 59
  Double_t pars[7] = { -0.069282, -0.0025422,     -4.043,     2.7721,   0.062833,  -0.012376,          0}; // electron-FXT	chisq = 390.341916 / NDF = 58
  Double_t pars[7] = {  -0.12051,   -0.02907,    -1.3553,     2.6704,   -0.66218,    0.28614,  -0.033912}; // electron-FXT	chisq = 279.392893 / NDF = 57
  Double_t pars[7] = { -0.082144, -0.0012493,    -5.9997,     2.9024,  -0.031554,          0,          0}; // electron+FXT	chisq = 1093.876523 / NDF = 60
  Double_t pars[7] = { -0.060189, -0.0022009,    -3.7086,     2.8155,    0.17296,  -0.029677,          0}; // electron+FXT	chisq = 748.738627 / NDF = 59
  Double_t pars[7] = {  -0.13164,  -0.057247,    -1.0776,     2.4509,   -0.88342,    0.41293,  -0.049502}; // electron+FXT	chisq = 453.312446 / NDF = 58
  Double_t pars[7] = { -0.081057,  0.0015963,     5.5394,     2.8465,  -0.028146,          0,          0}; // electronFXT	chisq = 1477.459419 / NDF = 60
  Double_t pars[7] = { -0.064431,  0.0023793,     3.7896,     2.7925,    0.11764,  -0.020966,          0}; // electronFXT	chisq = 1115.434973 / NDF = 59
  Double_t pars[7] = {  -0.12629,   0.045864,     1.1627,     2.5427,   -0.80495,    0.36744,  -0.043926}; // electronFXT	chisq = 673.906498 / NDF = 58

  */
  if (! f) {
    f = new TF1("SaturationF",StdEdxModel::saturationFunc,-5,5,7);
    f->SetParNames("offset","slope","scale","shift","decay","decay2","decay3");
  }
  f->SetParameters(params[particle]);
  return f;
}
//________________________________________________________________________________
Double_t StdEdxModel::bgCorrected(Double_t bgRC) {
  // Parameterization of correction from /hlt/cephfs/fisyak/Fit/*/dBGLGADCut23.root 09/25/2022
  Double_t pars[3] = {-0.00020089,  0.0031976,  0.0062467}; //
  static TF1 *pol2 = 0;
  if (! pol2) {
    pol2 = (TF1 *) gROOT->GetListOfFunctions()->FindObject("pol2");
    if (! pol2) {
      TF1::InitStandardFunctions();
      pol2 = (TF1 *) gROOT->GetListOfFunctions()->FindObject("pol2");
    }
    assert(pol2);
  }
  Double_t bgRC2 = bgRC*bgRC;
  Double_t beta2RC = bgRC2/(bgRC2 + 1);
  Double_t betaRCL10 = 0.5*TMath::Log(beta2RC);
  Double_t bgMC = bgRC;
  if (betaRCL10 < -0.5) bgMC += pol2->EvalPar(&betaRCL10, pars);
  return bgMC;
}
//________________________________________________________________________________
TH1D *StdEdxModel::protonEff() {
//========= Macro generated from object: Func/
//========= by ROOT version5.34/39
   
   TH1D *Func__1 = new TH1D("protonEff","",100,-2.0,0.0);
   Func__1->SetBinContent(1,0.829974);
   Func__1->SetBinContent(2,0.833488);
   Func__1->SetBinContent(3,0.836679);
   Func__1->SetBinContent(4,0.839568);
   Func__1->SetBinContent(5,0.842227);
   Func__1->SetBinContent(6,0.844577);
   Func__1->SetBinContent(7,0.846659);
   Func__1->SetBinContent(8,0.848506);
   Func__1->SetBinContent(9,0.850145);
   Func__1->SetBinContent(10,0.851604);
   Func__1->SetBinContent(11,0.857514);
   Func__1->SetBinContent(12,0.863548);
   Func__1->SetBinContent(13,0.869116);
   Func__1->SetBinContent(14,0.874097);
   Func__1->SetBinContent(15,0.878448);
   Func__1->SetBinContent(16,0.88217);
   Func__1->SetBinContent(17,0.885305);
   Func__1->SetBinContent(18,0.887923);
   Func__1->SetBinContent(19,0.890128);
   Func__1->SetBinContent(20,0.891974);
   Func__1->SetBinContent(21,0.893513);
   Func__1->SetBinContent(22,0.894796);
   Func__1->SetBinContent(23,0.895866);
   Func__1->SetBinContent(24,0.896761);
   Func__1->SetBinContent(25,0.897513);
   Func__1->SetBinContent(26,0.898147);
   Func__1->SetBinContent(27,0.898683);
   Func__1->SetBinContent(28,0.899139);
   Func__1->SetBinContent(29,0.899531);
   Func__1->SetBinContent(30,0.899868);
   Func__1->SetBinContent(31,0.90016);
   Func__1->SetBinContent(32,0.900415);
   Func__1->SetBinContent(33,0.900639);
   Func__1->SetBinContent(34,0.900836);
   Func__1->SetBinContent(35,0.90101);
   Func__1->SetBinContent(36,0.901165);
   Func__1->SetBinContent(37,0.901304);
   Func__1->SetBinContent(38,0.901431);
   Func__1->SetBinContent(39,0.90164);
   Func__1->SetBinContent(40,0.901817);
   Func__1->SetBinContent(41,0.901969);
   Func__1->SetBinContent(42,0.902099);
   Func__1->SetBinContent(43,0.902212);
   Func__1->SetBinContent(44,0.902309);
   Func__1->SetBinContent(45,0.902392);
   Func__1->SetBinContent(46,0.902469);
   Func__1->SetBinContent(47,0.902536);
   Func__1->SetBinContent(48,0.902595);
   Func__1->SetBinContent(49,0.902646);
   Func__1->SetBinContent(50,0.902692);
   Func__1->SetBinContent(51,0.902733);
   Func__1->SetBinContent(52,0.902769);
   Func__1->SetBinContent(53,0.902801);
   Func__1->SetBinContent(54,0.90283);
   Func__1->SetBinContent(55,0.902857);
   Func__1->SetBinContent(56,0.902881);
   Func__1->SetBinContent(57,0.902902);
   Func__1->SetBinContent(58,0.902922);
   Func__1->SetBinContent(59,0.90294);
   Func__1->SetBinContent(60,0.902956);
   Func__1->SetBinContent(61,0.90297);
   Func__1->SetBinContent(62,0.902984);
   Func__1->SetBinContent(63,0.902996);
   Func__1->SetBinContent(64,0.903007);
   Func__1->SetBinContent(65,0.903017);
   Func__1->SetBinContent(66,0.903026);
   Func__1->SetBinContent(67,0.903034);
   Func__1->SetBinContent(68,0.903042);
   Func__1->SetBinContent(69,0.903049);
   Func__1->SetBinContent(70,0.903055);
   Func__1->SetBinContent(71,0.903061);
   Func__1->SetBinContent(72,0.903066);
   Func__1->SetBinContent(73,0.903071);
   Func__1->SetBinContent(74,0.903075);
   Func__1->SetBinContent(75,0.903079);
   Func__1->SetBinContent(76,0.90308);
   Func__1->SetBinContent(77,0.90308);
   Func__1->SetBinContent(78,0.90308);
   Func__1->SetBinContent(79,0.90308);
   Func__1->SetBinContent(80,0.90308);
   Func__1->SetBinContent(81,0.90308);
   Func__1->SetBinContent(82,0.90308);
   Func__1->SetBinContent(83,0.90308);
   Func__1->SetBinContent(84,0.90308);
   Func__1->SetBinContent(85,0.90308);
   Func__1->SetBinContent(86,0.90308);
   Func__1->SetBinContent(87,0.90308);
   Func__1->SetBinContent(88,0.90308);
   Func__1->SetBinContent(89,0.90308);
   Func__1->SetBinContent(90,0.90308);
   Func__1->SetBinContent(91,0.90308);
   Func__1->SetBinContent(92,0.90308);
   Func__1->SetBinContent(93,0.90308);
   Func__1->SetBinContent(94,0.90308);
   Func__1->SetBinContent(95,0.90308);
   Func__1->SetBinContent(96,0.90308);
   Func__1->SetBinContent(97,0.90308);
   Func__1->SetBinContent(98,0.90308);
   Func__1->SetBinContent(99,0.90308);
   Func__1->SetBinContent(100,0.90308);
   Func__1->SetEntries(700);
   Func__1->SetDirectory(0);
   Func__1->SetStats(0);
   Func__1->SetFillColor(19);
   Func__1->SetFillStyle(0);
   Func__1->SetLineColor(9);
   Func__1->SetLineWidth(3);
   Func__1->SetMarkerStyle(20);
   Func__1->GetXaxis()->SetTitleOffset(1.2);
   //   Func__1->Draw("");
   return Func__1;
}
//________________________________________________________________________________
Double_t StdEdxModel::NpCorrection(Double_t betagamma) {
  Double_t bgL10 = TMath::Log10(betagamma);
  bgL10 = TMath::Max(-2.0, TMath::Min(1.0,bgL10));
  static TH1D *eff = 0;
  if (! eff) eff = protonEff();
  //  return 1.03*eff->Interpolate(bgL10);
  return eff->Interpolate(bgL10);
}
//________________________________________________________________________________
Double_t StdEdxModel::dNdxEff(Double_t poverm, Double_t charge) {
  if (!fgStdEdxModel) instance();
  Double_t bgMC = bgCorrected(poverm); 
  Double_t dNdxMC = dNdx(bgMC, charge);
  Double_t dNdx = dNdxMC*NpCorrection(poverm); 
  return dNdx;
}
