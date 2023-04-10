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
#include "TCallf77.h"
#ifndef WIN32
#define ggderiv ggderiv_
#else
#define ggderiv GGDERIV
#endif
extern "C" {
  void type_of_call ggderiv(Double_t &, Double_t &, Double_t &, Double_t &, Double_t &, Double_t *);
};
//#include "StMessMgr.h" 
using namespace std;
ClassImp(StdEdxModel)
StdEdxModel  *StdEdxModel::fgStdEdxModel = 0;
Int_t         StdEdxModel::_debug   = 1;
//________________________________________________________________________________
StdEdxModel* StdEdxModel::instance() {
  if (! fgStdEdxModel) new StdEdxModel();
  return fgStdEdxModel;
}
//________________________________________________________________________________
StdEdxModel::StdEdxModel() : mdNdx(0), fScale(1)
			   , fTmaxL10eV(5) // Tcut = 100 keV
			   , fGGaus(0), fGausExp(0)
			   , fpol2F(0), fpol5F(0), fpol6F(0), fpol7F(0)
			   , fLogkeVperElectron(0)
{
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
  InitPar();
  // Set normalization point the same as for I70 (increase energy per conduction electron from 20 eB to 52 eV)
  Double_t dEdxMIPLog = TMath::Log(2.62463815285237434); //TMath::Log(2.39761562607903311); // [keV/cm] for dX = 2 cm
  Double_t MIPBetaGamma10 = TMath::Log10(4.);
  //                  log2dx, charge
  Double_t pars[3] = {   1.0,    1.0};
  Double_t dEdxLog = zMP(&MIPBetaGamma10, pars);
  fLogkeVperElectron = dEdxMIPLog - dEdxLog;
  cout << "StdEdxModel:: set scale = " << Form("%5.1f",1e3*keVperElectron()) << " eV/electron" << endl;
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
  if (mdNdx)    {
    Double_t dNdx = fScale*Q_eff*Q_eff*mdNdx->Interpolate(poverm);
    return dNdx;
  }
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
  Double_t GA =                  TMath::Gaus(alpha*t,0,1,kTRUE);
  Double_t E = (1. + TMath::Erf(alpha*v));
  Double_t V = G/w*E;
  if (k == 0) return V;
  Double_t dVdNormL = V;
  if (k == 1) return dVdNormL;
  /*
    dV/V = dG/G - dw/w + dE/E
    dG/G = d(-t**2/2) = -t * dt;
    dt  = d((X - ksi)/w) = -dksi/w -dw *t/w 
    GA = 1/sqrt(2*pi) exp(-(alpha*t)**2/)
    dE/E = GA/E*((alpha*t)*(dalpha*t + alpha*dt) = GA/E*((alpha*t)*(dalpha*t + alpha*(-dksi/w -dw *t/w)))
    dV/V =  -t * dt -dw/w +  GA/E*((alpha*t)*(dalpha*t + alpha*(-dksi/w -dw *t/w)))

    dlog(V)/

   */
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
//Double_t dVdksi = - V/w*(1 + GA/E*alpha/TMath::Sqrt2());
  Double_t dVdksi = - (V + G/w*GA*alpha/TMath::Sqrt2())/w;
  if (k == 2) return dVdksi;
//Double_t dVdw   = - V/w*(2 + GA/E*alpha/TMath::Sqrt2());
  Double_t dVdw   = - (2*V + G/w*GA*alpha/TMath::Sqrt2())/w;
  if (k == 3) return dVdw;
  Double_t dVdalpha = GA*v;
  return dVdalpha;
}
//_______________________________________________________________________________
Double_t StdEdxModel::ggaus(Double_t *x, Double_t *p) {
  return ggausD(x,p,0);
}
//_______________________________________________________________________________
  Double_t StdEdxModel::ggausD(Double_t *x, Double_t *p, Double_t *der) {
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
  Double_t V = gausw(x, par);
  if (der) {
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
     f90(%);%(1,1) = 1
%(1,2) = -(sqrt(%pi*alpha**2+%pi)*exp(-(2*%pi)/abs(alpha))*(sqrt(%pi)*sqrt(alpha**2+1)*((6*%pi**2-24*%pi+16)*alpha**5+(10*%pi**2-24*%pi)*alpha**3+4*%pi**2*alpha)*exp((2*%pi)/abs(alpha))*abs(alpha)+((-sqrt(2)*%pi**3)+2**(5.0d+0/2.0d+0)*%pi**2-2**(5.0d+0/2.0d+0)*%pi)*alpha**7+((-3*sqrt(2)*%pi**3)+2**(7.0d+0/2.0d+0)*%pi**2-2**(5.0d+0/2.0d+0)*%pi)*alpha**5+(2**(5.0d+0/2.0d+0)*%pi**2-3*sqrt(2)*%pi**3)*alpha**3-sqrt(2)*%pi**3*alpha))/(sqrt((%pi-2)*alpha**2+%pi)*((2**(3.0d+0/2.0d+0)*%pi**3-2**(7.0d+0/2.0d+0)*%pi**2+2**(7.0d+0/2.0d+0)*%pi)*alpha**6+(3*2**(3.0d+0/2.0d+0)*%pi**3-2**(9.0d+0/2.0d+0)*%pi**2+2**(7.0d+0/2.0d+0)*%pi)*alpha**4+(3*2**(3.0d+0/2.0d+0)*%pi**3-2**(7.0d+0/2.0d+0)*%pi**2)*alpha**2+2**(3.0d+0/2.0d+0)*%pi**3)*abs(alpha))
%(1,3) = (sqrt((%pi-2)*alpha**2+%pi)*sqrt(%pi*alpha**2+%pi)*exp(-(2*%pi)/abs(alpha))*(sqrt(%pi)*sqrt(alpha**2+1)*(((2*%pi**4-12*%pi**3+24*%pi**2-16*%pi)*alpha**8+(8*%pi**4-36*%pi**3+48*%pi**2-16*%pi)*alpha**6+(12*%pi**4-36*%pi**3+24*%pi**2)*alpha**4+(8*%pi**4-12*%pi**3)*alpha**2+2*%pi**4)*abs(alpha)+(2*%pi**2-8*%pi+8)*alpha**8+(4*%pi**2-8*%pi)*alpha**6+2*%pi**2*alpha**4)*sigma+(((-5*sqrt(2)*%pi**3)+2**(9.0d+0/2.0d+0)*%pi**2+2**(7.0d+0/2.0d+0)*%pi)*alpha**8+((-3*2**(5.0d+0/2.0d+0)*%pi**3)+9*2**(5.0d+0/2.0d+0)*%pi**2+2**(7.0d+0/2.0d+0)*%pi)*alpha**6+(5*2**(5.0d+0/2.0d+0)*%pi**2-9*sqrt(2)*%pi**3)*alpha**4-2**(3.0d+0/2.0d+0)*%pi**3*alpha**2)*exp((2*%pi)/abs(alpha))*abs(alpha)*sigma))/(sqrt(%pi)*sqrt(alpha**2+1)*((2*%pi**4-16*%pi**3+48*%pi**2-64*%pi+32)*alpha**12+(10*%pi**4-64*%pi**3+144*%pi**2-128*%pi+32)*alpha**10+(20*%pi**4-96*%pi**3+144*%pi**2-64*%pi)*alpha**8+(20*%pi**4-64*%pi**3+48*%pi**2)*alpha**6+(10*%pi**4-16*%pi**3)*alpha**4+2*%pi**4*alpha**2)*abs(alpha))
%(2,1) = 0
%(2,2) = sqrt(%pi*alpha**2+%pi)/sqrt((%pi-2)*alpha**2+%pi)
%(2,3) = (2*alpha*sqrt(%pi*alpha**2+%pi)*sigma)/(sqrt((%pi-2)*alpha**2+%pi)*((%pi-2)*alpha**4+(2*%pi-2)*alpha**2+%pi))
%(3,1) = 0
%(3,2) = 0
%(3,3) = 1

     
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
    der[0] = der[1] = der[2] = 0;
    ggderiv(x[0], ksi, sigma, w, alpha, der);
  }
  return V;
}
//_______________________________________________________________________________
Double_t StdEdxModel::gausexp(Double_t *x, Double_t *p) {
  return gausexpD(x,p);
}
//_______________________________________________________________________________
Double_t StdEdxModel::gausexpD(Double_t *x, Double_t *p, Double_t *der) {
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
  Double_t dVdt = 0;
  Double_t dVdk = 0;
  if (t < k) {
    V = TMath::Exp(-t*t/2); // dV/dt = - V * t => - k * V
    dVdt = -V*t;
  } else {
    V = TMath::Exp(k*k/2 - k*t); // dV/dt =       - k * V 
    dVdt = -V*k;
    dVdk =  V*(k - t);
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
  Double_t Prob = TMath::Exp(normL)*V/N;
  if (der) {
    Double_t dtdM = -1./sigma;
    Double_t dtdS = -t /sigma;
    Double_t dDdk = TMath::Exp(-k*k/2.);
    Double_t dCdk = - C*(k + 1./(k*k));
    Double_t dNdk = (dDdk + dCdk) * sigma;
    Double_t dPdM = Prob * dVdt / V * dtdM; // over mu
    Double_t dPdS = Prob * dVdt / V * dtdS; // over sigma
    Double_t dPdk = Prob *(dVdk / V - dNdk / N);
    der[0] = dPdM;
    der[1] = dPdS;
    der[2] = dPdk;
  }
  return Prob;
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
void StdEdxModel::Parameters(Double_t Np, Double_t *parameters, Double_t *dPardNp) {
  parameters[0] = parameters[1] = parameters[2] = 0;
  if (Np <= 1.0) return;
  for (Int_t l = 0; l < 3; l++) {
    if (! dPardNp) {
      parameters[l] = Parameter(Np, l);
    } else {
      parameters[l] = Parameter(Np, l, &dPardNp[l]);
    }
  }
}
//________________________________________________________________________________
Double_t StdEdxModel::Parameter(Double_t Np, Int_t l, Double_t *dPardNp) {
  // parameters from dEdxFit::FitGG4
  static Double_t parsA[2] = {    5.4634,   -0.57598}; //alpha x
  static Double_t parsS[3] = {    1.6924,    -1.2912,    0.24698}; //sigma versus log(x)	 
  static Double_t parsM[8] = {   -4.3432,     4.6327,    -1.9522,     0.4691,  -0.066615,  0.0055111, -0.00024531, 4.5394e-06}; //mu pol7
  Double_t x = TMath::Log(Np);
  Double_t dxdNp = 1./Np;
  if (l == 2) {
    Double_t alpha  = parsA[0] + x *  parsA[1];
    if (dPardNp) dPardNp[0] =  parsA[1] * dxdNp;
    return alpha;
  } else if (l == 1) {
    Double_t xx = (x > 0) ? TMath::Log(x) : 0;
    Double_t sigma = parsS[0] + xx * ( parsS[1] + xx * parsS[2]);
    if (dPardNp) {
      Double_t dxxdx = 1./xx;
      Double_t dxxdNp = dxxdx * dxdNp;
      dPardNp[0] = (parsS[1] + 2 * xx * parsS[2]) * dxxdNp;
    }
    return sigma;
  } else if (l == 0) {
    Double_t mu = fpol7F->EvalPar(&x, parsM);
    if (dPardNp) {
      static Double_t parsMD[7] = {0};
      if (!parsMD[0]) {
	for (Int_t i = 0; i < 7; i++) {
	  parsMD[i] = (i+1)*parsM[i+1];
	}
      }
      dPardNp[0] = fpol6F->EvalPar(&x, parsMD) * dxdNp;
    }
    return mu;
  } else {
    assert(0);
    return 0;
  }
}
//________________________________________________________________________________
Double_t StdEdxModel::MukeV(Double_t Np) {
  return Parameter(Np, 0) + fLogkeVperElectron + TMath::Log(Np);
}
//________________________________________________________________________________
Double_t StdEdxModel::funParam(Double_t *x, Double_t *p) {
  Int_t l = p[0];
  if (l < 0 || l > 2) return 0;
  Double_t Np = TMath::Exp(x[0]);
  return StdEdxModel::instance()->Parameter(Np, l);
}
//________________________________________________________________________________
TF1 *StdEdxModel::FParam(Int_t l) {
  const Char_t *fNames[3] = {"MuPar","sigmaPar","alphaPar"};
  TF1 *f = 0;
  if (l < 0 || l > 2) return f;
  f = (TF1 *) gROOT->GetListOfFunctions()->FindObject(fNames[l]);
  if (! f) {
    f = new TF1(fNames[l],funParam,1.5,12,1);
    f->SetParName(0,fNames[l]);
    f->SetParameter(0,l);
    cout << "Create FParam with name " << f->GetName() << endl;
  }
  return f;
  
}
//________________________________________________________________________________
Double_t StdEdxModel::Prob(Double_t /* log(nE/Np) */ ee, Double_t Np, Double_t *der) { // GG: ggaus
  Double_t params[4] = {0};
  Double_t V = 0;
  if (! der) {
    Parameters(Np, &params[1]);
    V = ggaus(&ee, params);
  } else {
    Double_t dPardNp[3] = {0};
    Parameters(Np, &params[1], dPardNp);
    Double_t dVdP[3] = {0};
    V = ggausD(&ee, params, dVdP);
    der[0] = 0;
    for (Int_t l = 0; l < 3; l++) der[0] += dVdP[l]*dPardNp[l];
    static Int_t _debug = 0;
    if (_debug) {
      Double_t xP = TMath::Log(Np);
      Double_t D = instance()->FProbP()->Derivative(xP,&ee)/Np;
      cout << "estimated derivative (xP = " << xP << ", ee = " << ee << ") = " << der[0] << " calculated derivative = " << D << endl;
    }
  }
  return V;
}
//--------------------------------------------------------------------------------
Double_t StdEdxModel::funcProb(Double_t *x, Double_t *p) {
  return instance()->Prob(x[0],p[0]);
}
//________________________________________________________________________________
TF1 *StdEdxModel::FProb() {
  const Char_t *name = "GGProb";
  TF1 *f =  (TF1 *) gROOT->GetListOfFunctions()->FindObject(name);
  if (! f) {
    f = new TF1(name,funcProb,-1,4,1);
    f->SetParName(0,"Np");
    f->SetParameter(0,32);
    cout << "Create FProb with name " << f->GetName() << endl;
  }
  return f;
}
//--------------------------------------------------------------------------------
Double_t StdEdxModel::funcProbP(Double_t *x, Double_t *p) {
  if (p[0] < 1) return 0;
  return instance()->Prob(TMath::Log(p[0]),x[0]);
}
//________________________________________________________________________________
TF1 *StdEdxModel::FProbP() {
  const Char_t *name = "GGProbP";
  TF1 *f =  (TF1 *) gROOT->GetListOfFunctions()->FindObject(name);
  if (! f) {
    f = new TF1(name,funcProbP,2,12,1);
    f->SetParName(0,"x");
    f->SetParameter(0,1);
    cout << "Create FProbP with name " << f->GetName() << endl;
  }
  return f;
}
//--------------------------------------------------------------------------------
Double_t StdEdxModel::funcProbDer(Double_t *x, Double_t *p) {
  Double_t der;
  instance()->Prob(x[0], p[0], &der);
  return der;
}
//________________________________________________________________________________
TF1 *StdEdxModel::FProbDer() {
  const Char_t *name = "GGProbDer";
  TF1 *f =  (TF1 *) gROOT->GetListOfFunctions()->FindObject(name);
  if (! f) {
    f = new TF1(name,funcProbDer,-1,4,1);
    f->SetParName(0,"Np");
    f->SetParameter(0,32);
    cout << "Create FProb with name " << f->GetName() << endl;
  }
  return f;
}
#if 0
//________________________________________________________________________________
Double_t StdEdxModel::ProbEx(Double_t /* log(nE/Np) */ ee, Double_t Np, Double_t *der) { // GEX : gausexp
  Double_t params[4] = {0};
  Double_t V = 0;
  if (! der) {
    Parameters(Np, &params[1]);
    V = gausexp(&ee, params);
  } else {
    Double_t dPardNp[3] = {0};
    Parameters(Np, &params[1], dPardNp);
    Double_t dVdP[3] = {0};
    V = gausexpD(&ee, params, dVdP);
    der[0] = 0;
    for (Int_t l = 0; l < 3; l++) der[0] += dVdP[l]*dPardNp[l];
  }
  return V;
}
#endif
//________________________________________________________________________________
Double_t StdEdxModel::ProbdEGeVlog(Double_t dEGeVLog, Double_t Np, Double_t *der) {
  Double_t ee = Logne(dEGeVLog) - TMath::Log(Np);
  return Prob(ee, Np, der);
}
//________________________________________________________________________________
Double_t StdEdxModel::zMP(Double_t *x, Double_t *p) { // log(keV/cm)
  Double_t log10bg = x[0];
  Double_t pOverMRC  = TMath::Power(10., log10bg);
  Double_t log2dx  = p[0];
  Double_t charge  = p[1];
  Double_t dx      = TMath::Power( 2., log2dx);
  Double_t dNdx = StdEdxModel::instance()->dNdxEff(pOverMRC, charge); // */dNdxVsBgC*.root [-1.5,5]
  Double_t Np = dNdx*dx;
  //  Double_t NpLog = TMath::Log(Np);
  //  Double_t mu    = instance()->Parameter(Np, 0);
  //  Double_t sigma = instance()->Parameter(Np, 1);
  //  Double_t alpha = instance()->Parameter(Np, 2);
  //  Double_t dEkeVLog = NpLog + mu -3.13746587897608142e+00 +1.78334647296254700e-01;// + 7.02725079814016507e+00;// - 3.13746587897608142e+00;// 43.4 eV/conducting electron 
  Double_t dEkeVLog = instance()->MukeV(Np); // Parameter(Np, 0); 
  Double_t dEdxLog  = dEkeVLog - TMath::Log(dx);
  return   dEdxLog;
}
//________________________________________________________________________________
TF1 *StdEdxModel::ZMP(Double_t log2dx) {
  TString fName(Form("New%i",(int)(2*(log2dx+2))));
  TF1 *f = (TF1 *) gROOT->GetListOfFunctions()->FindObject(fName);
  if (! f) {
    f = new TF1(fName,zMP,-2,5,2);
    f->SetParName(0,"log2dx");
    f->SetLineStyle(4);
    f->SetParameter(0,log2dx);
    f->SetParameter(1, 1.0); // charge
    cout << "Create ZMPNew with name " << f->GetName() << " for log2dx = " << log2dx << endl;
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
   
   TH1D *eff = new TH1D("protonEff","",100,-2.0,0.0);
   Double_t corr[102] = { 0, 
			  0.8300, 0.8335, 0.8367, 0.8396, 0.8422, 0.8446, 0.8467, 0.8485, 0.8501, 0.8516, 
			  0.8575, 0.8635, 0.8691, 0.8741, 0.8784, 0.8822, 0.8853, 0.8879, 0.8901, 0.8920, 
			  0.8935, 0.8948, 0.8959, 0.8968, 0.8975, 0.8981, 0.8987, 0.8991, 0.8995, 0.8999, 
			  0.9002, 0.9004, 0.9006, 0.9008, 0.9010, 0.9012, 0.9013, 0.9014, 0.9016, 0.9018, 
			  0.9020, 0.9021, 0.9022, 0.9023, 0.9024, 0.9025, 0.9025, 0.9026, 0.9026, 0.9027, 
			  0.9027, 0.9028, 0.9028, 0.9028, 0.9029, 0.9029, 0.9029, 0.9029, 0.9029, 0.9030, 
			  0.9030, 0.9030, 0.9030, 0.9030, 0.9030, 0.9030, 0.9030, 0.9030, 0.9030, 0.9031, 
			  0.9031, 0.9031, 0.9031, 0.9031, 0.9031, 0.9031, 0.9031, 0.9031, 0.9031, 0.9031, 
			  0.9031, 0.9031, 0.9031, 0.9031, 0.9031, 0.9031, 0.9031, 0.9031, 0.9031, 0.9031, 
			  0.9031, 0.9031, 0.9031, 0.9031, 0.9031, 0.9031, 0.9031, 0.9031, 0.9031, 0.9031,
			  0};
   eff->Set(102, corr);
   eff->SetDirectory(0);
   eff->SetStats(0);
   eff->SetFillColor(19);
   eff->SetFillStyle(0);
   eff->SetLineColor(9);
   eff->SetLineWidth(3);
   eff->SetMarkerStyle(20);
   eff->GetXaxis()->SetTitleOffset(1.2);
   //   eff->Draw("");
   return eff;
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
  Double_t bgL10 = TMath::Log10(poverm);
  static TF1 *elCor = 0;
  static TF1 *elCor2 = 0;
  static TF1 *piCor = 0;
  // Modification to dN/dx from analysis of daughter tracks of unique reconstructed strange particle V0 decays and gamma conversions. OO200GeV_2021 samples
  Double_t dNdxCor = 0;
  if (bgL10 > 2.3) {
    if (bgL10 > 3.5) bgL10 = 3.5;
    if (! elCor) {elCor = new TF1("dNdxElCor","pol2",2.3,3.5); elCor->SetParameters(-0.66617,    0.42779,  -0.059554);}
    dNdxCor = elCor->Eval(bgL10);
  } 
  if (bgL10 > 2.1 && bgL10 < 2.55) {
    if (! elCor2) {elCor2 = new TF1("dNdxElCor2","pol7",2.1,2.55); elCor2->SetParameters(  123.04,    -102.77,    -4.7415,     12.443,     4.1275,   -0.88384,    -1.0646,    0.25247);}
    dNdxCor += elCor2->Eval(bgL10);
  }
  if (bgL10 < 2) {
    if (bgL10 < -0.6) bgL10 = -0.6;
    if (bgL10 >  1.0) bgL10 =  1.0;
    if (! piCor) {piCor = new TF1("dNdxPionCor","pol6",0.6,1.0); piCor->SetParameters( -0.026971,  -0.015765,    0.19093, -0.00048008,   -0.24187,  -0.072279,    0.17227);}
    dNdxCor = piCor->Eval(bgL10);
  }
  return dNdx*TMath::Exp(dNdxCor);
}
//________________________________________________________________________________
Double_t StdEdxModel::extremevalueG(Double_t *x, Double_t *p) {
  Double_t normL  = p[0];
  Double_t mu     = p[1];
  Double_t sigmaI = p[2];
  Double_t phase  = p[3];
  Double_t sigmaG = p[4];
  Double_t t = (mu - x[0])*sigmaI;
  Double_t frac = TMath::Sin(phase);
  frac *= frac;
  return TMath::Exp(normL)*((1. - frac)*TMath::Abs(sigmaI)*TMath::Exp(t - TMath::Exp(t)) + frac*TMath::Gaus(t, 0., sigmaG, kTRUE));
}
//________________________________________________________________________________
TF1 *StdEdxModel::ExValG() {
  TString fName("ExValG");
  TF1 *f = (TF1 *) gROOT->GetListOfFunctions()->FindObject(fName);
  if (! f) {
    f = new TF1(fName, extremevalueG, -2, 5, 5);
    f->SetParNames("normL","mu","sigmaI", "phase","sigmaG");
    f->SetParLimits(2, 0.1, 10.0);
    f->SetParLimits(3, 0., TMath::PiOver2());
    //    f->SetParLimits(4, 0.1, 1.0);
    f->FixParameter(4, 1.0);
  }
  f->SetParameters(0., 0., 2.5, 0.75, 1.0);
  return f;
}
