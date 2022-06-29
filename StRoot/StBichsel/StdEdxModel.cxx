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
//#include "StMessMgr.h" 
using namespace std;
ClassImp(StdEdxModel)
StdEdxModel  *StdEdxModel::fgStdEdxModel = 0;
TH1D         *StdEdxModel::mdNdxL10 = 0;  
TH1D         *StdEdxModel::mdNdx = 0;  
Double_t      StdEdxModel::fScale = 1; //TMath::Exp(9.12015e-02); // Bichsel
Int_t         StdEdxModel::_debug   = 1;
TF1          *StdEdxModel::fGGaus = 0;
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
//________________________________________________________________________________//________________________________________________________________________________
Double_t StdEdxModel::ggaus(Double_t *x, Double_t *p) {
  Int_t    k     = p[4];
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
  /* maxima
stardisp: true$
delta(alpha) := alpha/sqrt(1 + alpha*alpha);
muz(alpha) := delta(alpha)/sqrt(%pi/2);
sigmaz(alpha) := sqrt(1 - muz(alpha)*muz(alpha));
gamma1(alpha) := (4 - %pi)/2 * (delta(alpha)/sqrt(%pi/2))**3 / (1 - 2*delta(alpha)**2 /%pi)**1.5;
m_0(alpha) := muz(alpha) - gamma1(alpha)/sigmaz(alpha)/2 - signum(alpha)/2*exp(-2*%pi/abs(alpha));
w(sigma,alpha):= sigma/sqrt(1 - 2*delta(alpha)**2/%pi);
ksi(mu,sigma,alpha):= mu - w(sigma,alpha)*m_0(alpha);

F : jacobian([  ksi(mu,sigma,alpha), w(sigma,alpha), alpha], [mu, sigma, alpha]);
trigsimp(%);
fortran(%);


t(ksi,w) := (x - ksi)/w;
v(ksi,w) := t(ksi,w)/sqrt(2);
G(ksi,w) := gaussprob(t(ksi,w))/w;
E(ksi,w,alpha) := 1 + erf(alpha*v(ksi,w));
Val(ksi,w,alpha):= gaussprob(t(ksi,w))/w * ( 1 + erf(alpha*v(ksi,w)));

J : jacobian([Val(ksi,w,alpha)], [ksi,w,alpha]);
trigsimp(%);
fortran(%);

T: J . F; 
trigsimp(%);
fortran(%);
 
  */
}
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
  Double_t X = x[0];
Double_t pi = TMath::Pi();
Double_t pi2 = pi*pi;
Double_t alpha2 = alpha*alpha;
Double_t alpha3 = alpha*alpha2;
Double_t alpha5 = alpha2*alpha3;
Double_t X2 = X*X;
Double_t w2 = w*w;
Double_t ksi2 = ksi*ksi;
Double_t aXw2 = ((alpha2*X2)/w2)/2.;
Double_t akw2 = ((alpha2*ksi2)/w2)/2.;

Dpubel_t Xw2 =  (X2/w2)/2.;
Double_t kw2 =  (ksi2/w2)/2.;
  Double_t t = (X  - ksi)/w;
Double_t SQ2 = TMath::Sqrt2();
Double_t SQ2p3 = 
  Double_t v = t/SQ2;
Double_t w2  = w*w;
Double_t w3  = w2*w;
Double_t w4 = w2*w2;
Double_t pi32 = TMath::Power(TMath::Pi(), -3./2.);
%(1,1) = -(pi32*TMath::Exp(-aXw2-Xw2-akw2-kw2)*((pi*ksi*TMath::Exp(akw2)-pi*TMath::Exp(akw2)*X)*TMath::Exp(aXw2+(ksi*X)/w2)*erf((alpha*v))+pi*TMath::Exp(akw2)*(ksi-X)*TMath::Exp(aXw2+(ksi*X)/w2)+TMath::Sqrt(2)*TMath::Sqrt(pi)*alpha*w*TMath::Exp((alpha2`%*ksi*X)/w2+(ksi*X)/w2)))/(TMath::Sqrt(2)*w3)

%(1,2) = -(TMath::Sqrt((pi-2)*alpha2+pi)*TMath::Sqrt(pi*alpha2+pi)*TMath::Exp((-aXw2)-Xw2-akw2-kw2-(2*pi)/TMath::Abs(alpha))*(TMath::Sqrt(alpha2+1)*(TMath::Sqrt(pi)*((((6*pi2-24*pi+16)*alpha5+(10*pi2-24*pi)*alpha3+4*pi2*alpha)*TMath::Abs(alpha)*w*TMath::Exp(akw2+(2*pi)/TMath::Abs(alpha))*X+(((-6*pi2)+24*pi-16)*alpha5+(24*pi-10*pi2)*alpha3-4*pi2*alpha)*TMath::Abs(alpha)*ksi*w*TMath::Exp(akw2+(2*pi)/TMath::Abs(alpha)))*TMath::Exp(aXw2+(ksi*X)/w2)*erf((alpha*v))+(((6*pi2-24*pi+16)*alpha5+(10*pi2-24*pi)*alpha3+4*pi2*alpha)*TMath::Abs(alpha)*w*TMath::Exp(akw2+(2*pi)/TMath::Abs(alpha))*X+(((-6*pi2)+24*pi-16)*alpha5+(24*pi-10*pi2)*alpha3-4*pi2*alpha)*TMath::Abs(alpha)*ksi*w*TMath::Exp(akw2+(2*pi)/TMath::Abs(alpha)))*TMath::Exp(aXw2+(ksi*X)/w2))+(((-3*2**(3./2.)*pi2)+3*2**(7./2.)*pi-2**(9./2.))*alpha**6+(3*2**(7./2.)*pi-5*2**(3./2.)*pi2)*alpha**4-2**(5./2.)*pi2*alpha2)*TMath::Abs(alpha)*w2*TMath::Exp((alpha2*ksi*X)/w2+(ksi*X)/w2+(2*pi)/TMath::Abs(alpha)))+((((-2**(3./2.)*pi**3)+2**(7./2.)*pi2-2**(7./2.)*pi)*alpha**6+((-3*2**(3./2.)*pi**3)+2**(9./2.)*pi2-2**(7./2.)*pi)*alpha**4+(2**(7./2.)*pi2-3*2**(3./2.)*pi**3)*alpha2-2**(3./2.)*pi**3)*TMath::Abs(alpha)*TMath::Exp(akw2+(2*pi)/TMath::Abs(alpha))*X2+((((-TMath::Sqrt(2)*pi**3)+2**(5./2.)*pi2-2**(5./2.)*pi)*alpha**7+((-3*TMath::Sqrt(2)*pi**3)+2**(7./2.)*pi2-2**(5./2.)*pi)*alpha5+(2**(5./2.)*pi2-3*TMath::Sqrt(2)*pi**3)*alpha3-TMath::Sqrt(2)*pi**3*alpha)*w+((2**(5./2.)*pi**3-2**(9./2.)*pi2+2**(9./2.)*pi)*alpha**6+(3*2**(5./2.)*pi**3-2**(1.1000000000000001d+1/2.)*pi2+2**(9./2.)*pi)*alpha**4+(3*2**(5./2.)*pi**3-2**(9./2.)*pi2)*alpha2+2**(5./2.)*pi**3)*TMath::Exp((2*pi)/TMath::Abs(alpha))*TMath::Abs(alpha)*ksi)*TMath::Exp(akw2)*X+(((2**(3./2.)*pi**3-2**(7./2.)*pi2+2**(7./2.)*pi)*alpha**6+(3*2**(3./2.)*pi**3-2**(9./2.)*pi2+2**(7./2.)*pi)*alpha**4+(3*2**(3./2.)*pi**3-2**(7./2.)*pi2)*alpha2+2**(3./2.)*pi**3)*TMath::Exp((2*pi)/TMath::Abs(alpha))*TMath::Abs(alpha)*w2+((TMath::Sqrt(2)*pi**3-2**(5./2.)*pi2+2**(5./2.)*pi)*alpha**7+(3*TMath::Sqrt(2)*pi**3-2**(7./2.)*pi2+2**(5./2.)*pi)*alpha5+(3*TMath::Sqrt(2)*pi**3-2**(5./2.)*pi2)*alpha3+TMath::Sqrt(2)*pi**3*alpha)*ksi*w+(((-2**(3./2.)*pi**3)+2**(7./2.)*pi2-2**(7./2.)*pi)*alpha**6+((-3*2**(3./2.)*pi**3)+2**(9./2.)*pi2-2**(7./2.)*pi)*alpha**4+(2**(7./2.)*pi2-3*2**(3./2.)*pi**3)*alpha2-2**(3./2.)*pi**3)*TMath::Exp((2*pi)/TMath::Abs(alpha))*TMath::Abs(alpha)*ksi2)*TMath::Exp(akw2))*TMath::Exp(aXw2+(ksi*X)/w2)*erf((alpha*v))+((((-2**(3./2.)*pi**3)+2**(7./2.)*pi2-2**(7./2.)*pi)*alpha**6+((-3*2**(3./2.)*pi**3)+2**(9./2.)*pi2-2**(7./2.)*pi)*alpha**4+(2**(7./2.)*pi2-3*2**(3./2.)*pi**3)*alpha2-2**(3./2.)*pi**3)*TMath::Abs(alpha)*TMath::Exp(akw2+(2*pi)/TMath::Abs(alpha))*X2+((((-TMath::Sqrt(2)*pi**3)+2**(5./2.)*pi2-2**(5./2.)*pi)*alpha**7+((-3*TMath::Sqrt(2)*pi**3)+2**(7./2.)*pi2-2**(5./2.)*pi)*alpha5+(2**(5./2.)*pi2-3*TMath::Sqrt(2)*pi**3)*alpha3-TMath::Sqrt(2)*pi**3*alpha)*w+((2**(5./2.)*pi**3-2**(9./2.)*pi2+2**(9./2.)*pi)*alpha**6+(3*2**(5./2.)*pi**3-2**(1.1000000000000001d+1/2.)*pi2+2**(9./2.)*pi)*alpha**4+(3*2**(5./2.)*pi**3-2**(9./2.)*pi2)*alpha2+2**(5./2.)*pi**3)*TMath::Exp((2*pi)/TMath::Abs(alpha))*TMath::Abs(alpha)*ksi)*TMath::Exp(akw2)*X+(((2**(3./2.)*pi**3-2**(7./2.)*pi2+2**(7./2.)*pi)*alpha**6+(3*2**(3./2.)*pi**3-2**(9./2.)*pi2+2**(7./2.)*pi)*alpha**4+(3*2**(3./2.)*pi**3-2**(7./2.)*pi2)*alpha2+2**(3./2.)*pi**3)*TMath::Exp((2*pi)/TMath::Abs(alpha))*TMath::Abs(alpha)*w2+((TMath::Sqrt(2)*pi**3-2**(5./2.)*pi2+2**(5./2.)*pi)*alpha**7+(3*TMath::Sqrt(2)*pi**3-2**(7./2.)*pi2+2**(5./2.)*pi)*alpha5+(3*TMath::Sqrt(2)*pi**3-2**(5./2.)*pi2)*alpha3+TMath::Sqrt(2)*pi**3*alpha)*ksi*w+(((-2**(3./2.)*pi**3)+2**(7./2.)*pi2-2**(7./2.)*pi)*alpha**6+((-3*2**(3./2.)*pi**3)+2**(9./2.)*pi2-2**(7./2.)*pi)*alpha**4+(2**(7./2.)*pi2-3*2**(3./2.)*pi**3)*alpha2-2**(3./2.)*pi**3)*TMath::Exp((2*pi)/TMath::Abs(alpha))*TMath::Abs(alpha)*ksi2)*TMath::Exp(akw2))*TMath::Exp(aXw2+(ksi*X)/w2)+TMath::Sqrt(pi)*(((4*pi2-16*pi+16)*alpha**7+(12*pi2-32*pi+16)*alpha5+(12*pi2-16*pi)*alpha3+4*pi2*alpha)*TMath::Exp((2*pi)/TMath::Abs(alpha))*TMath::Abs(alpha)*w*X+((2*pi2-8*pi+8)*alpha**8+(6*pi2-16*pi+8)*alpha**6+(6*pi2-8*pi)*alpha**4+2*pi2*alpha2)*w2+(((-4*pi2)+16*pi-16)*alpha**7+((-12*pi2)+32*pi-16)*alpha5+(16*pi-12*pi2)*alpha3-4*pi2*alpha)*TMath::Exp((2*pi)/TMath::Abs(alpha))*TMath::Abs(alpha)*ksi*w)*TMath::Exp((alpha2*ksi*X)/w2+(ksi*X)/w2)))/(TMath::Sqrt(pi)*((4*pi**4-24*pi**3+48*pi2-32*pi)*alpha**8+(16*pi**4-72*pi**3+96*pi2-32*pi)*alpha**6+(24*pi**4-72*pi**3+48*pi2)*alpha**4+(16*pi**4-24*pi**3)*alpha2+4*pi**4)*TMath::Abs(alpha)*w**4)

%(1,3) = (TMath::Exp((-aXw2)-Xw2-akw2-kw2-(2*pi)/TMath::Abs(alpha))*(TMath::Sqrt(pi*alpha2+pi)*(TMath::Sqrt(alpha2+1)*(TMath::Sqrt(pi)*((((4*pi2-16*pi+16)*alpha**7+(8*pi2-16*pi)*alpha5+4*pi2*alpha3)*TMath::Abs(alpha)*sigma*TMath::Exp(akw2+(2*pi)/TMath::Abs(alpha))*X2+((((2*pi**4-12*pi**3+24*pi2-16*pi)*alpha**8+(8*pi**4-36*pi**3+48*pi2-16*pi)*alpha**6+(12*pi**4-36*pi**3+24*pi2)*alpha**4+(8*pi**4-12*pi**3)*alpha2+2*pi**4)*TMath::Abs(alpha)+(2*pi2-8*pi+8)*alpha**8+(4*pi2-8*pi)*alpha**6+2*pi2*alpha**4)*sigma*w+(((-8*pi2)+32*pi-32)*alpha**7+(32*pi-16*pi2)*alpha5-8*pi2*alpha3)*TMath::Exp((2*pi)/TMath::Abs(alpha))*TMath::Abs(alpha)*ksi*sigma)*TMath::Exp(akw2)*X+((((-4*pi2)+16*pi-16)*alpha**7+(16*pi-8*pi2)*alpha5-4*pi2*alpha3)*TMath::Exp((2*pi)/TMath::Abs(alpha))*TMath::Abs(alpha)*sigma*w2+((((-2*pi**4)+12*pi**3-24*pi2+16*pi)*alpha**8+((-8*pi**4)+36*pi**3-48*pi2+16*pi)*alpha**6+((-12*pi**4)+36*pi**3-24*pi2)*alpha**4+(12*pi**3-8*pi**4)*alpha2-2*pi**4)*TMath::Abs(alpha)+((-2*pi2)+8*pi-8)*alpha**8+(8*pi-4*pi2)*alpha**6-2*pi2*alpha**4)*ksi*sigma*w+((4*pi2-16*pi+16)*alpha**7+(8*pi2-16*pi)*alpha5+4*pi2*alpha3)*TMath::Exp((2*pi)/TMath::Abs(alpha))*TMath::Abs(alpha)*ksi2*sigma)*TMath::Exp(akw2))*TMath::Exp(aXw2+(ksi*X)/w2)*erf((alpha*v))+(((4*pi2-16*pi+16)*alpha**7+(8*pi2-16*pi)*alpha5+4*pi2*alpha3)*TMath::Abs(alpha)*sigma*TMath::Exp(akw2+(2*pi)/TMath::Abs(alpha))*X2+((((2*pi**4-12*pi**3+24*pi2-16*pi)*alpha**8+(8*pi**4-36*pi**3+48*pi2-16*pi)*alpha**6+(12*pi**4-36*pi**3+24*pi2)*alpha**4+(8*pi**4-12*pi**3)*alpha2+2*pi**4)*TMath::Abs(alpha)+(2*pi2-8*pi+8)*alpha**8+(4*pi2-8*pi)*alpha**6+2*pi2*alpha**4)*sigma*w+(((-8*pi2)+32*pi-32)*alpha**7+(32*pi-16*pi2)*alpha5-8*pi2*alpha3)*TMath::Exp((2*pi)/TMath::Abs(alpha))*TMath::Abs(alpha)*ksi*sigma)*TMath::Exp(akw2)*X+((((-4*pi2)+16*pi-16)*alpha**7+(16*pi-8*pi2)*alpha5-4*pi2*alpha3)*TMath::Exp((2*pi)/TMath::Abs(alpha))*TMath::Abs(alpha)*sigma*w2+((((-2*pi**4)+12*pi**3-24*pi2+16*pi)*alpha**8+((-8*pi**4)+36*pi**3-48*pi2+16*pi)*alpha**6+((-12*pi**4)+36*pi**3-24*pi2)*alpha**4+(12*pi**3-8*pi**4)*alpha2-2*pi**4)*TMath::Abs(alpha)+((-2*pi2)+8*pi-8)*alpha**8+(8*pi-4*pi2)*alpha**6-2*pi2*alpha**4)*ksi*sigma*w+((4*pi2-16*pi+16)*alpha**7+(8*pi2-16*pi)*alpha5+4*pi2*alpha3)*TMath::Exp((2*pi)/TMath::Abs(alpha))*TMath::Abs(alpha)*ksi2*sigma)*TMath::Exp(akw2))*TMath::Exp(aXw2+(ksi*X)/w2))+((((-2**(5./2.)*pi2)+2**(9./2.)*pi-2**(9./2.))*alpha**8+(2**(9./2.)*pi-2**(7./2.)*pi2)*alpha**6-2**(5./2.)*pi2*alpha**4)*TMath::Exp((2*pi)/TMath::Abs(alpha))*TMath::Abs(alpha)*sigma*w*X+((((-2**(3./2.)*pi**4)+3*2**(5./2.)*pi**3-3*2**(7./2.)*pi2+2**(9./2.)*pi)*alpha**9+((-2**(7./2.)*pi**4)+9*2**(5./2.)*pi**3-3*2**(9./2.)*pi2+2**(9./2.)*pi)*alpha**7+((-3*2**(5./2.)*pi**4)+9*2**(5./2.)*pi**3-3*2**(7./2.)*pi2)*alpha5+(3*2**(5./2.)*pi**3-2**(7./2.)*pi**4)*alpha3-2**(3./2.)*pi**4*alpha)*TMath::Abs(alpha)+((-2**(3./2.)*pi2)+2**(7./2.)*pi-2**(7./2.))*alpha**9+(2**(7./2.)*pi-2**(5./2.)*pi2)*alpha**7-2**(3./2.)*pi2*alpha5)*sigma*w2+((2**(5./2.)*pi2-2**(9./2.)*pi+2**(9./2.))*alpha**8+(2**(7./2.)*pi2-2**(9./2.)*pi)*alpha**6+2**(5./2.)*pi2*alpha**4)*TMath::Exp((2*pi)/TMath::Abs(alpha))*TMath::Abs(alpha)*ksi*sigma*w)*TMath::Exp((alpha2*ksi*X)/w2+(ksi*X)/w2))+((((-5*TMath::Sqrt(2)*pi**3)+2**(9./2.)*pi2+2**(7./2.)*pi)*alpha**8+((-3*2**(5./2.)*pi**3)+9*2**(5./2.)*pi2+2**(7./2.)*pi)*alpha**6+(5*2**(5./2.)*pi2-9*TMath::Sqrt(2)*pi**3)*alpha**4-2**(3./2.)*pi**3*alpha2)*TMath::Abs(alpha)*sigma*w*TMath::Exp(akw2+(2*pi)/TMath::Abs(alpha))*X+((5*TMath::Sqrt(2)*pi**3-2**(9./2.)*pi2-2**(7./2.)*pi)*alpha**8+(3*2**(5./2.)*pi**3-9*2**(5./2.)*pi2-2**(7./2.)*pi)*alpha**6+(9*TMath::Sqrt(2)*pi**3-5*2**(5./2.)*pi2)*alpha**4+2**(3./2.)*pi**3*alpha2)*TMath::Abs(alpha)*ksi*sigma*w*TMath::Exp(akw2+(2*pi)/TMath::Abs(alpha)))*TMath::Exp(aXw2+(ksi*X)/w2)*erf((alpha*v))+((((-5*TMath::Sqrt(2)*pi**3)+2**(9./2.)*pi2+2**(7./2.)*pi)*alpha**8+((-3*2**(5./2.)*pi**3)+9*2**(5./2.)*pi2+2**(7./2.)*pi)*alpha**6+(5*2**(5./2.)*pi2-9*TMath::Sqrt(2)*pi**3)*alpha**4-2**(3./2.)*pi**3*alpha2)*TMath::Abs(alpha)*sigma*w*TMath::Exp(akw2+(2*pi)/TMath::Abs(alpha))*X+((5*TMath::Sqrt(2)*pi**3-2**(9./2.)*pi2-2**(7./2.)*pi)*alpha**8+(3*2**(5./2.)*pi**3-9*2**(5./2.)*pi2-2**(7./2.)*pi)*alpha**6+(9*TMath::Sqrt(2)*pi**3-5*2**(5./2.)*pi2)*alpha**4+2**(3./2.)*pi**3*alpha2)*TMath::Abs(alpha)*ksi*sigma*w*TMath::Exp(akw2+(2*pi)/TMath::Abs(alpha)))*TMath::Exp(aXw2+(ksi*X)/w2)+TMath::Sqrt(pi)*((10*pi2-32*pi-16)*alpha**9+(24*pi2-72*pi-16)*alpha**7+(18*pi2-40*pi)*alpha5+4*pi2*alpha3)*TMath::Abs(alpha)*sigma*w2*TMath::Exp((alpha2*ksi*X)/w2+(ksi*X)/w2+(2*pi)/TMath::Abs(alpha)))+TMath::Sqrt(alpha2+1)*TMath::Sqrt((pi-2)*alpha2+pi)*(((2**(3./2.)*pi**3-3*2**(5./2.)*pi2+3*2**(7./2.)*pi-2**(9./2.))*alpha**10+(2**(7./2.)*pi**3-9*2**(5./2.)*pi2+3*2**(9./2.)*pi-2**(9./2.))*alpha**8+(3*2**(5./2.)*pi**3-9*2**(5./2.)*pi2+3*2**(7./2.)*pi)*alpha**6+(2**(7./2.)*pi**3-3*2**(5./2.)*pi2)*alpha**4+2**(3./2.)*pi**3*alpha2)*TMath::Exp((2*pi)/TMath::Abs(alpha))*TMath::Abs(alpha)*w2*X+(((-2**(3./2.)*pi**3)+3*2**(5./2.)*pi2-3*2**(7./2.)*pi+2**(9./2.))*alpha**10+((-2**(7./2.)*pi**3)+9*2**(5./2.)*pi2-3*2**(9./2.)*pi+2**(9./2.))*alpha**8+((-3*2**(5./2.)*pi**3)+9*2**(5./2.)*pi2-3*2**(7./2.)*pi)*alpha**6+(3*2**(5./2.)*pi2-2**(7./2.)*pi**`53)*alpha**4-2**(3./2.)*pi**3*alpha2)*TMath::Exp((2*pi)/TMath::Abs(alpha))*TMath::Abs(alpha)*ksi*w2)*TMath::Exp((alpha2*ksi*X)/w2+(ksi*X)/w2)))/(TMath::Sqrt(alpha2+1)*TMath::Sqrt((pi-2)*alpha2+pi)*((2**(3./2.)*pi**4-3*2**(5./2.)*pi**3+3*2**(7./2.)*pi2-2**(9./2.)*pi)*alpha**10+(2**(7./2.)*pi**4-9*2**(5./2.)*pi**3+3*2**(9./2.)*pi2-2**(9./2.)*pi)*alpha**8+(3*2**(5./2.)*pi**4-9*2**(5./2.)*pi**3+3*2**(7./2.)*pi2)*alpha**6+(2**(7./2.)*pi**4-3*2**(5./2.)*pi**3)*alpha**4+2**(3./2.)*pi**4*alpha2)*TMath::Abs(alpha)*w**4)


A.txt
A(1,1) = -pi32*TMath::Exp((-((alpha2*X2)/w2)/2.)-(X2/w2)/2.-((alpha2*ksi2)/w2)/2.-(ksi2/w2)/2.)*((pi*ksi*TMath::Exp(((alpha2*ksi2)/w2)/2.)-pi*TMath::Exp(((alpha2*ksi2)/w2)/2.)*X)*TMath::Exp(((alpha2*X2)/w2)/2.+(ksi*X)/w2)*erf((alpha*X-alpha*ksi)/(TMath::Sqrt(2)*w))+(pi*ksi*TMath::Exp(((alpha2*ksi2)/w2)/2.)-pi*TMath::Exp(((alpha2*ksi2)/w2)/2.)*X)*TMath::Exp(((alpha2*X2)/w2)/2.+(ksi*X)/w2)+TMath::Sqrt(2)*TMath::Sqrt(pi)*alpha*w*TMath::Exp((alpha2*ksi*X)/w2+(ksi*X)/w2)))/(TMath::Sqrt(2)*w**3)

A(1,2) = -(TMath::Sqrt((pi-2)*alpha2+pi)*TMath::Sqrt(pi*alpha2+pi)*TMath::Exp((-((alpha2*X2)/w2)/2.)-(X2/w2)/2.-((alpha2*ksi2)/w2)/2.-(ksi2/w2)/2.-(2*pi)/TMath::Abs(alpha))*(TMath::Sqrt(alpha2+1)*(TMath::Sqrt(pi)*((((6*pi2-24*pi+16)*alpha5+(10*pi2-24*pi)*alpha3+4*pi2*alpha)*TMath::Abs(alpha)*w*TMath::Exp(((alpha2*ksi2)/w2)/2.+(2*pi)/TMath::Abs(alpha))*X+(((-6*pi2)+24*pi-16)*alpha5+(24*pi-10*pi2)*alpha3-4*pi2*alpha)*TMath::Abs(alpha)*ksi*w*TMath::Exp(((alpha2*ksi2)/w2)/2.+(2*pi)/TMath::Abs(alpha)))*TMath::Exp(((alpha2*X2)/w2)/2.+(ksi*X)/w2)*erf((alpha*X-alpha*ksi)/(TMath::Sqrt(2)*w))+(((6*pi2-24*pi+16)*alpha5+(10*pi2-24*pi)*alpha3+4*pi2*alpha)*TMath::Abs(alpha)*w*TMath::Exp(((alpha2*ksi2)/w2)/2.+(2*pi)/TMath::Abs(alpha))*X+(((-6*pi2)+24*pi-16)*alpha5+(24*pi-10*pi2)*alpha3-4*pi2*alpha)*TMath::Abs(alpha)*ksi*w*TMath::Exp(((alpha2*ksi2)/w2)/2.+(2*pi)/TMath::Abs(alpha)))*TMath::Exp(((alpha2*X2)/w2)/2.+(ksi*X)/w2))+(((-3*2**(3./2.)*pi2)+3*2**(7./2.)*pi-2**(9./2.))*alpha**6+(3*2**(7./2.)*pi-5*2**(3./2.)*pi2)*alpha**4-2**(5./2.)*pi2*alpha2)*TMath::Abs(alpha)*w2*TMath::Exp((alpha2*ksi*X)/w2+(ksi*X)/w2+(2*pi)/TMath::Abs(alpha)))+((((-2**(3./2.)*pi**3)+2**(7./2.)*pi2-2**(7./2.)*pi)*alpha**6+((-3*2**(3./2.)*pi**3)+2**(9./2.)*pi2-2**(7./2.)*pi)*alpha**4+(2**(7./2.)*pi2-3*2**(3./2.)*pi**3)*alpha2-2**(3./2.)*pi**3)*TMath::Abs(alpha)*TMath::Exp(((alpha2*ksi2)/w2)/2.+(2*pi)/TMath::Abs(alpha))*X2+((((-TMath::Sqrt(2)*pi**3)+2**(5./2.)*pi2-2**(5./2.)*pi)*alpha**7+((-3*TMath::Sqrt(2)*pi**3)+2**(7./2.)*pi2-2**(5./2.)*pi)*alpha5+(2**(5./2.)*pi2-3*TMath::Sqrt(2)*pi**3)*alpha3-TMath::Sqrt(2)*pi**3*alpha)*w+((2**(5./2.)*pi**3-2**(9./2.)*pi2+2**(9./2.)*pi)*alpha**6+(3*2**(5./2.)*pi**3-2**(1.1000000000000001d+1/2.)*pi2+2**(9./2.)*pi)*alpha**4+(3*2**(5./2.)*pi**3-2**(9./2.)*pi2)*alpha2+2**(5./2.)*pi**3)*TMath::Exp((2*pi)/TMath::Abs(alpha))*TMath::Abs(alpha)*ksi)*TMath::Exp(((alpha2*ksi2)/w2)/2.)*X+(((2**(3./2.)*pi**3-2**(7./2.)*pi2+2**(7./2.)*pi)*alpha**6+(3*2**(3./2.)*pi**3-2**(9./2.)*pi2+2**(7./2.)*pi)*alpha**4+(3*2**(3./2.)*pi**3-2**(7./2.)*pi2)*alpha2+2**(3./2.)*pi**3)*TMath::Exp((2*pi)/TMath::Abs(alpha))*TMath::Abs(alpha)*w2+((TMath::Sqrt(2)*pi**3-2**(5./2.)*pi2+2**(5./2.)*pi)*alpha**7+(3*TMath::Sqrt(2)*pi**3-2**(7./2.)*pi2+2**(5./2.)*pi)*alpha5+(3*TMath::Sqrt(2)*pi**3-2**(5./2.)*pi2)*alpha3+TMath::Sqrt(2)*pi**3*alpha)*ksi*w+(((-2**(3./2.)*pi**3)+2**(7./2.)*pi2-2**(7./2.)*pi)*alpha**6+((-3*2**(3./2.)*pi**3)+2**(9./2.)*pi2-2**(7./2.)*pi)*alpha**4+(2**(7./2.)*pi2-3*2**(3./2.)*pi**3)*alpha2-2**(3./2.)*pi**3)*TMath::Exp((2*pi)/TMath::Abs(alpha))*TMath::Abs(alpha)*ksi2)*TMath::Exp(((alpha2*ksi2)/w2)/2.))*TMath::Exp(((alpha2*X2)/w2)/2.+(ksi*X)/w2)*erf((alpha*X-alpha*ksi)/(TMath::Sqrt(2)*w))+((((-2**(3./2.)*pi**3)+2**(7./2.)*pi2-2**(7./2.)*pi)*alpha**6+((-3*2**(3./2.)*pi**3)+2**(9./2.)*pi2-2**(7./2.)*pi)*alpha**4+(2**(7./2.)*pi2-3*2**(3./2.)*pi**3)*alpha2-2**(3./2.)*pi**3)*TMath::Abs(alpha)*TMath::Exp(((alpha2*ksi2)/w2)/2.+(2*pi)/TMath::Abs(alpha))*X2+((((-TMath::Sqrt(2)*pi**3)+2**(5./2.)*pi2-2**(5./2.)*pi)*alpha**7+((-3*TMath::Sqrt(2)*pi**3)+2**(7./2.)*pi2-2**(5./2.)*pi)*alpha5+(2**(5./2.)*pi2-3*TMath::Sqrt(2)*pi**3)*alpha3-TMath::Sqrt(2)*pi**3*alpha)*w+((2**(5./2.)*pi**3-2**(9./2.)*pi2+2**(9./2.)*pi)*alpha**6+(3*2**(5./2.)*pi**3-2**(1.1000000000000001d+1/2.)*pi2+2**(9./2.)*pi)*alpha**4+(3*2**(5./2.)*pi**3-2**(9./2.)*pi2)*alpha2+2**(5./2.)*pi**3)*TMath::Exp((2*pi)/TMath::Abs(alpha))*TMath::Abs(alpha)*ksi)*TMath::Exp(((alpha2*ksi2)/w2)/2.)*X+(((2**(3./2.)*pi**3-2**(7./2.)*pi2+2**(7./2.)*pi)*alpha**6+(3*2**(3./2.)*pi**3-2**(9./2.)*pi2+2**(7./2.)*pi)*alpha**4+(3*2**(3./2.)*pi**3-2**(7./2.)*pi2)*alpha2+2**(3./2.)*pi**3)*TMath::Exp((2*pi)/TMath::Abs(alpha))*TMath::Abs(alpha)*w2+((TMath::Sqrt(2)*pi**3-2**(5./2.)*pi2+2**(5./2.)*pi)*alpha**7+(3*TMath::Sqrt(2)*pi**3-2**(7./2.)*pi2+2**(5./2.)*pi)*alpha5+(3*TMath::Sqrt(2)*pi**3-2**(5./2.)*pi2)*alpha3+TMath::Sqrt(2)*pi**3*alpha)*ksi*w+(((-2**(3./2.)*pi**3)+2**(7./2.)*pi2-2**(7./2.)*pi)*alpha**6+((-3*2**(3./2.)*pi**3)+2**(9./2.)*pi2-2**(7./2.)*pi)*alpha**4+(2**(7./2.)*pi2-3*2**(3./2.)*pi**3)*alpha2-2**(3./2.)*pi**3)*TMath::Exp((2*pi)/TMath::Abs(alpha))*TMath::Abs(alpha)*ksi2)*TMath::Exp(((alpha2*ksi2)/w2)/2.))*TMath::Exp(((alpha2*X2)/w2)/2.+(ksi*X)/w2)+TMath::Sqrt(pi)*(((4*pi2-16*pi+16)*alpha**7+(12*pi2-32*pi+16)*alpha5+(12*pi2-16*pi)*alpha3+4*pi2*alpha)*TMath::Exp((2*pi)/TMath::Abs(alpha))*TMath::Abs(alpha)*w*X+((2*pi2-8*pi+8)*alpha**8+(6*pi2-16*pi+8)*alpha**6+(6*pi2-8*pi)*alpha**4+2*pi2*alpha2)*w2+(((-4*pi2)+16*pi-16)*alpha**7+((-12*pi2)+32*pi-16)*alpha5+(16*pi-12*pi2)*alpha3-4*pi2*alpha)*TMath::Exp((2*pi)/TMath::Abs(alpha))*TMath::Abs(alpha)*ksi*w)*TMath::Exp((alpha2*ksi*X)/w2+(ksi*X)/w2)))/(TMath::Sqrt(pi)*((4*pi**4-24*pi**3+48*pi2-32*pi)*alpha**8+(16*pi**4-72*pi**3+96*pi2-32*pi)*alpha**6+(24*pi**4-72*pi**3+48*pi2)*alpha**4+(16*pi**4-24*pi**3)*alpha2+4*pi**4)*TMath::Abs(alpha)*w**4)

A(1,3) = (TMath::Exp((-((alpha2*X2)/w2)/2.)-(X2/w2)/2.-((alpha2*ksi2)/w2)/2.-(ksi2/w2)/2.-(2*pi)/TMath::Abs(alpha))*(TMath::Sqrt(pi*alpha2+pi)*(TMath::Sqrt(alpha2+1)*(TMath::Sqrt(pi)*((((4*pi2-16*pi+16)*alpha**7+(8*pi2-16*pi)*alpha5+4*pi2*alpha3)*TMath::Abs(alpha)*sigma*TMath::Exp(((alpha2*ksi2)/w2)/2.+(2*pi)/TMath::Abs(alpha))*X2+((((2*pi**4-12*pi**3+24*pi2-16*pi)*alpha**8+(8*pi**4-36*pi**3+48*pi2-16*pi)*alpha**6+(12*pi**4-36*pi**3+24*pi2)*alpha**4+(8*pi**4-12*pi**3)*alpha2+2*pi**4)*TMath::Abs(alpha)+(2*pi2-8*pi+8)*alpha**8+(4*pi2-8*pi)*alpha**6+2*pi2*alpha**4)*sigma*w+(((-8*pi2)+32*pi-32)*alpha**7+(32*pi-16*pi2)*alpha5-8*pi2*alpha3)*TMath::Exp((2*pi)/TMath::Abs(alpha))*TMath::Abs(alpha)*ksi*sigma)*TMath::Exp(((alpha2*ksi2)/w2)/2.)*X+((((-4*pi2)+16*pi-16)*alpha**7+(16*pi-8*pi2)*alpha5-4*pi2*alpha3)*TMath::Exp((2*pi)/TMath::Abs(alpha))*TMath::Abs(alpha)*sigma*w2+((((-2*pi**4)+12*pi**3-24*pi2+16*pi)*alpha**8+((-8*pi**4)+36*pi**3-48*pi2+16*pi)*alpha**6+((-12*pi**4)+36*pi**3-24*pi2)*alpha**4+(12*pi**3-8*pi**4)*alpha2-2*pi**4)*TMath::Abs(alpha)+((-2*pi2)+8*pi-8)*alpha**8+(8*pi-4*pi2)*alpha**6-2*pi2*alpha**4)*ksi*sigma*w+((4*pi2-16*pi+16)*alpha**7+(8*pi2-16*pi)*alpha5+4*pi2*alpha3)*TMath::Exp((2*pi)/TMath::Abs(alpha))*TMath::Abs(alpha)*ksi2*sigma)*TMath::Exp(((alpha2*ksi2)/w2)/2.))*TMath::Exp(((alpha2*X2)/w2)/2.+(ksi*X)/w2)*erf((alpha*X-alpha*ksi)/(TMath::Sqrt(2)*w))+(((4*pi2-16*pi+16)*alpha**7+(8*pi2-16*pi)*alpha5+4*pi2*alpha3)*TMath::Abs(alpha)*sigma*TMath::Exp(((alpha2*ksi2)/w2)/2.+(2*pi)/TMath::Abs(alpha))*X2+((((2*pi**4-12*pi**3+24*pi2-16*pi)*alpha**8+(8*pi**4-36*pi**3+48*pi2-16*pi)*alpha**6+(12*pi**4-36*pi**3+24*pi2)*alpha**4+(8*pi**4-12*pi**3)*alpha2+2*pi**4)*TMath::Abs(alpha)+(2*pi2-8*pi+8)*alpha**8+(4*pi2-8*pi)*alpha**6+2*pi2*alpha**4)*sigma*w+(((-8*pi2)+32*pi-32)*alpha**7+(32*pi-16*pi2)*alpha5-8*pi2*alpha3)*TMath::Exp((2*pi)/TMath::Abs(alpha))*TMath::Abs(alpha)*ksi*sigma)*TMath::Exp(((alpha2*ksi2)/w2)/2.)*X+((((-4*pi2)+16*pi-16)*alpha**7+(16*pi-8*pi2)*alpha5-4*pi2*alpha3)*TMath::Exp((2*pi)/TMath::Abs(alpha))*TMath::Abs(alpha)*sigma*w2+((((-2*pi**4)+12*pi**3-24*pi2+16*pi)*alpha**8+((-8*pi**4)+36*pi**3-48*pi2+16*pi)*alpha**6+((-12*pi**4)+36*pi**3-24*pi2)*alpha**4+(12*pi**3-8*pi**4)*alpha2-2*pi**4)*TMath::Abs(alpha)+((-2*pi2)+8*pi-8)*alpha**8+(8*pi-4*pi2)*alpha**6-2*pi2*alpha**4)*ksi*sigma*w+((4*pi2-16*pi+16)*alpha**7+(8*pi2-16*pi)*alpha5+4*pi2*alpha3)*TMath::Exp((2*pi)/TMath::Abs(alpha))*TMath::Abs(alpha)*ksi2*sigma)*TMath::Exp(((alpha2*ksi2)/w2)/2.))*TMath::Exp(((alpha2*X2)/w2)/2.+(ksi*X)/w2))+((((-2**(5./2.)*pi2)+2**(9./2.)*pi-2**(9./2.))*alpha**8+(2**(9./2.)*pi-2**(7./2.)*pi2)*alpha**6-2**(5./2.)*pi2*alpha**4)*TMath::Exp((2*pi)/TMath::Abs(alpha))*TMath::Abs(alpha)*sigma*w*X+((((-2**(3./2.)*pi**4)+3*2**(5./2.)*pi**3-3*2**(7./2.)*pi2+2**(9./2.)*pi)*alpha**9+((-2**(7./2.)*pi**4)+9*2**(5./2.)*pi**3-3*2**(9./2.)*pi2+2**(9./2.)*pi)*alpha**7+((-3*2**(5./2.)*pi**4)+9*2**(5./2.)*pi**3-3*2**(7./2.)*pi2)*alpha5+(3*2**(5./2.)*pi**3-2**(7./2.)*pi**4)*alpha3-2**(3./2.)*pi**4*alpha)*TMath::Abs(alpha)+((-2**(3./2.)*pi2)+2**(7./2.)*pi-2**(7./2.))*alpha**9+(2**(7./2.)*pi-2**(5./2.)*pi2)*alpha**7-2**(3./2.)*pi2*alpha5)*sigma*w2+((2**(5./2.)*pi2-2**(9./2.)*pi+2**(9./2.))*alpha**8+(2**(7./2.)*pi2-2**(9./2.)*pi)*alpha**6+2**(5./2.)*pi2*alpha**4)*TMath::Exp((2*pi)/TMath::Abs(alpha))*TMath::Abs(alpha)*ksi*sigma*w)*TMath::Exp((alpha2*ksi*X)/w2+(ksi*X)/w2))+((((-5*TMath::Sqrt(2)*pi**3)+2**(9./2.)*pi2+2**(7./2.)*pi)*alpha**8+((-3*2**(5./2.)*pi**3)+9*2**(5./2.)*pi2+2**(7./2.)*pi)*alpha**6+(5*2**(5./2.)*pi2-9*TMath::Sqrt(2)*pi**3)*alpha**4-2**(3./2.)*pi**3*alpha2)*TMath::Abs(alpha)*sigma*w*TMath::Exp(((alpha2*ksi2)/w2)/2.+(2*pi)/TMath::Abs(alpha))*X+((5*TMath::Sqrt(2)*pi**3-2**(9./2.)*pi2-2**(7./2.)*pi)*alpha**8+(3*2**(5./2.)*pi**3-9*2**(5./2.)*pi2-2**(7./2.)*pi)*alpha**6+(9*TMath::Sqrt(2)*pi**3-5*2**(5./2.)*pi2)*alpha**4+2**(3./2.)*pi**3*alpha2)*TMath::Abs(alpha)*ksi*sigma*w*TMath::Exp(((alpha2*ksi2)/w2)/2.+(2*pi)/TMath::Abs(alpha)))*TMath::Exp(((alpha2*X2)/w2)/2.+(ksi*X)/w2)*erf((alpha*X-alpha*ksi)/(TMath::Sqrt(2)*w))+((((-5*TMath::Sqrt(2)*pi**3)+2**(9./2.)*pi2+2**(7./2.)*pi)*alpha**8+((-3*2**(5./2.)*pi**3)+9*2**(5./2.)*pi2+2**(7./2.)*pi)*alpha**6+(5*2**(5./2.)*pi2-9*TMath::Sqrt(2)*pi**3)*alpha**4-2**(3./2.)*pi**3*alpha2)*TMath::Abs(alpha)*sigma*w*TMath::Exp(((alpha2*ksi2)/w2)/2.+(2*pi)/TMath::Abs(alpha))*X+((5*TMath::Sqrt(2)*pi**3-2**(9./2.)*pi2-2**(7./2.)*pi)*alpha**8+(3*2**(5./2.)*pi**3-9*2**(5./2.)*pi2-2**(7./2.)*pi)*alpha**6+(9*TMath::Sqrt(2)*pi**3-5*2**(5./2.)*pi2)*alpha**4+2**(3./2.)*pi**3*alpha2)*TMath::Abs(alpha)*ksi*sigma*w*TMath::Exp(((alpha2*ksi2)/w2)/2.+(2*pi)/TMath::Abs(alpha)))*TMath::Exp(((alpha2*X2)/w2)/2.+(ksi*X)/w2)+TMath::Sqrt(pi)*((10*pi2-32*pi-16)*alpha**9+(24*pi2-72*pi-16)*alpha**7+(18*pi2-40*pi)*alpha5+4*pi2*alpha3)*TMath::Abs(alpha)*sigma*w2*TMath::Exp((alpha2*ksi*X)/w2+(ksi*X)/w2+(2*pi)/TMath::Abs(alpha)))+TMath::Sqrt(alpha2+1)*TMath::Sqrt((pi-2)*alpha2+pi)*(((2**(3./2.)*pi**3-3*2**(5./2.)*pi2+3*2**(7./2.)*pi-2**(9./2.))*alpha**10+(2**(7./2.)*pi**3-9*2**(5./2.)*pi2+3*2**(9./2.)*pi-2**(9./2.))*alpha**8+(3*2**(5./2.)*pi**3-9*2**(5./2.)*pi2+3*2**(7./2.)*pi)*alpha**6+(2**(7./2.)*pi**3-3*2**(5./2.)*pi2)*alpha**4+2**(3./2.)*pi**3*alpha2)*TMath::Exp((2*pi)/TMath::Abs(alpha))*TMath::Abs(alpha)*w2*X+(((-2**(3./2.)*pi**3)+3*2**(5./2.)*pi2-3*2**(7./2.)*pi+2**(9./2.))*alpha**10+((-2**(7./2.)*pi**3)+9*2**(5./2.)*pi2-3*2**(9./2.)*pi+2**(9./2.))*alpha**8+((-3*2**(5./2.)*pi**3)+9*2**(5./2.)*pi2-3*2**(7./2.)*pi)*alpha**6+(3*2**(5./2.)*pi2-2**(7./2.)*pi**3)*alpha**4-2**(3./2.)*pi**3*alpha2)*TMath::Exp((2*pi)/TMath::Abs(alpha))*TMath::Abs(alpha)*ksi*w2)*TMath::Exp((alpha2*ksi*X)/w2+(ksi*X)/w2)))/(TMath::Sqrt(alpha2+1)*TMath::Sqrt((pi-2)*alpha2+pi)*((2**(3./2.)*pi**4-3*2**(5./2.)*pi**3+3*2**(7./2.)*pi2-2**(9./2.)*pi)*alpha**10+(2**(7./2.)*pi**4-9*2**(5./2.)*pi**3+3*2**(9./2.)*pi2-2**(9./2.)*pi)*alpha**8+(3*2**(5./2.)*pi**4-9*2**(5./2.)*pi**3+3*2**(7./2.)*pi2)*alpha**6+(2**(7./2.)*pi**4-3*2**(5./2.)*pi**3)*alpha**4+2**(3./2.)*pi**4*alpha2)*TMath::Abs(alpha)*w**4)


#endif
