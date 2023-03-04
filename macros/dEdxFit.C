/* 
   root.exe -q -b lBichsel.C pionMIP.root 'dEdxFit.C+("SecRow3C","GF")'

# Fit Sparse : use ~/bin/SpareSubmit.csh to fit it in parallel  
@ count = 0
while  (1)
    root.exe -q -b AdcSparseD3.root lBichsel.C 'dEdxFit.C+("Sparse","GP","R",'${count}')' >& X${count}.log &
    @ count++;  echo "count $count";
    if ($count > 28) then 
        break;
    endif
  endif
end

*/
#if !defined(__CINT__) && !defined(__CLING__) && ! defined(__MAKECINT__)
// code that should be seen ONLY by the compiler
#else
#if !defined(__CINT__) && !defined(__CLING__) || defined(__MAKECINT__)
// code that should be seen by the compiler AND rootcint
#else
// code that should always be seen
#endif
#endif
//    ROOT5
#if defined(__CINT__) && !defined(__MAKECINT__)
// source code being actually interpreted by cint
#elif defined(__MAKECINT__)
// source code seen by rootcint only
#elif defined(__ACLIC__)
// source code being actually compiled by ACLiC
#else
// source code suitable for a standalone executable
#endif
//    ROOT6 
#if defined(__CLING__) && !defined(__ROOTCLING__)
// source code being actually interpreted by Cling
#elif defined(__ROOTCLING__)
// source code seen by rootcling only
#elif defined(__ACLIC__)
// source code being actually compiled by ACLiC
#else
// source code suitable for a standalone executable
#endif
//________________________________________________________________________________
#if !defined(__CINT__) || defined(__MAKECINT__)
//#include <map>
//#include <array>
#include "Riostream.h"
#include <stdio.h>
#include "TROOT.h"
#include "TSystem.h"
#include "TMath.h"
#include "Math/ProbFuncMathCore.h"
#include "TH1.h"
#include "TH2.h"
#include "TH3.h"
#include "THnSparse.h"
#include "THnSparseProject.h"
#include "TStyle.h"
#include "TF1.h"
#include "TProfile.h"
#include "TTree.h"
#include "TChain.h"
#include "TFile.h"
#include "TNtuple.h"
//#include "TFitResult.h"
#include "TCanvas.h"
#include "TFileSet.h"
#include "TDataSetIter.h"
#include "TDataSet.h"
#include "TClassTable.h"
#include "TMinuit.h"
#include "TSpectrum.h"
#include "StBichsel/Bichsel.h"
#include "StBichsel/StdEdxModel.h"
#include "TString.h"
#include "TLine.h"
#include "TText.h"
#include "TList.h"
#include "THashList.h"
#include "TPolyMarker.h"
#include "TKey.h"
#include "TLegend.h"
#ifdef __USE_ROOFIT__
#include "RooRealVar.h"
#include "RooDataSet.h"
#include "RooGaussian.h"
#include "RooFFTConvPdf.h"
#include "RooPlot.h"
#include "RooCFunction1Binding.h" 
#include "RooCFunction3Binding.h"
#include "RooTFnBinding.h" 
#include "RooDataHist.h"
#include "RooAbsPdf.h"
#include "RooRealProxy.h"
#include "RooFit.h"
#include "RooRandom.h"
#include "RooFitResult.h"
#include "RooWorkspace.h"
#include "TArrayI.h"
#include "TArrayF.h"
#include "TArrayD.h"
using namespace RooFit ;
#endif /* __USE_ROOFIT__ */
#include "Ask.h"
#include "TObjectTable.h"
#else
class TMinuit;
class TF1;
class TH1F;
class TH2F;
class TH3F;
class TProfile;
class TH2D;
class TCanvas;
class TSpectrum;
class TSystem;
class Bichsel;
// Refer to a class implemented in libRooFit to force its loading
// via the autoloader.
#ifdef __USE_ROOFIT__
class Roo2DKeysPdf;
#endif /* __USE_ROOFIT__ */
#endif
#include "Names.h"
using namespace std;
const Int_t NH = NHYPS/2;
Int_t N = 0;
const Int_t noPoints = 1000;
Double_t X[noPoints];
Double_t dX[noPoints];
Double_t Nu[noPoints];
Double_t Mu[noPoints];
Double_t dMu[noPoints];
Double_t Sigma[noPoints];
Double_t dSigma[noPoints];
TCanvas *canvas = 0;
Double_t Xlog10bg, Ylog2dx, Z;
static TFile *newf = 0;
static TFile *fOut = 0;
static TNtuple *FitP = 0;
static TH1 *projNs[5];
// BDiif.C 
// peak postion at p = 0.475 GeV/c wrt pion
//                        Z     pion
/// <peak postion> at p = [0.45,0.50] GeV/c wrt pion
struct peak_t {Double_t peak, sigma, mass; const Char_t *Name; Int_t N; Float_t X[8], Y[8];};
struct MIPFitPar_t {
  const Char_t *Name;
  Double_t     normL;
  Double_t        mu;
  Double_t     sigma;
  Double_t     alpha;
  void Print(Option_t *opt="", Double_t *pars = 0) {
    cout << Form("%3i %3i %25s mu: %8.3f sigma:%8.3f alpha:%8.3f",Name,mu,sigma,alpha);
    if (pars) cout << Form("\t mu: %8.3f sigma:%8.3f alpha:%8.3f",pars[1], pars[2], pars[3]);
    cout << endl;
  }
};
struct MIPFitParX_t {
  Int_t           p1;
  Int_t           p2;
  Int_t            i;
  Int_t            j;
  const Char_t *Name;
  Double_t     normL;
  Double_t        mu;
  Double_t     sigma;
  Double_t     alpha;
  void Print(Option_t *opt="", Double_t *pars = 0) {
    cout << Form("%3i %3i %25s mu: %8.3f sigma:%8.3f alpha:%8.3f",p1,p2,Name,mu,sigma,alpha);
    if (pars) cout << Form("\t mu: %8.3f sigma:%8.3f alpha:%8.3f",pars[1], pars[2], pars[3]);
    cout << endl;
  }
};
struct MIPFitParG_t {// extremevalueG/ ExValG
  Int_t           p1;
  Int_t           p2;
  Int_t            i;
  Int_t            j;
  const Char_t *Name;
  Double_t     normL;
  Double_t        mu;
  Double_t    sigmaI;
  Double_t     phase;
  Double_t    sigmaG;
  void Print(Option_t *opt="", Double_t *pars = 0) {
    cout << Form("%3i %3i %25s mu: %8.3f sigmaI:%8.3f phase:%8.3f sigmaG:%8.3f",p1,p2,Name,mu,sigmaI,phase,sigmaG);
    if (pars) cout << Form("\t mu: %8.3f sigmaI:%8.3f phase:%8.3f sigmaG:%8.3f",pars[1], pars[2], pars[3],pars[4]);
    cout << endl;
  }
};
struct Fitx_t {
  Float_t i;
  Float_t j;
  Float_t x;
  Float_t y;
  Float_t mean;
  Float_t rms;
  Float_t peak;
  Float_t dpeak;
  Float_t entries;
  Float_t chisq;
  Float_t prob;
  Float_t Npar;
  Float_t muJ;
  Float_t dmuJ;
  Float_t NormL; // par[0]
  Float_t mu;    // par[1]
  Float_t sigma;
  Float_t p3; // a0;    // P
  Float_t p4; // a1;    // K
  Float_t p5; // pa2;   // e
  Float_t p6; // a3;    // d
  Float_t p7; //           muon
  Float_t p8; //           triton
  Float_t p9; //           He3
  Float_t p10;//           alpha
  Float_t p11;// a4;    // Total
  Float_t p12;// a5;    // Case
  Float_t p13;// a6;    // occupancy
  Float_t p14;// a7;    // IO
  Float_t p15;// a8;    // sign
  Float_t p16;// a9;    // recombin 
  Float_t p17;// a10;   //  par[13]; 
  Float_t dNormL; // dpar[0]
  Float_t dmu;    // dpar[1]
  Float_t dsigma;
  Float_t dp3; // a0;    // P
  Float_t dp4; // a1;    // K
  Float_t dp5; // pa2;   // e
  Float_t dp6; // a3;    // d
  Float_t dp7; //           muon
  Float_t dp8; //           triton
  Float_t dp9; //           He3
  Float_t dp10;//           alpha
  Float_t dp11;// a4;    // Total
  Float_t dp12;// a5;    // Case
  Float_t dp13;// a6;    // occupancy
  Float_t dp14;// a7;    // IO
  Float_t dp15;// a8;    // sign
  Float_t dp16;// a9;    // recombin 
  Float_t dp17;// a10;   //  par[13]; 
};
const Char_t *Fitx_VarList = "i:j:x:y:mean:rms:peak:dpeak:entries:chisq:prob:Npar:muJ:dmuJ:NormL:mu:sigma:p3:p4:p5:p6:p7:p8:p9:p10:p11:p12:p13:p14:p15:p16:p17:dNormL:dmu:dsigma:dp3:dp4:dp5:dp6:dp7:dp8:dp9:dp10:dp11:dp12:dp13:dp14:dp15:dp16:dp17";
//const Char_t *Fit_VarList = "i:j:x:y:mean:rms:peak:mu:sigma:entries:chisq:prob:a0:a1:a2:a3:a4:a5:a6:Npar:dpeak:dmu:dsigma:da0:da1:da2:da3:da4:da5:da6:muJ:dmuJ";
#ifndef __CINT__
static const  peak_t Peaks[6] = {
#if 0
  // 06/25/10
  //   {0.      ,       0., 0.13956995, "pion"}, // pion
  //   {1.425822, 0.101693, 0.93827231, "proton"}, // proton - pion
  //   {0.565455, 0.061626, 0.493677,   "kaon"  }, // Kaon   - pi
  //   {0.424916, 0.004081, 0.51099907e-3,"e"}, // e      - pi
  //   {2.655586, 0.123754, 1.875613,   "d"}, // d      - pi
  //   {0.004178, 0.002484, 0.105658,   "mu"}};// mu     - pi
  // 10/20/19 <peak postion> at p = [0.35,0.75] GeV/c wrt pion
  // <peak position> at p = [0.35,0.75] GeV/c wrt pion : dN/dP = gaus(mean = 1.75843e-01; sigma = 2.21705e-01)
  {            0.      ,                   0., 0.13956995,         "pion", 
	       0, {0}, {0}},
  {       1.39305,        0.284445,       0.938272,       "proton",
	  7,// protonNzB
	  { 0.7000, 0.9000, 1.1000, 1.3000, 1.5000, 1.7000, 1.9000}, // X
	  { 0.0251, 0.0902, 0.1477, 0.2007, 0.2416, 0.2669, 0.0278}  // Y
  },
  {       0.557112,       0.174024,       0.493677,       "kaon",
	  0, // 5,// kaonNzB
	  { 0.1000, 0.3000, 0.5000, 0.7000, 0.9000}, // X
	  { 0.0177, 0.1928, 0.3611, 0.3618, 0.0666}  // Y
  },
  {       0.421061,       0.0096963,      0.000510999,    "e",
	  0, // 3,// eNzB
	  { 0.3900, 0.4100, 0.4300}, // X
	  { 0.0262, 0.3455, 0.6283}  // Y
  },
  {       2.60851,        0.347455,       1.87561,        "deuteron",
	  0, // 8,// deuteronNzB
	{ 1.7000, 1.9000, 2.1000, 2.3000, 2.5000, 2.7000, 2.9000, 3.1000}, // X
	  { 0.0142, 0.0499, 0.0854, 0.1259, 0.1671, 0.1893, 0.2353, 0.1330}  // Y
  },
  {       0.000482817,    0.0128277,      0.105658,       "mu",
	  0, // 4,// muNzB
	  {-0.0300,-0.0100, 0.0100, 0.0300}, // X
	  { 0.0452, 0.4258, 0.4626, 0.0663}  // Y
  }
#else
  // 06/27/10
  //   // MIP from Heed bg = 3.77 => p_pion = 0.526
  //   Double_t pMIP = 0.526;
  //   Double_t pmin = pMIP - 0.05; // 0.45;
  //   Double_t pmax = pMIP + 0.05; // 0.55;
  {            0.      ,                   0., 0.13956995,         "pion",   0, {0}, {0}},
  {       1.2806, 0.0798256,      0.938272,       "proton",
	  6,// protonNzB
	  { 1.1250, 1.1750, 1.2250, 1.2750, 1.3250, 1.3750}, // X
	  { 0.0787, 0.1227, 0.1469, 0.1846, 0.2178, 0.2493}  // Y
  },
  {       0.435379,       0.0452135,      0.493677,       "kaon",
	  4,// kaonNzB
	  { 0.3750, 0.4250, 0.4750, 0.5250}, // X
	  { 0.2536, 0.3698, 0.2734, 0.1032}  // Y
  },
  {       0.313036,       0.000501486,    0.000510999,    "e",   0, {0}, {0}},
  {       2.40653,        0.0859432,      1.87561,        "deuteron",
	  7,// deuteronNzB
	  { 2.2250, 2.2750, 2.3250, 2.3750, 2.4250, 2.4750, 2.5250}, // X
	  { 0.0520, 0.1026, 0.1192, 0.1522, 0.1892, 0.2273, 0.1575}  // Y
  },
  {       0.00820447,     0.00328347,     0.105658,       "mu",   0, {0}, {0}}
#endif
};
#endif
Bichsel *gBichsel = 0;
//#define PRINT 1
TF1 *func = 0;
TF1 *LandauF = 0; 
class St_TpcSecRowCor;
#ifdef __USE_ROOFIT__
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

class RooRealVar;

class RooLandauZ : public RooAbsPdf {
public:
  RooLandauZ() {} ;
  RooLandauZ(const char *name, const char *title, RooAbsReal& _x, RooAbsReal& _mean, RooAbsReal& _sigma);
  RooLandauZ(const RooLandauZ& other, const char* name=0);
  virtual TObject* clone(const char* newname) const { return new RooLandauZ(*this,newname); }
  inline virtual ~RooLandauZ() { }

  Int_t getGenerator(const RooArgSet& directVars, RooArgSet &generateVars, Bool_t staticInitOK=kTRUE) const;
  void generateEvent(Int_t code);
  
protected:
  
  RooRealProxy x ;
  RooRealProxy mean ;
  RooRealProxy sigma ;
  
  Double_t evaluate() const ;
  
private:
  
  ClassDef(RooLandauZ,1) // LandauZ Distribution PDF
};

ClassImp(RooLandauZ)


//_____________________________________________________________________________
RooLandauZ::RooLandauZ(const char *name, const char *title, RooAbsReal& _x, RooAbsReal& _mean, RooAbsReal& _sigma) :
  RooAbsPdf(name,title),
  x("x","Dependent",this,_x),
  mean("mean","Mean",this,_mean),
  sigma("sigma","Width",this,_sigma)
{
}
 

//_____________________________________________________________________________
RooLandauZ::RooLandauZ(const RooLandauZ& other, const char* name) : 
  RooAbsPdf(other,name),
  x("x",this,other.x),
  mean("mean",this,other.mean),
  sigma("sigma",this,other.sigma)
{
} 


//_____________________________________________________________________________
Double_t RooLandauZ::evaluate() const
{
  //  return TMath::Landau(x, mean, sigma);
  const Double_t par[2] = {mean, sigma};
  Double_t xd = x;
  return landauZ(&xd,(Double_t *) par);
  //  return xx*TMath::Landau(xx,mpc,sigmad);
  //  return xx*TMath::Landau(xx,mpc,sigmad) / sigmad;

}


//_____________________________________________________________________________
Int_t RooLandauZ::getGenerator(const RooArgSet& directVars, RooArgSet &generateVars, Bool_t /*staticInitOK*/) const
{
  if (matchArgs(directVars,generateVars,x)) return 1 ;  
  return 0 ;
}


//_____________________________________________________________________________
void RooLandauZ::generateEvent(Int_t code)
{
  assert(code==1) ;
  Double_t xgen ;
  Double_t mpshift  = -0.22278298; // 1.0844535734 + 0.61417;       // Landau maximum location
  // MP shift correction
  Double_t meand = mean;
  Double_t sigmad = sigma;
  Double_t mpc = TMath::Exp(meand) - mpshift * sigmad; 
  while(1) {    
    xgen = RooRandom::randomGenerator()->Landau(mpc,sigma);
    if (xgen <= 0) continue;
    Double_t xgl = TMath::Log(xgen) ;
    if (xgl<x.max() && xgl>x.min()) {
      x = xgl;
      break;
    }
  }
  return;
}
//_______________________________________________________________________________
///.........................................................................//
//--------------------------B E G I N N I N G ------------------------------//
//------------------------L A N D A U   Z    5  function ------------------//
//---------------------------------------------------------------//
Double_t landauZ5(Double_t *x, Double_t *par) {
  // Double_t Norm = par[0];
  Double_t xd = x[0];
  Double_t meand_pi = par[1];
  Double_t meand_pr = par[1] + Peaks[1].peak;
  Double_t sigmad_pi = par[8];
  Double_t sigmad_pr = sigmad_pi;
  Double_t meand_k = par[1] + Peaks[2].peak;
  Double_t sigmad_k = sigmad_pi;
  Double_t meand_el = par[1] + Peaks[3].peak;
  Double_t sigmad_el = sigmad_pi;
  Double_t meand_d = par[1] + Peaks[4].peak;
  Double_t sigmad_d = sigmad_pi;
  Double_t mpshift  = -0.22278298; // 1.0844535734 + 0.61417;       // LandauZ maximum location
  Double_t frac[5];
  frac[1] = TMath::Power(TMath::Sin(par[3]),2);
  frac[2] = TMath::Power(TMath::Sin(par[4]),2);
  //  frac[3] = 0.;
  //  frac[4] = 0.;
    frac[3] = TMath::Power(TMath::Sin(par[5]),2);
    frac[4] = TMath::Power(TMath::Sin(par[6]),2);

  if( ((par[4]==0.)&&(par[5]==0.)&&(par[6]==0.)&&(par[3]!=0.)) || 
      ((par[3]==0.)&&(par[5]==0.)&&(par[6]==0.)&&(par[4]!=0.)) ||
      ((par[3]==0.)&&(par[4]==0.)&&(par[6]==0.)&&(par[5]!=0.)) ||
      ((par[3]==0.)&&(par[4]==0.)&&(par[5]==0.)&&(par[6]!=0.)) )
    {frac[0]=0.;
    }

  else {frac[0] = 1. - frac[1] - frac[2] - frac[3] - frac[4];}
  // MP shift correction
  Double_t mpc_pi = TMath::Exp(meand_pi) - mpshift * sigmad_pi; 
  Double_t mpc_pr = TMath::Exp(meand_pr) - mpshift * sigmad_pr;
  Double_t mpc_k = TMath::Exp(meand_k)   - mpshift * sigmad_k;
  Double_t mpc_el = TMath::Exp(meand_el) - mpshift * sigmad_el;
  Double_t mpc_d = TMath::Exp(meand_d)   - mpshift * sigmad_d;

  Double_t xx = TMath::Exp(xd);

  return (xx*TMath::Landau(xx,mpc_pi,sigmad_pi)*frac[0] + 
	  xx*TMath::Landau(xx,mpc_pr,sigmad_pr)*frac[1] + 
	  xx*TMath::Landau(xx,mpc_k, sigmad_k) *frac[2] + 
	  xx*TMath::Landau(xx,mpc_el,sigmad_el)*frac[3] + 
	  xx*TMath::Landau(xx,mpc_d, sigmad_d) *frac[4])*par[7];
}


//________________________________________________________________________________

//-------------------------L A N D A U   5   C L A S S --------------------------------------------------------------------------------//
//-------------------------------------------------------------------------------------------------------------------------//

class RooLandauZ5:public RooAbsPdf
{

public:
  RooLandauZ5(){}
  RooLandauZ5(const char *name, const char *title, RooAbsReal& _x, 
	      RooAbsReal& _norm, RooAbsReal& _mean_pi, 
	      RooAbsReal& _sigma_pi, RooAbsReal& _sigma_pr, 
	      RooAbsReal& _sigma_k, RooAbsReal& _sigma_el, 
	      RooAbsReal& _sigma_d, RooAbsReal& _total, RooAbsReal& _width);
  RooLandauZ5(const RooLandauZ5& other, const char* name=0);
  virtual TObject *clone(const char *newname) const 
  {    return new RooLandauZ5(*this,newname);  }
  inline virtual ~RooLandauZ5() {}

protected:
  RooRealProxy x;
  RooRealProxy norm;
  RooRealProxy mu;
  RooRealProxy sigma_pi;
  RooRealProxy sigma_pr;
  RooRealProxy sigma_k;
  RooRealProxy sigma_el;
  RooRealProxy sigma_d;
  RooRealProxy total;
  RooRealProxy width;

  Double_t evaluate() const;

private:
  ClassDef(RooLandauZ5,1)
};

ClassImp(RooLandauZ5)

RooLandauZ5::RooLandauZ5(const char *name, const char *title, RooAbsReal& _x,
			 RooAbsReal& _norm, RooAbsReal& _mu, 
			 RooAbsReal& _sigma_pi, RooAbsReal& _sigma_pr,
			 RooAbsReal& _sigma_k, RooAbsReal& _sigma_el, 
			 RooAbsReal& _sigma_d, RooAbsReal& _total,
			 RooAbsReal& _width): 
  RooAbsPdf(name,title), 
  x("x","Dependent",this,_x), 
  norm("norm","Norm",this,_norm), 
  mu("mu","peak position",this,_mu),
  sigma_pi("sigma_pi","Width pion",this,_sigma_pi), 
  sigma_pr("sigma_pr","Width proton",this,_sigma_pr),
  sigma_k("sigma_k","Width kaon",this,_sigma_k),
  sigma_el("sigma_el","Width electron",this,_sigma_el),
  sigma_d("sigma_d","Width deutron",this,_sigma_d),
  total("total","Total",this,_total),
  width("width","Width",this,_width){}

RooLandauZ5::RooLandauZ5(const RooLandauZ5& other, const char* name): 
  RooAbsPdf(other,name), 
  x("x",this,other.x), 
  norm("norm",this,other.norm), 
  mu("mu",this,other.mu),
  sigma_pi("sigma_pi",this,other.sigma_pi), 
  sigma_pr("sigma_pr",this,other.sigma_pr),
  sigma_k("sigma_k",this,other.sigma_k), 
  sigma_el("sigma_el",this,other.sigma_el),
  sigma_d("sigma_d",this,other.sigma_d), 
  total("total",this,other.total), 
  width("width",this,other.width){} 


Double_t RooLandauZ5::evaluate() const{
  const Double_t par[10] = {norm,mu,sigma_pi,sigma_pr,sigma_k,sigma_el,sigma_d,total,width};
  Double_t xd =x;
  return landauZ5(&xd, (Double_t *)par);
}

//---------------------------------------------------------------------//
//------Global variables needed to create TF1 for particle fraction ----//
TF1 *l5xg_func = 0, *func_pi=0, *func_pr=0, *func_k=0, *func_el=0, *func_de=0;
Double_t frac_pr=0.,frac_k=0.,frac_el=-0.,frac_de=-0., frac_pi=0.;

//-------------Function for scaling TF1 ---------------// 
Double_t func_lz5xg_mult(Double_t *x, Double_t *par) {
  if (! l5xg_func) return 0;
  Double_t xd = x[0];
  Double_t mult = par[7];
  Int_t Case = (int)par[9];
  switch(Case){
  case 0: 
    if (! func_pi) return 0;
    return ((func_pi->Eval(xd))*mult*frac_pi);
    break;
  case 1: 
    if (! func_pr) return 0;
    return ((func_pr->Eval(xd))*mult*frac_pr);
    break;
  case 2: 
    if (! func_k) return 0;
    return ((func_k->Eval(xd))*mult*frac_k);
    break;
  case 3: 
    if (! func_el) return 0;
    return ((func_el->Eval(xd))*mult*frac_el);    
    break;
  case 4: 
    if (! func_de) return 0;
    return ((func_de->Eval(xd))*mult*frac_de);
    break;
  case 5:
    return ((l5xg_func->Eval(xd))*mult);
    break;
  default: 
    return ((l5xg_func->Eval(xd))*mult);
    break;
  }
}

//--------------------Function for creating TF1 for particle fractions -----//
void Sep_func(RooFFTConvPdf *l5xg, RooRealVar *t, RooRealVar *norm, RooRealVar *mu, RooRealVar *sg, RooRealVar *fProton, RooRealVar *fKaon, RooRealVar *fElectron, RooRealVar *fDeuteron, RooRealVar *total, RooRealVar *width, Int_t i )
{
    switch(i){
    case 0://Pion//
      if (func_pi) delete func_pi;
      func_pi = (TF1*)l5xg->asTF(RooArgList(*t), RooArgList(*norm,*mu,*sg,*fProton,*fKaon,*fElectron,*fDeuteron,*total,*width),*t ); 
      func_pi->FixParameter(3,0.);
      func_pi->FixParameter(4,0.);
      func_pi->FixParameter(5,0.);
      func_pi->FixParameter(6,0.);
      break;
    case 1://Proton//
      if (func_pr) delete func_pr;
      func_pr = (TF1*)l5xg->asTF(RooArgList(*t), RooArgList(*norm,*mu,*sg,*fProton,*fKaon,*fElectron,*fDeuteron,*total,*width),*t );
      frac_pr = TMath::Power(TMath::Sin(func_pr->GetParameter(i+2)),2);
      func_pr->FixParameter(4,0.);
      func_pr->FixParameter(5,0.);
      func_pr->FixParameter(6,0.);
      break;
    case 2: //Kaon//
      if (func_k) delete func_k;
      func_k = (TF1*)l5xg->asTF(RooArgList(*t), RooArgList(*norm,*mu,*sg,*fProton,*fKaon,*fElectron,*fDeuteron,*total,*width),*t );
      frac_k = TMath::Power(TMath::Sin(func_k->GetParameter(i+2)),2);
      func_k->FixParameter(3,0.);
      func_k->FixParameter(5,0.);
      func_k->FixParameter(6,0.);
      break;
    case 3: //Elektron//
      if (func_el) delete func_el;
      func_el = (TF1*)l5xg->asTF(RooArgList(*t), RooArgList(*norm,*mu,*sg,*fProton,*fKaon,*fElectron,*fDeuteron,*total,*width),*t );
      frac_el = TMath::Power(TMath::Sin(func_el->GetParameter(i+2)),2);
      func_el->FixParameter(3,0.);
      func_el->FixParameter(4,0.);
      func_el->FixParameter(6,0.);
      break;
    case 4: //Deuteron//
      if (func_de) delete func_de;
      func_de = (TF1*)l5xg->asTF(RooArgList(*t), RooArgList(*norm,*mu,*sg,*fProton,*fKaon,*fElectron,*fDeuteron,*total,*width),*t );
      frac_de = TMath::Power(TMath::Sin(func_de->GetParameter(i+2)),2);
      func_de->FixParameter(3,0.);
      func_de->FixParameter(4,0.);
      func_de->FixParameter(5,0.);
      break;
    default: break;
    }
}

//----------------------------------------------------------------------//

TF1 *FitRL5(TH1 *hist, Bool_t outer = kFALSE)
{
 if(!hist) return 0;
// cout<<"TEST (FUNKCJA): OUTER sector = "<<outer<<", "<<hist->GetName()<<endl;
 l5xg_func = 0; func_pi=0; func_pr=0; func_k=0; func_el=0;func_de=0; frac_pr=0.;frac_k=0.;frac_el=-0.;frac_de=-0.; frac_pi=0.;

 //-------------------------chose Inner or Outer sector----------------//
  Double_t sec_w=0.;
  if(outer) sec_w = 0.0699;//cout<<"!!!!!!!!!!!!WIDTH = "<<sec_w;}
  else sec_w = 0.0762;

  //-------------------------------Variables---------------------------//
  static RooRealVar *t = 0, *norm = 0, *mu = 0, *sigma = 0, *fProton = 0, *fKaon=0, *fElectron = 0, *fDeuteron = 0, *total = 0, *width = 0;
  static RooRealVar *mg = 0, *sg = 0;
  static RooLandauZ5 *landauZ5 = 0;
  static RooGaussian *gauss = 0;
  static RooFFTConvPdf *l5xg = 0;
  static TF1 *l5xg_mult = 0;
 // cout<<"kasowanie"<<endl;
  if (t) delete t;    
  t         = new RooRealVar("t"        ,"t"        ,-2.,5.) ;
                   t->setBins(1000,"cache");
  if (norm) delete norm;
  norm      = new RooRealVar("norm"     ,"norm"     ,0.) ;
  if (mu) delete mu;
  mu        = new RooRealVar("mu"       ,"mu"       ,0.,-1.,1.) ;
  if (sigma) delete sigma;
  sigma     = new RooRealVar("sigma"    ,"sigma"    ,0.) ;
  if (fProton) delete fProton;
  fProton   = new RooRealVar("fProton"  ,"fProton"  ,0.,0.,1.57) ;
  if (fKaon) delete fKaon;
  fKaon     = new RooRealVar("fKaon"    ,"fKaon"    ,0.,0.,1.57) ;
  if (fElectron) delete fElectron;
  fElectron = new RooRealVar("fElectron","fElectron",0.) ;
  if (fDeuteron) delete fDeuteron;
  fDeuteron = new RooRealVar("fDeuteron","fDeuteron",0.) ;
  if (total) delete total;
  total     = new RooRealVar("total"    ,"total"    ,0.,0.,10.);
  if (width) delete width;
  width     = new RooRealVar("width"    ,"width"    ,sec_w);//0.1,0.01,2.5) ;
  *width = sec_w;

  if (mg) delete mg;
  mg = new RooRealVar("mg","mg",0.) ;
  if (sg) delete sg;
  sg = new RooRealVar("sg","sg",0.25,0.01,10.) ;

  if (landauZ5) delete landauZ5;
  landauZ5= new RooLandauZ5("landauZ5","landauZ5",*t,*norm,*mu,*sigma,*fProton,*fKaon,*fElectron,*fDeuteron,*total,*width) ;
  if (gauss) delete gauss;
  gauss = new RooGaussian ("gauss","gauss",*t,*mg,*sg) ;
  if (l5xg) delete l5xg;
  l5xg = new RooFFTConvPdf("l5xg","landauZ5 (X) gauss",*t,*landauZ5,*gauss) ;
  //Import data
  //--------------------------------------------------------------
  RooDataHist data("data","data",*t,Import(*hist));

  //------- Fit to data -----------------------//
  l5xg->fitTo(data,Save());
  //----- Create TF1 -------------//
  if (l5xg_func) delete l5xg_func;
  l5xg_func = (TF1*)l5xg->asTF(RooArgList(*t), RooArgList(*norm,*mu,*sg,*fProton,*fKaon,*fElectron,*fDeuteron,*total,*width),*t );
  l5xg_func->SetParError(0,norm->getError());
  l5xg_func->SetParError(1,mu->getError());
  l5xg_func->SetParError(2,sg->getError());
  l5xg_func->SetParError(3,fProton->getError());
  l5xg_func->SetParError(4,fKaon->getError());
  l5xg_func->SetParError(5,fElectron->getError());
  l5xg_func->SetParError(6,fDeuteron->getError());
  l5xg_func->SetParError(7,total->getError());
  l5xg_func->SetParError(8,width->getError());

  //----- Scale TF1 fit function ------//
  Double_t mult_ev = hist->Integral()*hist->GetBinWidth(5);
  if (l5xg_mult) delete l5xg_mult;
  l5xg_mult = new TF1(Form("mult_%s",hist->GetName()),func_lz5xg_mult,-2.,5.,11);
  l5xg_mult->SetParent(hist);
  l5xg_mult->SetParName(0,"norm");  //l5xg_mult->SetParLimits(0,-80,80);
  l5xg_mult->SetParName(1,"mu");    //l5xg_mult->SetParLimits(1,-1.5,1.5);
  l5xg_mult->SetParName(2,"Sigma"); //l5xg_mult->SetParLimits(2,0.2,0.8);
  l5xg_mult->SetParName(3,"P"); 
  l5xg_mult->SetParName(4,"K");     //l5xg_mult->SetParLimits(4,0.0,0.5);
  l5xg_mult->SetParName(5,"e");     //l5xg_mult->SetParLimits(5,0.0,0.5);
  l5xg_mult->SetParName(6,"d");     //l5xg_mult->SetParLimits(6,0.0,0.5);
  l5xg_mult->SetParName(7,"Total");//l5xg_mult->SetParLimits(7,0.01,2.0);
  l5xg_mult->SetParName(8,"WidthL");
  l5xg_mult->SetParName(9,"case");  
  l5xg_mult->SetParName(10,"Mu");    //l5xg_mult->SetParLimits(1,-1.5,1.5);
  Double_t pars[11], errs[11];
  memset(pars, 0, sizeof(pars));
  memset(errs, 0, sizeof(errs));
  l5xg_func->GetParameters(pars);

  pars[7] = mult_ev; cout<<"MULT_EV = "<<mult_ev<<endl;
  pars[9] = 5;
  pars[10] = pars[1];
  l5xg_mult->SetParameters(pars);

  l5xg_mult->SetLineColor(1);
  //    l5xg_mult->SetLineStyle(2);
  memcpy(errs, l5xg_func->GetParErrors(), 8*sizeof(Double_t));
  errs[10] = errs[1];
  l5xg_mult->SetParErrors(errs);
  hist->GetListOfFunctions()->Add(l5xg_mult);
  l5xg_mult->SetParent(hist);
  //-- Fits for particles: 0.Pion, 1.Proton, 2.Kaon, 3.Elektron, 4.Deuteron --//
  for (Int_t i = 1; i <= 5; i++) {//<=5
    Int_t j = 0;
    if(i==5)j = 0;//i==5
    else j = i;
    //cout<<"(!!:TEST:!!) i = "<<i<<", j = "<<j<<endl;
    Sep_func(l5xg,t,norm,mu,sg,fProton,fKaon,fElectron,fDeuteron,total,width,j);
    TF1 *g0 = new TF1(Form("%s_%s",Peaks[j].Name,hist->GetName()),func_lz5xg_mult,-2.,5.,11);
    Double_t pars0[11], errs0[11];
    memset(pars0, 0, sizeof(pars0));
    memset(errs0, 0, sizeof(errs0));
    g0->GetParameters(pars0);
    g0->SetParameter(7,mult_ev);
    g0->SetParameter(9,j);
    if(i==1)frac_pi+=frac_pr; //cout<<"(!!:TEST:!!) PROTON : frac_pr = "<<frac_pr<<endl;}
    else if(i==2)frac_pi+=frac_k;// cout<<"(!!:TEST:!!) KAON : frac_k = "<<frac_k<<endl;}
    else if(i==3)frac_pi+=frac_el;// cout<<"(!!:TEST:!!) ELEKTRON : frac_el = "<<frac_el<<endl;}
    else if(i==4)frac_pi+=frac_de;// cout<<"(!!:TEST:!!) DEUTERON : frac_de = "<<frac_de<<endl;}
    else if(i==5){//5
      frac_pi=1.-frac_pi;
      //cout<<"(!!:TEST:!!) PION : frac_pi = "<<frac_pi<<endl;
      //cout<<"!!!!!!!!!!!!!!!!!!PEAK POSS = "<<func_pi->GetParameter(1)<<", max"<<func_pi->GetMaximumX()<<endl;
      pars[1] = func_pi->GetMaximumX();
      l5xg_mult->SetParameter(1,pars[1]);
    }
    g0->SetLineColor(j+2);
    g0->SetParent(hist);
    hist->GetListOfFunctions()->Add(g0);
  }
  Double_t X = l5xg_mult->GetParameter(1);
  Double_t Y = 0;
  //  static TPolyMarker *pm = 0;
  //  if (pm) delete pm;
  TPolyMarker *pm = new TPolyMarker(1, &X, &Y);
  hist->GetListOfFunctions()->Add(pm);
  pm->SetMarkerStyle(23);
  pm->SetMarkerColor(kRed);
  pm->SetMarkerSize(1.3);
  hist->Draw();

  return l5xg_mult;
}
//________________________________________________________________________________
TF1 *FitRL5(const Char_t *hName = "f1_1", Bool_t outer = kFALSE) {
  TH1 *hist = (TH1 *) gDirectory->Get(hName);
  if (! hist) return 0;
  return FitRL5(hist,outer);
}
//---------------------------------------------------------------------------//
//----------------------------LANDAU Z 5 END --------------------------------//
//---------------------------------------------------------------------------//


//----- Fit the line to plot width(row) from MC data -----------------------//
//----- Fit is used in FitRL5 as w width for Inner and Outer sector -------//
void pol_fit(TNtuple *FitP)
{
  //fit for MonteCarlo MIP pion function to find landau width
  TString command("a4:y>>hist(");
  command += 47;
  command += ",0,46)";
  FitP->Draw(command, "", "");
  TH1* hist = (TH1*) gDirectory->Get("hist");
  hist->SetDirectory(0);
  hist->GetXaxis()->SetTitle("t");
  hist->GetYaxis()->SetTitle("width");

  hist->Draw();
  
  TF1 *f1 = new TF1("f1","[0]",0.,13.);
  f1->SetParameter(0,0.076); //Inner: 0.0762
  f1->SetLineColor(2);

  TF1 *f2 = new TF1("f2","[0]",13.,50.);
  f2->SetParameter(0,0.07);//Outer: 0.0699
  f2->SetLineColor(2);

  hist->Fit("f1", "R");
  hist->Fit("f2", "R");
  f1->Draw("same");
  f2->Draw("same");  
}


//________________________________________________________________________________
TF1 *FitRL1lxg() {
  static TF1 *flxg = 0;
  if (flxg) return flxg;
  static RooFFTConvPdf *lxg = 0;
  static RooRealVar *t = 0, *ml = 0, *sl = 0, *mg = 0, *sg = 0;
  static RooLandauZ *landauz = 0;
  static RooGaussian *gauss = 0;
  if (! lxg) {
    // S e t u p   c o m p o n e n t   p d f s 
    // ---------------------------------------
    // Construct observable
    t = new RooRealVar("t","t",-2,6) ;
    // Construct landauz(t,ml,sl) ;
    ml = new RooRealVar("ml","log mean landauz",0.0,-20,20) ;
    sl = new RooRealVar("sl","sigma landauz",0.10,0.01,10) ;
    landauz = new RooLandauZ("lx","lx",*t,*ml,*sl) ;
    // Construct gauss(t,mg,sg)
    mg = new RooRealVar("mg","mg",0) ;
    sg = new RooRealVar("sg","sg",0.25,0.01,10) ;
    gauss = new RooGaussian ("gauss","gauss",*t,*mg,*sg) ;
    // C o n s t r u c t   c o n v o l u t i o n   p d f 
    // ---------------------------------------
    // Set #bins to be used for FFT sampling to 10000
    t->setBins(10000,"cache") ; 
    // Construct landauz (x) gauss
    lxg = new RooFFTConvPdf("lxg","landauZ (X) gauss",*t,*landauz,*gauss) ;
  }
  flxg = (TF1*)lxg->asTF(RooArgList(*t), RooArgList(*ml,*sl,*sg),*t );
  return flxg;
}
//________________________________________________________________________________
Double_t gfR5Func(Double_t *x, Double_t *par) {
  // par[0] - norm
  // par[1] - pion position wrt Z_pion (Bichsel prediction)
  // par[2] - sigma 
  // par[3] - proton signal
  // par[4] - Kaon    -"-
  // par[5] - electorn -"-
  // par[6] - deuteron -"-
  // par[7] - Total
  // par[8] - width of Landau
  Double_t sigma = par[2];
  Double_t frac[5];
  Int_t i;
  frac[0] = 1;
  for (i = 1; i < 5; i++) {
    frac[i] = TMath::Sin(par[2+i]);
    frac[i] *= frac[i];
    frac[0] -= frac[i];
  }
  if (frac[0] < 0.4) return 0;
  Double_t Value = 0;
  Int_t i1 = 0;
  Int_t i2 = 4;
  Int_t icase = (Int_t ) par[9];
  if (icase >= 0) {i1 = i2 = icase;}
  //                        MP,  WidthL,   SigmaG
  Double_t parLI[3] = { par[1],  par[8],    0.24};
  TF1 *lxg = FitRL1lxg();
  for (i = i1; i <= i2; i++) { 
    Double_t Sigma = TMath::Sqrt(sigma*sigma + Peaks[i].sigma*Peaks[i].sigma);
    parLI[0] = par[1] + Peaks[i].peak;
    parLI[1] = par[8];
    parLI[2] = Sigma;
    Value += frac[i]*lxg->EvalPar(x,parLI); 
    //    cout << "i\t" << i << "\tx = " << x[0] << " frac " << frac[i] << "\t" << Value << endl;
  }
  return par[7]*TMath::Exp(par[0])*Value;
}
//________________________________________________________________________________
TF1 *FitR5(TH1 *proj, Option_t *opt="", Int_t nhyps = 5) { // fit by 5 landau convoluted with gauss via RooFit
  // fit in momentum range p = 0.45 - 0.50 GeV/c
  TString Opt(opt);
  //  Bool_t quet = Opt.Contains("Q",TString::kIgnoreCase);
  TF1 *g2 = (TF1*) gROOT->GetFunction("R5");
  if (! g2) {
    g2 = new TF1("R5",gfR5Func, -5, 5, 10);
    g2->SetParName(0,"norm"); g2->SetParLimits(0,-80,80);
    g2->SetParName(1,"mu");     //g2->SetParLimits(1,-1.5,1.5);
    g2->SetParName(2,"Sigma");  g2->SetParLimits(2,0.2,0.8);
    g2->SetParName(3,"P"); 
    g2->SetParName(4,"K");      g2->SetParLimits(4,0.0,0.5);
    g2->SetParName(5,"e");      g2->SetParLimits(5,0.0,0.5);
    g2->SetParName(6,"d");      g2->SetParLimits(6,0.0,0.5);
    g2->SetParName(7,"Total");
    g2->SetParName(8,"WidthL"); g2->SetParLimits(8,0.01,2.0);
    g2->SetParName(9,"case");  
    //    g2->SetParName(7,"factor"); g2->SetParLimits(7,-.1,0.1);
  }
  
  Double_t total = proj->Integral()*proj->GetBinWidth(5);
  g2->SetParameters(0, proj->GetMean(), proj->GetRMS(), 0.0, 0.0, 0.0, 0.0,0.0,0.5,-1);
  g2->FixParameter(3,0);
  g2->FixParameter(4,0);
  g2->FixParameter(5,0);
  g2->FixParameter(6,0);
  g2->FixParameter(7,total);
  g2->FixParameter(9,-1);
  if (! proj) return g2;

  Int_t iok = proj->Fit(g2,Opt.Data());
  if (nhyps == 5) {
    g2->ReleaseParameter(3); g2->SetParLimits(3,0.0,TMath::Pi()/2);
    g2->ReleaseParameter(4); g2->SetParLimits(4,0.0,TMath::Pi()/2);
    g2->ReleaseParameter(5); g2->SetParLimits(5,0.0,TMath::Pi()/2);
    g2->ReleaseParameter(6); g2->SetParLimits(6,0.0,TMath::Pi()/2);
    iok = proj->Fit(g2,Opt.Data());
  }
  if ( iok < 0) {
    cout << g2->GetName() << " fit has failed with " << iok << " for " 
	 << proj->GetName() << "/" << proj->GetTitle() << " Try one again" << endl; 
    proj->Fit(g2,Opt.Data());
  }
  Opt += "m";
  iok = proj->Fit(g2,Opt.Data());
  if (! Opt.Contains("q",TString::kIgnoreCase)) {
    Double_t params[10];
    g2->GetParameters(params);
    Double_t X = params[1];
    Double_t Y = TMath::Exp(params[0]);
    TPolyMarker *pm = new TPolyMarker(1, &X, &Y);
    proj->GetListOfFunctions()->Add(pm);
    pm->SetMarkerStyle(23);
    pm->SetMarkerColor(kRed);
    pm->SetMarkerSize(1.3);
    if (nhyps == 5) {
      for (int i = 0; i < nhyps; i++) {
	TF1 *f = new TF1(*g2);
	f->SetName(Form("L5%s",Peaks[i].Name));
	f->FixParameter(9,i);
	f->SetLineColor(i+2);
	proj->GetListOfFunctions()->Add(f);
      }
      proj->Draw();
    }
  }
  return g2;
}

//________________________________________________________________________________
//RooFitResult *FitRL1(TH1 *hist) {
TF1 *FitRL1(TH1 *hist) {
  if (! hist) return 0;
  // S e t u p   c o m p o n e n t   p d f s 
  // ---------------------------------------
  // Construct observable
  RooRealVar t("t","t",-2,6) ;
  // Construct landauz(t,ml,sl) ;
  RooRealVar ml("ml","log mean landauz",0.0,-20,20) ;
  RooRealVar sl("sl","sigma landauz",0.10,0.01,10) ;
  RooLandauZ landauz("lx","lx",t,ml,sl) ;
  // Construct gauss(t,mg,sg)
  RooRealVar mg("mg","mg",0) ;
  RooRealVar sg("sg","sg",0.25,0.01,10) ;
  RooGaussian gauss("gauss","gauss",t,mg,sg) ;
  // C o n s t r u c t   c o n v o l u t i o n   p d f 
  // ---------------------------------------
  // Set #bins to be used for FFT sampling to 10000
  t.setBins(10000,"cache") ; 
  // Construct landauz (x) gauss
  RooFFTConvPdf lxg("lxg","landauZ (X) gauss",t,landauz,gauss) ;
  // S a m p l e ,   f i t   a n d   p l o t   c o n v o l u t e d   p d f 
  RooDataHist* data = new RooDataHist("data","data",t,Import(*hist));
  // Fit gxlx to data
  //RooFitResult* r = 
  lxg.fitTo(*data,Save()) ;
  // Plot data, landauz pdf, landauz (X) gauss pdf
  RooPlot* frame = t.frame(Title("landauz (x) gauss convolution")) ;
  data->plotOn(frame) ;
  lxg.plotOn(frame) ;
  //  landauz.plotOn(frame,LineStyle(kDashed)) ;
  // Draw frame on canvas
  TCanvas *ca = (TCanvas *) gROOT->GetListOfCanvases()->FindObject("FitRL1");
  if (! ca) ca = new TCanvas("FitRL1","FitRL1",600,600) ;
  else      ca->Clear();
  //  gPad->SetLeftMargin(0.15) ; frame->GetYaxis()->SetTitleOffset(1.4) ; 
  frame->Draw() ;
  delete data;
  return 0;//r;
}
//________________________________________________________________________________
//RooFitResult *FitRL1(const Char_t *hName = "f1_1") {
TF1 *FitRL1(const Char_t *hName = "f1_1") {
  TH1 *hist = (TH1 *) gDirectory->Get(hName);
  if (! hist) return 0;
  return FitRL1(hist);
}
#endif /* __USE_ROOFIT__ */
//________________________________________________________________________________
TF1* Landau(){
  if (!LandauF) 
  LandauF = // P10
    new TF1("LandauF","exp([0]-0.5*((x-[1])/[2])**2+exp([3]-0.5*((x-[4])/[5])**2+exp([6]-0.5*((x-[7])/[8])**2)))",-5,10);
  // dEdxP->Draw("phi*sigma_z:(z-zm)/sigma_z>>Lan(100,-3,7)","bg>1&&x>1&&x<3","prof");
  // Lan->Fit("LandauF","i")
  Double_t params[9] = {
    -7.74975e+00,
     6.53414e+00,
     1.21524e+00,
     3.31409e+00,
    -2.58291e+00,
     3.51463e+00,
    -3.47755e+00,
     3.77698e-02,
     6.67913e-01};
  LandauF->SetParameters(params);
  return LandauF;
}
//________________________________________________________________________________
Double_t fithfcn(Double_t *x,Double_t *par) {
  Double_t z   = x[0];
  Double_t sigma = par[0];
  Double_t norm = 1./TMath::Sqrt(2*TMath::Pi())/sigma;
  Double_t value = 0;
  //  Int_t    hyp   = (Int_t) par[1+2*NH];
  //  Double_t ref = par[hyp+1];
  for (int k = 0; k < NH; k++) {
    Double_t mu = par[k+1];
    //    if (k != hyp) mu -= ref;
    Double_t dev = (z - mu)/sigma;
    value += norm*TMath::Exp(par[k+1+NH]-dev*dev/2.);
  }
  return value;
}
//________________________________________________________________________________
void FitH(const Char_t *set="z", Int_t Hyp = -1, Int_t Bin=-1) {
  if (!gBichsel) {
    gSystem->Load("StBichsel");
    gBichsel = Bichsel::Instance();
  }
  TString Set(set);
  const Double_t window = 0.4;
  const Double_t range[2] = {-2., 4.};
  const Double_t LFrMin = -10;
  if (! canvas) {
    canvas = new TCanvas("FitHC","FitH Canvas");
    canvas->SetGrid();
  }
  const Int_t nHYPS = NHYPS;
  TH2 *hists[nHYPS];
  TProfile *histp[nHYPS];
  TFile *fRootFile = (TFile *) gDirectory->GetFile();
  if (! fRootFile ) {printf("Cannot find/open %s",fRootFile->GetName()); return;}
  else               printf("%s found\n",fRootFile->GetName());
  TString newfile("FitH");
  newfile += Set; 
  newfile += gSystem->BaseName(fRootFile->GetName());
  TString FileN("FitPars");
  FileN += Set;
  FileN += ".h";
  TFile * f = 0;
  if (Bin < 0)  f = new TFile(newfile.Data(),"recreate");
  TH1 *proj = 0;
  TF1 *g = new TF1("g",fithfcn,range[0],range[1],2+2*NH);
  g->SetParName(0,"Sigma"); g->SetParLimits(0,0.06,0.12);
  g->SetParName(1+2*NH,"Hyp");
  Int_t Nfit = 0;
  Int_t nh1 = 0;
  Int_t nh2 = NHYPS-1; //Int_t nh1 = 3, nh2 = 3;
  if (Hyp >=0 ) {nh1 = nh2 = Hyp;}
  Int_t parstatus[NH];
  for (Int_t hyp = nh1; hyp<=nh2; hyp++) {
    Int_t kh = hyp%NH;
    Char_t *HistN                 = (Char_t *) HistNames[hyp];
    if (Set.Contains("70")) HistN = (Char_t *) HistNames70[hyp];
    hists[hyp] = (TH2 *) fRootFile->Get(HistN);
    if (!hists[hyp]) {printf("Cannot histogram %s\n",HistNames[hyp]); continue;}
    histp[hyp] = (TProfile *) fRootFile->Get(HistNameP[hyp]);
    if (!histp[hyp]) {printf("Cannot histogram %s\n",HistNameP[hyp]); continue;;}
    Int_t k = 0;
    if (hyp > NH) k = NH;
    for (Int_t hypl = (hyp/NH)*NH; hypl < (hyp/NH+1)*NH; hypl++) {
      Int_t lh = hypl%NH;
      TString name1("LF.");
      name1 += Names[hypl];
      TString name2("");
      name2 += Names[hypl];
      g->SetParName(lh+1+NH,name1.Data());
      g->SetParName(lh+1,name2.Data());
    }
    Int_t nx = hists[hyp]->GetNbinsX();
    Int_t jbin1 = 1;
    Int_t jbin2 = nx;
    if (Bin > 0) {jbin1 = jbin2 = Bin;}
    for (int jbin=jbin1; jbin<=jbin2; jbin++) {
      TString name(Form("%s_%i_%i",hists[hyp]->GetName(),hyp,jbin));
      proj = hists[hyp]->ProjectionY(name.Data(),jbin,jbin);
      Int_t ix1=proj->GetXaxis()->FindBin(-window);
      Int_t ix2=proj->GetXaxis()->FindBin( window);
      Double_t dinT = 5*proj->Integral();
      Double_t dint = proj->Integral(ix1,ix2);
      if (dint < 100.) {
	printf("hist:%s bin %i for hyp %i has only %10.0f entries\n",hists[hyp]->GetName(),jbin,hyp,dint);
	delete proj;
	continue;
      }
      for (Int_t lh = 0; lh < NH; lh++) {
	g->ReleaseParameter(lh+1);
	g->ReleaseParameter(lh+1+NH);
	parstatus[lh] = 0;
      }
      printf("hist:%s bin %i for hyp %i has  %10.0f entries\n",hists[hyp]->GetName(),jbin,hyp,dint);
      Double_t bg = TMath::Power(10., hists[hyp]->GetBinCenter(jbin));
      Double_t pmom = Masses[hyp]*bg;
      if (pmom > 0.1) {parstatus[4] = 1; printf("fix muon\n");}
      if (kh == 4 && pmom > 0.1) continue;
      Double_t devZ[NH];
      Double_t zref = 0;
      if (TString(set) == "70")  zref = 1.e-6*gBichsel->GetI70(TMath::Log10(bg),1.0); 
      else                       zref = 1.e-6*TMath::Exp(gBichsel->GetMostProbableZ(TMath::Log10(bg),1.0));
      //      printf("zref = %f\n",zref);
      Int_t iok = 1;
      for (Int_t hypl = (hyp/NH)*NH; hypl < (hyp/NH+1)*NH; hypl++) {
	Int_t lh = hypl%NH;
	Double_t z = 0;
	bg = pmom/Masses[hypl];
	if (Set.Contains("70")) z = 1.e-6*gBichsel->GetI70(TMath::Log10(bg),1.0); 
	else                    z = 1.e-6*TMath::Exp(gBichsel->GetMostProbableZ(TMath::Log10(bg),1.0));
	devZ[lh] = TMath::Log(z/zref);
      }
      for (int l = 0; l < NH; l++) {
	if (l == kh) continue;
	if (devZ[l] < -2.0 || devZ[l] > 4.0) {
	  printf("Fix %s dZ = %f\n", Names[NH*(hyp/NH)+l],devZ[l]);
	  parstatus[l] = 1;
	  continue;
	}
	if (TMath::Abs(devZ[l]) < 0.01 || 
	    (hyp%NH == 3 && l == 4 && TMath::Abs(devZ[l]) < 0.04)) {
	    printf("Fix %s dZ = %f\n", Names[NH*(hyp/NH)+l],devZ[l]);
	    parstatus[l] = 1;
	    continue;
	}
      }
      for (Int_t hypl = (hyp/NH)*NH; hypl < (hyp/NH+1)*NH; hypl++) {
      AGAIN:
	Int_t lh = hypl%NH;
	if (parstatus[lh]) continue;
	Double_t windowN = devZ[lh]-window;
	Double_t windowP = devZ[lh]+window;
	for (int m = 0; m < NH; m++) {
	  if (m != lh && ! parstatus[m]) {
	    Double_t dev = 0.5*(devZ[m] - devZ[lh]);
	    if (dev < 0 && windowN < devZ[lh] + dev) windowN = devZ[lh] + dev;
	    if (dev > 0 && windowP > devZ[lh] + dev) windowP = devZ[lh] + dev;
	  } 
	}
	g->SetParameter(lh+1,devZ[lh]);
	g->SetParLimits(lh+1,windowN, windowP);
	g->SetParLimits(lh+1+NH,LFrMin, TMath::Log(dinT));
	if (hypl == hyp) 
	  g->SetParameter(lh+1+NH,TMath::Log(dint));
	else {
	  ix1=proj->GetXaxis()->FindBin(windowN);
	  ix2=proj->GetXaxis()->FindBin(windowP);
	  Double_t din= proj->Integral(ix1,ix2);
	  //	  printf("%s dZ = %f in = %f\n", Names[hypl], devZ[lh], din);
	  if (din > 0) g->SetParameter(lh+1+NH,TMath::Log(din));
	  else         {
	    printf("Fix %s din = %f\n", Names[hypl],din);
	    parstatus[lh] = 1;
	    goto AGAIN;
	  }
	}
      }
      for (int l = 0; l < NH; l++) 
	if (parstatus[l]) {
	  printf("Fix %s\n",Names[NH*(hyp/NH)+l]);
	  g->FixParameter(l+1+NH, LFrMin); 
	  g->FixParameter(l+1, devZ[l]);
	}
      g->FixParameter(1+2*NH, hyp);
      if (parstatus[kh]) iok = 0;
      if (! iok) {printf("Too close\n"); continue;}
      //      proj->Fit(g->GetName(),"RIM");
      proj->Fit(g->GetName(),"R");
      canvas->Update();
      Int_t lh = (hyp%NH)+1;
      Double_t Nul  = g->GetParameter(lh); 
      printf("hyp = %i lh = %i Nul = %f zref =%f %f\n",hyp,lh,Nul,zref,g->GetParameter(lh));
      Double_t Mul  = 1.e6*zref*TMath::Exp(Nul);
      Double_t dMul = g->GetParError(lh);
      Int_t NFitPoints = g->GetNumberFitPoints();
      Int_t NDF = g->GetNDF();
      Double_t prob = g->GetProb();//TMath::Prob(chisq, NDF);
      Double_t chisq      = g->GetChisquare();
      Double_t X = hists[hyp]->GetXaxis()->GetBinCenter(jbin);
      printf("Nul = %f Mul = %f dMul = %f\n",Nul,Mul,dMul);
      printf ("%s :hyp = %i bin=%i, Point=%i, x=%f, p=%f, Delta_I=%f, I=%f, Sigma_I=%f,\n"
	      "chisq=%f, NoPoints=%i,ndf=%i, prob=%f\n",
	      Names[hyp],hyp,jbin,Nfit,X,pmom,Nul,Mul,dMul,chisq,NFitPoints,NDF,prob);
      printf("{\"%-4s\",%2i,%4i,%6i,%6.3f,%7.3f,%10.6f,%10.6f,%8.5f,%10.3f},//%3i,%3i,%5.3f -- %s\n",
	     Names[hyp],hyp,jbin,Nfit,X,pmom,Nul,Mul,dMul,chisq,NFitPoints,NDF,prob,g->GetName());
      if (f) {
	FILE *fp = fopen(FileN.Data(),"a");
	if (fp) {
	  if (Nfit == 0) {
	    TDatime time;
	    fprintf (fp,"dEdxPoint_t dEdxZ[] = {\n");
		    fprintf(fp,"// Date: Time = %i : %i\n",time.GetDate(), time.GetTime());
	    fprintf(fp,
		    "//          bin, Point,     x,      p,   Delta_I,         I, Sigma_I, chisq, NoPoints,ndf, prob\n");
	  }
	  if (Nfit == 0) fprintf(fp," {");
	  else           fprintf(fp,",{");
	  fprintf(fp,
		  "\"%-4s\",%2i,%4i,%6i,%6.3f,%7.3f,%10.6f,%10.6f,%8.5f,%10.3f},//%3i,%3i,%5.3f -- %s\n",
		  Names[hyp],hyp,jbin,Nfit,X,pmom,Nul,Mul,dMul,chisq,NFitPoints,NDF,prob,g->GetName());
	  fclose(fp);
	  Nfit++; 	
	}
	proj->Write();
	delete proj;
      }
    }
  }
  if (f) {
    FILE *fp = fopen(FileN.Data(),"a");
    if (fp) fprintf(fp,"};\n");
    fclose(fp);
    delete f;
  }
}


//________________________________________________________________________________
TH2F *Project(TH3F *hist) {
  if (!hist) return 0;
  Int_t nx = hist->GetNbinsX();
  Int_t ny = hist->GetNbinsY();
  TString name(hist->GetName());
  name += "_xy";
  TH2F *h = new TH2F(name.Data(),hist->GetTitle(),
		     nx,hist->GetXaxis()->GetXmin(),hist->GetXaxis()->GetXmax(),
		     ny,hist->GetYaxis()->GetXmin(),hist->GetYaxis()->GetXmax());
  Double_t params[5];
  Double_t error;
  TF1 *g = new TF1("g","gaus(0)+pol1(3)");
  for (int i=1;i<=nx;i++){
    for (int j=1;j<=ny;j++){
      TH1D *proj = hist->ProjectionZ("f_11",i,i,j,j);
      Double_t mean = proj->GetMean();
      Double_t sum  = proj->Integral();
      Double_t rms = proj->GetRMS();
      params[0] = sum;
      params[1] = mean;
      params[2] = rms;
      params[3] = 0;
      params[4] = 0;
      g->SetParameters(params);
      //      g->SetRange(mean-2*rms,mean+2*rms);
      g->SetRange(-1.,1.);
      error = 0;
      if (sum > 100) {
	proj->Fit("g","RQ");
	g->GetParameters(params);
	error = g->GetParError(1);
      }
      else error = 0;
      h->SetCellContent(i,j,params[1]);
      h->SetCellError(i,j,error);
      printf("i:%i j:%i sum:%f mean:%f rms:%f mu:%f sigma:%f\n",
	     i,j,sum,mean,rms,params[1],params[2]);
      delete proj;
    }
  }
  return h;
} 
//________________________________________________________________________________
TH2D *GetRelYZError(TH3 *hist, const Char_t *Name="_yz") {
  if (!hist) return 0;
  Int_t nx = hist->GetNbinsX();
  Int_t ny = hist->GetNbinsY();
  Int_t nz = hist->GetNbinsZ();
  TString name(hist->GetName());
  name += Name;
  //  TAxis *fXaxis = hist->GetXaxis();
  TAxis *fYaxis = hist->GetYaxis();
  TAxis *fZaxis = hist->GetZaxis();
  TH2D *h = new TH2D(name.Data(),"Relative error in Space charge (%)",
		     ny,fYaxis->GetXmin(),fYaxis->GetXmax(),
		     nz,fZaxis->GetXmin(),fZaxis->GetXmax());
  h->SetXTitle("Row number");
  h->SetYTitle("Z (cm)");
  Int_t bin;
  Double_t Scale = TMath::Sqrt(24.*3726./1153.);
  for (int iybin=0;iybin<ny;iybin++){
    for (int izbin=0;izbin<nz;izbin++){
      Double_t cont = 0, err = 0;
      for (int ixbin=1;ixbin<=nx;ixbin++){
	bin = hist->GetBin(ixbin,iybin,izbin);
	cont += hist->GetBinContent(bin);
	err  += hist->GetBinError(bin)*hist->GetBinError(bin);
      }
      if (cont > 0) {
	bin = h->GetBin(iybin,izbin);
	Double_t val = 100*TMath::Sqrt(err)/cont*Scale;
	h->SetBinContent(bin,val);
      }
    }
  }
  return h;
} 
//________________________________________________________________________________
TH2F *ProjectX(TH3F *hist, const Char_t *Name="_yz",const Int_t binx1=0,const Int_t binx2=10000) {
  if (!hist) return 0;
  Int_t nx = hist->GetNbinsX();
  if (nx > binx2) nx = binx2;
  Int_t ny = hist->GetNbinsY();
  Int_t nz = hist->GetNbinsZ();
  TString name(hist->GetName());
  name += Name;
  //  TAxis *fXaxis = hist->GetXaxis();
  TAxis *fYaxis = hist->GetYaxis();
  TAxis *fZaxis = hist->GetZaxis();
  TH2F *h = new TH2F(name.Data(),hist->GetTitle(),
		     ny,fYaxis->GetXmin(),fYaxis->GetXmax(),
		     nz,fZaxis->GetXmin(),fZaxis->GetXmax());
  //  Double_t params[3];
  //  Double_t error;
  //  TF1 *g = new TF1("g","gaus");
  for (int iybin=0;iybin<ny;iybin++){
    for (int izbin=0;izbin<nz;izbin++){
      for (int ixbin=binx1;ixbin<nx;ixbin++){
	Int_t bin = hist->GetBin(ixbin,iybin,izbin);
	Double_t cont = hist->GetBinContent(bin);
	if (cont) h->Fill(fYaxis->GetBinCenter(iybin),fZaxis->GetBinCenter(izbin), cont);
      }
    }
  }
  return h;
} 
//________________________________________________________________________________
Double_t funFreq(Double_t *x, Double_t *par) {
  Double_t value = 0;
  return par[3] + TMath::Exp(par[0])*TMath::Freq(-(x[0]-par[1])*par[2]);
}
//________________________________________________________________________________
TF1 *FunFreq() {
  TF1 *f = (TF1*) gROOT->GetFunction("FunFreq");
  if (! f) {
    f = new TF1("FunFreq",funFreq,-1000,1000,4);
    f->SetParNames("Norm","#mu","1/#sigma","grass");
    f->SetParameters(0,208,1.,0);
    f->SetParLimits(0,-10,20);
    f->SetParLimits(1,-1000,1000);
    //    f->SetParLimits(2,0,10);
  }
  return f;
}
//________________________________________________________________________________
Double_t funFreqp2(Double_t *x, Double_t *par) {
  return TMath::Exp(par[0])*TMath::Freq(-(x[0]-par[1])*par[2])  + par[3] +  x[0]*(par[4] + x[0]*par[5]);
}
//________________________________________________________________________________
TF1 *FunFreqP2() {
  TF1 *f = (TF1*) gROOT->GetFunction("FunFreqP2");
  if (! f) {
    f = new TF1("FunFreqp2",funFreqp2,-1000,1000,6);
    f->SetParNames("Norm","#mu","1/#sigma","a[0]","a[1]","a[2]");
    f->SetParameters(0,208,1.,0);
    f->SetParLimits(0,-10,20);
    f->SetParLimits(1,-1000,1000);
    //    f->SetParLimits(2,0,10);
    f->SetParameters(0.0, 2.6, -3.0, -0.1, 0.0, 0.0);
  }
  return f;
}
//________________________________________________________________________________
TF1 *FitFreq(TH1 *proj, Option_t *opt="", Double_t zmin = 205, Double_t zmax = 215) { // Fit by Freq function
  TString Opt(opt);
  //  Bool_t quet = Opt.Contains("Q",TString::kIgnoreCase);
  TF1 *g2 = FunFreq();
  if (! proj) return g2;
  Double_t total = proj->Integral()*proj->GetBinWidth(5);
  if (total < 1) return 0;
  g2->SetRange(zmin,zmax);
  g2->SetParameters(0,0.5*(zmin + zmax),1.,0);
  Int_t iok = proj->Fit(g2,Opt.Data());
  if ( iok < 0) {
    cout << g2->GetName() << " fit has failed with " << iok << " for " 
	 << proj->GetName() << "/" << proj->GetTitle() << " Try one again" << endl; 
    proj->Fit(g2,Opt.Data());
  }
  Opt += "m";
  iok = proj->Fit(g2,Opt.Data());
  if (! Opt.Contains("q",TString::kIgnoreCase)) {
    Double_t params[10];
    g2->GetParameters(params);
    Double_t X = params[1];
    Double_t Y = TMath::Exp(params[0]);
    TPolyMarker *pm = new TPolyMarker(1, &X, &Y);
    proj->GetListOfFunctions()->Add(pm);
    pm->SetMarkerStyle(23);
    pm->SetMarkerColor(kRed);
    pm->SetMarkerSize(1.3);
  }
  return g2;
}
//________________________________________________________________________________
Double_t funcLN(Double_t *x, Double_t *p = 0) {
  //  static 
  Double_t Sqrt2piI = 1./TMath::Sqrt(TMath::TwoPi());
  if (x[0] <= 0.0) return 0;
  Double_t lnX = -100;
  if (x[0] > 0) lnX = TMath::Log(x[0]);
  Double_t Norm  = TMath::Exp(p[0]);
  Double_t mu    = p[1];
  Double_t sigma = p[2];
  Double_t dev    = (lnX - mu)/sigma;
  Double_t val = Norm*Sqrt2piI/sigma*TMath::Exp(-dev*dev/2. - lnX);
  return val;
}
//________________________________________________________________________________
TF1 *LogNor(Double_t Mean, Double_t RMS) {
  static TF1 *f = 0;
  if (! f) {
    f = new TF1("LogNor",funcLN, -2, 5, 3);
    f->SetParName(0,"logNorm");
    f->SetParName(1,"mu");
    f->SetParName(2,"sigma");
  }
  /*
    root.exe [127] bin->Fit("gaus")
 FCN=2.23005e+07 FROM MIGRAD    STATUS=CONVERGED     144 CALLS         145 TOTAL
                     EDM=1.64469e-06    STRATEGY= 1      ERROR MATRIX ACCURATE 
  EXT PARAMETER                                   STEP         FIRST   
  NO.   NAME      VALUE            ERROR          SIZE      DERIVATIVE 
   1  Constant     7.73934e-01   7.37966e-05   1.43110e-04   2.32586e+01
   2  Mean         8.26344e-01   2.98068e-05   4.48187e-05   4.11637e+01
   3  Sigma        2.53134e-01   1.30189e-05   1.98117e-05  -4.61696e+01

  Mean = exp(mu-sigma**2/2)
  RMS = Mean*sqrt(exp(sigma**2)-1)

  (RMS/Mean)**2 = exp(sigma**2) - 1
  exp(sigma**2) = (RMS/Mean)**2 + 1
  sigma = sqrt(log((RMS/Mean)**2 + 1)) => 1.54909878849684235e+00
  double RMS =       2.53134e-01
  double Mean  =       8.26344e-01
  double sigma = sqrt(log(RMS/Mean)**2) + 1)) = 1.54909878849684235e+00
  mu - sigma**2/2 = log(Mean);
  double mu = sigma*sigma/2 + log(Mean) = 1.00910940099364188e+00
  */ 
  Double_t sigma = TMath::Sqrt(TMath::Log((RMS/Mean)*(RMS/Mean) + 1));
  Double_t mu = sigma*sigma/2 + TMath::Log(Mean);
  f->SetParameters(0, mu, sigma);
  return f;
}
//________________________________________________________________________________
TF1 *FitLN(TH1 *proj, Option_t *opt="RQ", Double_t nSigma=3, Int_t pow=3, Double_t zmin = -2, Double_t zmax = 5, Double_t SX = 1) { // log normal
  Double_t params[9];
#if 0
  Int_t binMin = proj->GetXaxis()->FindBin(zmin);
  Int_t binMax = proj->GetXaxis()->FindBin(zmax);
  proj->GetXaxis()->SetRange(binMin,binMax);
#endif
  Int_t peak = proj->GetMaximumBin();
  Double_t peakX = proj->GetBinCenter(peak);
  Double_t peakY = proj->GetBinContent(peak);
  if (peakY <= 0.) return 0;
  params[0] = TMath::Log(peakY);
  params[1] = peakX; // proj->GetMean();
  params[2] = 0.5*proj->GetRMS();
  params[3] = 0;
  params[4] = 0;
  params[5] = 0;
  params[6] = 0;
  params[7] = 0;
  params[8] = 0;
  Double_t sigma = nSigma*params[2];
  TF1 *g = LogNor(proj->GetMean(),proj->GetRMS());
  if (SX > 0) {
    g->FixParameter(2,SX);
  } else {
    g->ReleaseParameter(2);
  }
#if 0
  g->SetParameters(params);
  g->SetRange(params[1]-sigma,params[1]+sigma);
  g->SetRange(params[1]-sigma,params[1]+sigma);
  g->SetParLimits(1, params[1]-params[2], params[1]+params[2]);
  g->SetParLimits(2, 1e-3, proj->GetRMS());
#endif
  Bool_t res = proj->Fit(g,opt);
#if 0
  if (g->GetProb() > 0.01) {
    return g;
  }
  // Last attempt reduce fit range to +/- 2 sigma
  g->GetParameters(params);
  Bool_t res = proj->Fit(g,opt,"",params[1]-2*params[2],params[1]+2*params[2]);
#endif
  return g;
}
//________________________________________________________________________________
Bool_t  PreSetG0Parameters(TH1 *proj, TF1 *g2) { // Fit peak nearest to 0 by gaus
  if (! proj || ! g2) return kFALSE;
  static TSpectrum *fSpectrum = 0;
  if (! fSpectrum) {
    fSpectrum = new TSpectrum(6);
  }
  // Find pion peak
  Int_t nfound = fSpectrum->Search(proj);
  if (! nfound) return kFALSE;
  Int_t NN = nfound + 1;
#if  ROOT_VERSION_CODE < ROOT_VERSION(6,0,0)
  TArrayF X(NN,fSpectrum->GetPositionX());
  TArrayF Y(NN,fSpectrum->GetPositionY());
#else
  TArrayD X(NN,fSpectrum->GetPositionX());
  TArrayD Y(NN,fSpectrum->GetPositionY());
#endif
  TArrayI idxT(NN);
  Int_t nP = 0;
  Double_t xpi = -9999;
  Int_t ixpi = -1;
  Double_t xPeak[10] = {0};
  Double_t yPeak[10] = {0};
  if (nfound > 0) {
    TMath::Sort(nfound,X.GetArray(),idxT.GetArray(),kFALSE);
    for (Int_t i = 0; i < nfound; i++) {
      Int_t p = idxT[i];
      Double_t xp = X[p];
      Int_t bin = proj->GetXaxis()->FindBin(xp);
      Double_t yp = proj->GetBinContent(bin);
      Double_t ep = proj->GetBinError(bin);
      if (yp-5*ep < 0) continue;
      // take only peak associated with pion, proton and deuteron
      xPeak[nP] = xp;
      yPeak[nP] = yp;
      if (TMath::Abs(xPeak[nP]) < TMath::Abs(xpi)) {xpi = xPeak[nP]; ixpi = nP;}
      nP++;
    }
  }   
  if (nP <= 0) return kFALSE;
  if (ixpi < 0) return kFALSE;
  if (TMath::Abs(xpi) > 0.5) return kFALSE;
#if 0
  Double_t xL = 999, xH = 999;
  if (ixpi >      0) xL = xPeak[ixpi-1];
  if (ixpi < nP - 1) xH = xPeak[ixpi+1];
  Int_t binMin = proj->GetXaxis()->FindBin(xL);
  Int_t binMax = proj->GetXaxis()->FindBin(xH);
  proj->GetXaxis()->SetRange(binMin,binMax);
#endif
  Double_t total = proj->Integral();// *proj->GetBinWidth(5);
  if (total < 100) return kFALSE;
  g2->SetParameter(1, proj->GetMean());
  g2->SetParameter(2, proj->GetRMS());
  g2->SetParameter(7, total);
  return kTRUE;
}
//________________________________________________________________________________
TF1 *FitG0P(TH1 *proj, Option_t *opt="R", Int_t pow=-1, Double_t zmin = -0.4, Double_t zmax = 0.4) {
  if (! proj) return 0;
  TF1 *g = 0;
  Double_t params[9];
  Int_t binMin = proj->GetXaxis()->FindBin(zmin);
  Int_t binMax = proj->GetXaxis()->FindBin(zmax);
  proj->GetXaxis()->SetRange(binMin,binMax);
  Int_t peak = proj->GetMaximumBin();
  Double_t peakX = proj->GetBinCenter(peak);
  Double_t peakY = proj->GetBinContent(peak);
  if (peakY <= 100.) return 0;
  params[0] = TMath::Log(peakY);
  params[1] = peakX; // proj->GetMean();
  params[2] = 0.5*proj->GetRMS();
  for (Int_t p = -1; p <= pow; p++) {
    TString fName("lgaus");
    if (p >= 0) fName += p;
    g = (TF1 *) gROOT->GetListOfFunctions()->FindObject(fName);
    if (! g) {
      TString Func("TMath::Exp([0])*TMath::Gaus(x, [1], [2])");
      if (p >= 0) Func += Form("+pol%i(3)",p);
      g = new TF1(fName,Func,zmin,zmax);
      g->SetParName(0,"log(Const)");
      g->SetParName(1,"Mean");
      g->SetParName(2,"sigma");
      for (Int_t i = 0; i <= p+1; i++) {
	g->SetParName(3+i,Form("a[%i]",i));
      }
    }
    g->SetParameters(params);
    //    if (! PreSetG0Parameters(proj, g)) {return 0;}
    Bool_t res = proj->Fit(g,opt);
    if (g->GetProb() > 0.01) {
      return g;
    }
    g->GetParameters(params);
  }
  // Last attempt reduce fit range to +/- 2 sigma
  Bool_t res = proj->Fit(g,opt,"",params[1]-2*params[2],params[1]+2*params[2]);
  return g;
}
//________________________________________________________________________________
TF1 *FitG(TH1 *proj, Option_t *opt="R") {
  if (! proj) return 0;
  TString fName("gaus");
  TF1 *g = (TF1 *) gROOT->GetListOfFunctions()->FindObject(fName);
#if 0
  if (! g) {
    TString Func("TMath::Exp([0])*TMath::Gaus(x, [1], [2],1)");
    g = new TF1(fName,Func,-5,5);
    g->SetParName(0,"log(Const)");
    g->SetParName(1,"Mean");
    g->SetParName(2,"sigma");
  }
  g->SetParameters(0,TMath::Log(proj->GetEntries()));
  g->SetParameters(1,proj->GetMean());
  g->SetParameters(2,proj->GetRMS());
  // Last attempt reduce fit range to +/- 2 sigma
#endif
  if (! g) {
    TF1::InitStandardFunctions();
    g = (TF1 *) gROOT->GetListOfFunctions()->FindObject(fName);
  }
  g->SetRange(-5,5);
  Bool_t res = proj->Fit(g,opt);
  return g;
}
//________________________________________________________________________________
TF1 *FitGP(TH1 *proj, Option_t *opt="RQ", Double_t nSigma=3, Int_t pow=3, Double_t zmin = -2, Double_t zmax = 2) {
  if (! proj) return 0;
  Double_t params[9];
  Int_t binMin = proj->GetXaxis()->FindBin(zmin);
  Int_t binMax = proj->GetXaxis()->FindBin(zmax);
  proj->GetXaxis()->SetRange(binMin,binMax);
  Int_t peak = proj->GetMaximumBin();
  Double_t peakX = proj->GetBinCenter(peak);
  Double_t peakY = proj->GetBinContent(peak);
  if (peakY <= 0.) return 0;
  params[0] = TMath::Log(peakY);
  params[1] = peakX; // proj->GetMean();
  params[2] = 0.5*proj->GetRMS();
  params[3] = 0;
  params[4] = 0;
  params[5] = 0;
  params[6] = 0;
  params[7] = 0;
  params[8] = 0;
  Double_t sigma = nSigma*params[2];
  TF1 *g = 0;
  for (Int_t p = -1; p <= pow; p++) {
    TString fName("lgaus");
    if (p >= 0) fName += p;
    g = (TF1 *) gROOT->GetListOfFunctions()->FindObject(fName);
    if (! g) {
      TString Func("TMath::Exp([0])*TMath::Gaus(x, [1], [2])");
      if (p >= 0) Func += Form("+pol%i(3)",p);
      g = new TF1(fName,Func,zmin,zmax);
      g->SetParName(0,"log(Const)");
      g->SetParName(1,"Mean");
      g->SetParName(2,"sigma");
      for (Int_t i = 0; i <= p+1; i++) {
	g->SetParName(3+i,Form("a[%i]",i));
      }
    }
    g->SetParameters(params);
    if (sigma > 0) {
      g->SetRange(params[1]-sigma,params[1]+sigma);
      g->SetRange(params[1]-sigma,params[1]+sigma);
      g->SetParLimits(1, params[1]-params[2], params[1]+params[2]);
      g->SetParLimits(2, 1e-3, proj->GetRMS());
    }
    Bool_t res = proj->Fit(g,opt);
    if (g->GetProb() > 0.01) {
      return g;
    }
  }
  // Last attempt reduce fit range to +/- 2 sigma
  g->GetParameters(params);
  Bool_t res = proj->Fit(g,opt,"",params[1]-2*params[2],params[1]+2*params[2]);
  return g;
}
//________________________________________________________________________________
TF1 *FitADC(TH1 *proj, Option_t *opt="Q", Double_t nSigma=3, Int_t pow=3) {
  if (! proj) return 0;
  TString Opt(opt);
  //  Bool_t quet = Opt.Contains("Q",TString::kIgnoreCase);
  TF1 *g = 0, *g0 = 0;
  TF1 *gaus = (TF1*) gROOT->GetFunction("gaus");
  if (pow >= 0) g0 = new TF1("g0",Form("gaus(0)+pol%i(3)",pow),-2.0,2.0);
  else          g0 = new TF1("g0","gaus",-2.0,2.0); 
  g0->SetParName(0,"Constant");
  g0->SetParName(1,"Mean");  g0->SetParLimits(1, -0.1, 0.8);
  g0->SetParName(2,"Sigma"); g0->SetParLimits(2,  0.0, 0.1);
  for (int i=0; i<=pow;i++) g0->SetParName(3+i,Form("a%i",i));
  TF1 *g1 = new TF1("g1",Form("gaus(0)+pol%i(3)",pow+1),-2.0,2.0);
  g1->SetParName(0,"Constant");
  g1->SetParName(1,"Mean");  g1->SetParLimits(1, -0.1, 0.8);
  g1->SetParName(2,"Sigma"); g1->SetParLimits(2,  0.0, 0.1);
  for (int i=0; i<=pow+1;i++) g1->SetParName(3+i,Form("a%i",i));
  TF1 *g2 = new TF1("g2",Form("gaus(0)+pol%i(3)",pow+2),-2.0,2.0);
  g2->SetParName(0,"Constant");
  g2->SetParName(1,"Mean");  g2->SetParLimits(1, -0.1, 0.8);
  g2->SetParName(2,"Sigma"); g2->SetParLimits(2,  0.0, 0.1);
  for (int i=0; i<=pow+2;i++) g2->SetParName(3+i,Form("a%i",i));
  Double_t params[9];
  Int_t peak = proj->GetMaximumBin();
  Double_t peakX = proj->GetBinCenter(peak);
  params[0] = proj->GetBinContent(peak);
  if (peakX > 10.5) {
    params[1] = 0;
    params[2] = 0.05;
  }
  else {
    params[1] = peakX;
    params[2] = 0.05;
  }
  Double_t sigma = nSigma*params[2];
  params[3] = 0;
  params[4] = 0;
  params[5] = 0;
  params[6] = 0;
  params[7] = 0;
  params[8] = 0;
  Int_t iok = 0;
  if (gaus) {
    g = gaus;
    g->SetParameters(params);
    g->SetRange(params[1]-sigma,params[1]+sigma);
    iok = proj->Fit(g,opt);
    if (! iok) {
      if (g->GetProb() > 0.01) return g;
       g->GetParameters(params);
      params[2] = TMath::Abs(params[2]);
    }
  }
  sigma = 0.1; // nSigma*params[2];
  g = g0;
  g->SetParameters(params);
  g->SetRange(params[1]-sigma,params[1]+sigma);
  iok =  proj->Fit(g,opt);
  if (! iok) {
    if (g->GetProb() > 0.01) return g;
    g->GetParameters(params);
    params[2] = TMath::Abs(params[2]);
  }
  g = g1;
  //  sigma = nSigma*params[2];
  g->SetParameters(params);
  g->SetRange(params[1]-sigma,params[1]+sigma);
  iok = proj->Fit(g,opt);
  if (! iok) {
    if (g->GetProb() > 0.01) return g;
    g->GetParameters(params);
    params[2] = TMath::Abs(params[2]);
  }
  g = g2;
  params[2] = TMath::Abs(params[2]);
  //  sigma = nSigma*params[2];
  g->SetParameters(params);
  g->SetRange(params[1]-sigma,params[1]+sigma);
  iok = proj->Fit(g,opt);
  if (! Opt.Contains("q",TString::kIgnoreCase)) {
    g->GetParameters(params);
    Double_t X = params[1];
    Double_t Y = params[0]/TMath::Sqrt(2*TMath::Pi()*params[2]);
    TPolyMarker *pm = new TPolyMarker(1, &X, &Y);
    proj->GetListOfFunctions()->Add(pm);
    pm->SetMarkerStyle(23);
    pm->SetMarkerColor(kRed);
    pm->SetMarkerSize(1.3);
    proj->Draw();
  }
  return g;
}
//________________________________________________________________________________
Double_t gfFunc(Double_t *x, Double_t *par) {
  // par[0] - norm
  // par[1] - pion position wrt Z_pion (Bichsel prediction)
  // par[2] - sigma 
  // par[3] - proton signal
  // par[4] - Kaon    -"-
  // par[5] - electorn -"-
  // par[6] - deuteron -"-
  // par[7] - Total
  // par[8] - case (-1 all, >-0 hyp no.)
  // par[9] - scale 
  Double_t sigma = par[2];
  Double_t scale = par[9];
  Double_t frac[5];
  Int_t i;
  frac[0] = 1;
  for (i = 1; i < 5; i++) {
    frac[i] = TMath::Sin(par[2+i]);
    frac[i] *= frac[i];
    frac[0] -= frac[i];
  }
  if (frac[0] < 0.4 && frac[1] < 0.4) return 0;
  Double_t Value = 0;
  Int_t icase = (Int_t) par[8];
  Int_t i1 = 0;
  Int_t i2 = 4;
  if (icase >= 0) {i1 = i2 = icase;}
  for (i = i1; i <= i2; i++) { 
    Double_t Sigma = sigma;
    if (Peaks[i].N == 0) {
      Value += frac[i]*TMath::Gaus(x[0],scale*(par[1]+Peaks[i].peak),Sigma,1);
    } else {
      for (Int_t ix = 0; ix < Peaks[i].N; ix++) {
	Value += frac[i]*TMath::Gaus(x[0],scale*(par[1]+Peaks[i].X[ix]),Sigma,1)*Peaks[i].Y[ix];
      }
    }
    //    cout << "i\t" << i << "\tx = " << x[0] << " frac " << frac[i] << "\t" << Value << endl;
  }
  return par[7]*TMath::Exp(par[0])*Value;
}
//________________________________________________________________________________
void PreSetParameters(TH1 *proj, TF1 *g2) {
  if (! proj || ! g2) return;
  static TSpectrum *fSpectrum = 0;
  if (! fSpectrum) {
    fSpectrum = new TSpectrum(6);
  }
  // Find pion peak
  Int_t nfound = fSpectrum->Search(proj);
  Int_t NN = nfound + 1;
#if  ROOT_VERSION_CODE < ROOT_VERSION(6,0,0)
  TArrayF X(NN,fSpectrum->GetPositionX());
  TArrayF Y(NN,fSpectrum->GetPositionY());
#else
  TArrayD X(NN,fSpectrum->GetPositionX());
  TArrayD Y(NN,fSpectrum->GetPositionY());
#endif
  TArrayI idxT(NN);
  Int_t nP = 0;
  //                             pi,        proton,          kaon,             e,     deuteron, 
  Double_t frac[5] = {           1.,          1e-6,          1e-6,          1e-6,          1e-6};
  Double_t post[5] = {Peaks[0].peak, Peaks[1].peak, Peaks[2].peak, Peaks[3].peak, Peaks[4].peak};
  Double_t T = 0;
  Double_t tots[5] = {0};
  Double_t xpiPd[3] = {0};
  Double_t ypiPd[3] = {0};
  Double_t ppiPd[3] = {Peaks[0].peak, Peaks[1].peak, Peaks[4].peak};
  Double_t shift = 0;
  Double_t total = 0;
  if (nfound > 0) {
    TMath::Sort(nfound,X.GetArray(),idxT.GetArray(),kFALSE);
    for (Int_t i = 0; i < nfound; i++) {
      Int_t p = idxT[i];
      Double_t xp = X[p];
      Int_t bin = proj->GetXaxis()->FindBin(xp);
      Double_t yp = proj->GetBinContent(bin);
      Double_t ep = proj->GetBinError(bin);
      if (yp-5*ep < 0) continue;
      // take only peak associated with pion, proton and deuteron
      xpiPd[nP] = xp;
      ypiPd[nP] = yp;
      total += yp;
      shift += (xp - ppiPd[nP])*yp; 
      nP++;
      if (nP == 3) break;
    }
  }   
  if (nP > 0) {
    tots[0] = ypiPd[0];
    tots[1] = ypiPd[1];
    tots[5] = ypiPd[2];
    shift = shift/total;
    Double_t xpi = ppiPd[0] + shift;
    g2->SetParameters(1, xpi);
    for (Int_t l = 1; l < 5; l++) {
      if (tots[l] > 0) {
	frac[l] = tots[l]/total;
      }
      Double_t phi = TMath::ASin(TMath::Sqrt(frac[l]));
      g2->FixParameter(l+2, phi);
    }
  } else {// nP == 0 
    g2->SetParameters(1, proj->GetMean());
  }
  g2->SetParameter(2, 0.01);
  total = proj->Integral()*proj->GetBinWidth(5);
  g2->FixParameter(7,total);
  g2->FixParameter(8,-1);
  g2->FixParameter(9, 1);
}
//________________________________________________________________________________
TF1 *FitGF(TH1 *proj, Option_t *opt="") {
  // fit in momentum range p = 0.45 - 0.50 GeV/c
  if (! proj) return 0;
  TString Opt(opt);
  //  Bool_t quet = Opt.Contains("Q",TString::kIgnoreCase);
  TF1 *g2 = (TF1*) gROOT->GetFunction("GF");
  if (! g2) {
    g2 = new TF1("GF",gfFunc, -5, 5, 10);
    g2->SetParName(0,"norm"); g2->SetParLimits(0,-80,80);
    g2->SetParName(1,"mu");     g2->SetParLimits(1,-0.5,0.5);
    g2->SetParName(2,"Sigma");  g2->SetParLimits(2,0.05,0.8);
    g2->SetParName(3,"P");      g2->SetParLimits(3,0,1.5);
    g2->SetParName(4,"K");      g2->SetParLimits(4,0.0,0.5);
    g2->SetParName(5,"e");      g2->SetParLimits(5,0.0,0.5);
    g2->SetParName(6,"d");      g2->SetParLimits(6,0.0,0.5);
    g2->SetParName(7,"Total");
    g2->SetParName(8,"Case");
    g2->SetParName(9,"scale");  g2->FixParameter(9,1.);
    //    g2->SetParName(7,"factor"); g2->SetParLimits(7,-.1,0.1);
  }
  PreSetParameters(proj, g2);
  proj->Fit(g2,Opt.Data());
  g2->ReleaseParameter(3); g2->SetParLimits(3,0.0,1.5);
  g2->ReleaseParameter(4); g2->SetParLimits(4,0.0,0.5);
  g2->ReleaseParameter(5); g2->SetParLimits(5,0.0,0.5);
  g2->ReleaseParameter(6); g2->SetParLimits(6,0.0,0.5);
  //  g2->ReleaseParameter(9); g2->SetParLimits(9,0.5,2.0);
  Int_t iok = proj->Fit(g2,Opt.Data());
  if ( iok < 0) {
    cout << g2->GetName() << " fit has failed with " << iok << " for " 
	 << proj->GetName() << "/" << proj->GetTitle() << " Try one again" << endl; 
    proj->Fit(g2,Opt.Data());
  }
  Opt += "m";
  iok = proj->Fit(g2,Opt.Data());
  if (iok < 0 ) return 0;
  if (! Opt.Contains("q",TString::kIgnoreCase)) {
    Double_t params[10];
    g2->GetParameters(params);
    Double_t X = params[1];
    Double_t Y = TMath::Exp(params[0]);
    TPolyMarker *pm = new TPolyMarker(1, &X, &Y);
    proj->GetListOfFunctions()->Add(pm);
    pm->SetMarkerStyle(23);
    pm->SetMarkerColor(kRed);
    pm->SetMarkerSize(1.3);
    for (int i = 0; i <= 4; i++) {
      TF1 *f = new TF1(*g2);
      f->SetName(Peaks[i].Name);
      f->FixParameter(8,i);
      f->SetLineColor(i+2);
      proj->GetListOfFunctions()->Add(f);
    }
    proj->Draw();
  }
  return g2;
}
//________________________________________________________________________________
Double_t langaufun(Double_t *x, Double_t *par) {// $ROOTSYS/tutorials/fit/langaus.C

   //Fit parameters:
   //par[0]=Width (scale) parameter of Landau density
   //par[1]=Most Probable (MP, location) parameter of Landau density
   //par[2]=Total area (integral -inf to inf, normalization constant)
   //par[3]=Width (sigma) of convoluted Gaussian function
   //
   //In the Landau distribution (represented by the CERNLIB approximation), 
   //the maximum is located at x=-0.22278298 with the location parameter=0.
   //This shift is corrected within this function, so that the actual
   //maximum is identical to the MP parameter.

      // Numeric constants
      Double_t invsq2pi = 0.3989422804014;   // (2 pi)^(-1/2)
      Double_t mpshift  = -0.22278298;       // Landau maximum location

      // Control constants
      Double_t np = 100.0;      // number of convolution steps
      Double_t sc =   5.0;      // convolution extends to +-sc Gaussian sigmas

      // Variables
      Double_t xx, zz;
      Double_t mpc;
      Double_t fland;
      Double_t sum = 0.0;
      Double_t xlow,xupp;
      Double_t step;
      Double_t i;


      // MP shift correction
      mpc = TMath::Exp(par[1]) - mpshift * par[0]; 

      // Range of convolution integral
      xlow = x[0] - sc * par[3];
      xupp = x[0] + sc * par[3];

      step = (xupp-xlow) / np;
      // Convolution integral of Landau and Gaussian by sum
      for(i=1.0; i<=np/2; i++) {
         zz = xlow + (i-.5) * step;
	 xx = TMath::Exp(zz);
         fland = xx*TMath::Landau(xx,mpc,par[0]) / par[0];
         sum += fland * TMath::Gaus(x[0],zz,par[3]);

         zz = xupp - (i-.5) * step;
	 xx = TMath::Exp(zz);
         fland = xx*TMath::Landau(xx,mpc,par[0]) / par[0];
         sum += fland * TMath::Gaus(x[0],zz,par[3]);
      }

      return (par[2] * step * sum * invsq2pi / par[3]);
}

//________________________________________________________________________________
Double_t gfL5Func(Double_t *x, Double_t *par) {
  // par[0] - norm
  // par[1] - pion position wrt Z_pion (Bichsel prediction)
  // par[2] - sigma 
  // par[3] - proton signal
  // par[4] - Kaon    -"-
  // par[5] - electorn -"-
  // par[6] - deuteron -"-
  // par[7] - Total
  // par[8] - width of Landau
  Double_t sigma = par[2];
  Double_t frac[5];
  Int_t i;
  frac[0] = 1;
  for (i = 1; i < 5; i++) {
    frac[i] = TMath::Sin(par[2+i]);
    frac[i] *= frac[i];
    frac[0] -= frac[i];
  }
  if (frac[0] < 0.4) return 0;
  Double_t Value = 0;
  Int_t i1 = 0;
  Int_t i2 = 4;
  Int_t icase = (Int_t ) par[9];
  if (icase >= 0) {i1 = i2 = icase;}
  //                   WidthL        MP  Total, SigmaG
  Double_t parLI[4] = {  par[8], par[1],     1,   0.24};
  for (i = i1; i <= i2; i++) { 
    Double_t Sigma = TMath::Sqrt(sigma*sigma + Peaks[i].sigma*Peaks[i].sigma);
    parLI[1] = par[1] + Peaks[i].peak;
    parLI[3] = Sigma;
    Value += frac[i]*langaufun(x,parLI); 
    //    cout << "i\t" << i << "\tx = " << x[0] << " frac " << frac[i] << "\t" << Value << endl;
  }
  return par[7]*TMath::Exp(par[0])*Value;
}
//________________________________________________________________________________
TF1 *FitL5(TH1 *proj, Option_t *opt="", Int_t nhyps = 5) { // fit by 5 landau convoluted with gauss
  // fit in momentum range p = 0.45 - 0.50 GeV/c
  if (! proj) return 0;
  TString Opt(opt);
  //  Bool_t quet = Opt.Contains("Q",TString::kIgnoreCase);
  TF1 *g2 = (TF1*) gROOT->GetFunction("L5");
  if (! g2) {
    g2 = new TF1("L5",gfL5Func, -5, 5, 10);
    g2->SetParName(0,"norm"); g2->SetParLimits(0,-80,80);
    g2->SetParName(1,"mu");     //g2->SetParLimits(1,-1.5,1.5);
    g2->SetParName(2,"Sigma");  g2->SetParLimits(2,0.05,0.8);
    g2->SetParName(3,"P"); 
    g2->SetParName(4,"K");      g2->SetParLimits(4,0.0,0.5);
    g2->SetParName(5,"e");      g2->SetParLimits(5,0.0,0.5);
    g2->SetParName(6,"d");      g2->SetParLimits(6,0.0,0.5);
    g2->SetParName(7,"Total");
    g2->SetParName(8,"WidthL"); g2->SetParLimits(8,0.01,2.0);
    g2->SetParName(9,"case");  
    //    g2->SetParName(7,"factor"); g2->SetParLimits(7,-.1,0.1);
  }
  
  Double_t total = proj->Integral()*proj->GetBinWidth(5);
  g2->SetParameters(0, proj->GetMean(), proj->GetRMS(), 0.0, 0.0, 0.0, 0.0,0.0,0.5,-1);
  g2->FixParameter(3,0);
  g2->FixParameter(4,0);
  g2->FixParameter(5,0);
  g2->FixParameter(6,0);
  g2->FixParameter(7,total);
  g2->FixParameter(9,-1);
  Int_t iok = proj->Fit(g2,Opt.Data());
  if (nhyps == 5) {
    g2->ReleaseParameter(3); g2->SetParLimits(3,0.0,TMath::Pi()/2);
    g2->ReleaseParameter(4); g2->SetParLimits(4,0.0,0.3);
    g2->ReleaseParameter(5); g2->SetParLimits(5,0.0,0.3);
    g2->ReleaseParameter(6); g2->SetParLimits(6,0.0,0.3);
    iok = proj->Fit(g2,Opt.Data());
  }
  if ( iok < 0) {
    cout << g2->GetName() << " fit has failed with " << iok << " for " 
	 << proj->GetName() << "/" << proj->GetTitle() << " Try one again" << endl; 
    proj->Fit(g2,Opt.Data());
  }
  Opt += "m";
  iok = proj->Fit(g2,Opt.Data());
  if (! Opt.Contains("q",TString::kIgnoreCase)) {
    Double_t params[10];
    g2->GetParameters(params);
    Double_t X = params[1];
    Double_t Y = TMath::Exp(params[0]);
    TPolyMarker *pm = new TPolyMarker(1, &X, &Y);
    proj->GetListOfFunctions()->Add(pm);
    pm->SetMarkerStyle(23);
    pm->SetMarkerColor(kRed);
    pm->SetMarkerSize(1.3);
    if (nhyps == 5) {
      for (int i = 0; i < nhyps; i++) {
	TF1 *f = new TF1(*g2);
	f->SetName(Form("L5%s",Peaks[i].Name));
	f->FixParameter(9,i);
	f->SetLineColor(i+2);
	proj->GetListOfFunctions()->Add(f);
      }
      proj->Draw();
    }
  }
  return g2;
}
//________________________________________________________________________________
Double_t gbFunc(Double_t *x, Double_t *par) {
  // par[0] - norm
  // par[1] - pion position wrt Z_pion (Bichsel prediction)
  // par[2] - sigma 
  // par[3] - proton signal
  // par[4] - Kaon    -"-
  // par[5] - electorn -"-
  // par[6] - deuteron -"-
  // par[7] - Total
  // par[8] - <dX>
  if (! LandauF) Landau();
  static Double_t sigma_p[3] = {// sigma versus ::log(dX)
    5.31393e-01,//    1  p0  1.33485e-03   7.13072e-07   7.08416e-08
    -1.43277e-01,//    2  p1  3.36846e-03   6.62434e-07  -1.13681e-05
    2.43800e-02 //    3  p2  1.81240e-03   4.02492e-07  -2.08423e-05
  };
  Double_t sigmaC = par[2];
  Double_t frac[5];
  Int_t i;
  frac[0] = 1;
  for (i = 1; i < 5; i++) {
    frac[i] = TMath::Sin(par[2+i]);
    frac[i] *= frac[i];
    frac[0] -= frac[i];
  }
  Double_t Value = 0;
  static Double_t pMom = 0.475;
  static Double_t Xlog10bg[5];
  Double_t Ylog2dx = TMath::Log2(par[8]);
  Double_t Sigma = sigma_p[2];
  for (int n = 1; n>=0; n--) Sigma = Ylog2dx*Sigma + sigma_p[n];
  Double_t zMostProb[5];
  for (i = 0; i < 5; i++) {
    Xlog10bg[i] = TMath::Log10(pMom/Peaks[i].mass);
    zMostProb[i] = gBichsel->GetMostProbableZ(Xlog10bg[i],Ylog2dx);
    Double_t sigma     = Sigma + sigmaC;
    //    Double_t xi = (x[0] + zMostProb[0] - zMostProb[i])/sigma;
    //    Double_t xi = (x[0] + par[1] + zMostProb[0] - zMostProb[i])/sigma;
    Double_t xi = (x[0] - par[1] - Peaks[i].peak)/sigma;
    Double_t Prob = LandauF->Eval(xi);
    
    Value += frac[i]*Prob;
    //    cout << "i\t" << i << "\tx = " << x[0] << " frac " << frac[i] << "\t" << Value << endl;
  }
  return par[7]*TMath::Exp(par[0])*Value;
}
//________________________________________________________________________________
TF1 *FitGB(TH1 *proj, Option_t *opt="", Double_t dX = 2.364) {
  if (!gBichsel) {
    gSystem->Load("StBichsel");
    gBichsel = Bichsel::Instance();
  }
  // fit in momentum range p = 0.45 - 0.50 GeV/c
  if (! proj) return 0;
  TString Opt(opt);
  //  Bool_t quet = Opt.Contains("Q",TString::kIgnoreCase);
  TF1 *g2 = (TF1*) gROOT->GetFunction("GB");
  if (! g2) {
    g2 = new TF1("GB",gbFunc, -5, 5, 9);
    g2->SetParName(0,"norm"); 
    g2->SetParName(1,"mu");     g2->SetParLimits(1,-1.5,1.5);
    g2->SetParName(2,"Sigma");  g2->SetParLimits(2,0.0,0.8);
    g2->SetParName(3,"P"); 
    g2->SetParName(4,"K"); 
    g2->SetParName(5,"e"); 
    g2->SetParName(6,"d");
    g2->SetParName(7,"Total");
    g2->SetParName(8,"dX");
    //    g2->SetParName(7,"factor"); g2->SetParLimits(7,-.1,0.1);
  }
  
  Double_t total = proj->Integral()*proj->GetBinWidth(5);
  g2->SetParameters(0, 1e-3, 0.01, 0.4, 0., 0., 0.,0.);
  g2->FixParameter(7,total);
  g2->FixParameter(8,dX);
  Int_t iok = proj->Fit(g2,Opt.Data());
  if ( iok < 0 ) {
    cout << g2->GetName() << " fit has failed with " << iok << " for " 
	 << proj->GetName() << "/" << proj->GetTitle() << " Try one again" << endl; 
    proj->Fit(g2,Opt.Data());
  }
  Opt += "m";
  iok = proj->Fit(g2,Opt.Data());
  if (! Opt.Contains("q",TString::kIgnoreCase)) {
    Double_t params[10];
    g2->GetParameters(params);
    Double_t X = params[1];
    Double_t Y = TMath::Exp(params[0]);
    TPolyMarker *pm = new TPolyMarker(1, &X, &Y);
    proj->GetListOfFunctions()->Add(pm);
    pm->SetMarkerStyle(23);
    pm->SetMarkerColor(kRed);
    pm->SetMarkerSize(1.3);
    proj->Draw();
  }
  return g2;
}
#if ROOT_VERSION_CODE <= ROOT_VERSION(5,34,39)
// namespace ROOT {
// namespace Math {

#define MATH_ERROR_MSG(loc,str)                   \
  std::cout << "Error in ROOT::Math::" << loc << ">: " << str	\
       << std::endl;
#define MATH_WARN_MSG(loc,str)                   \
  std::cout << "Warning in ROOT::Math::" << loc << ">: " << str	\
       << std::endl;
/*
  Crystal ball function

  See the definition at
  <A HREF="http://en.wikipedia.org/wiki/Crystal_Ball_function">
  Wikipedia</A>.

  It is not really a pdf since it is not normalized

  @ingroup PdfFunc

  */

  double crystalball_function(double x, double alpha, double n, double sigma, double mean = 0) {
     // Inlined to enable clad-auto-derivation for this function.

     // evaluate the crystal ball function
     if (sigma < 0.)     return 0.;
     double z = (x - mean)/sigma;
     if (alpha < 0) z = -z;
     double abs_alpha = std::abs(alpha);
     // double C = n/abs_alpha * 1./(n-1.) * std::exp(-alpha*alpha/2.);
     // double D = std::sqrt(M_PI/2.)*(1.+ROOT::Math::erf(abs_alpha/std::sqrt(2.)));
     // double N = 1./(sigma*(C+D));
     if (z  > - abs_alpha)
        return std::exp(- 0.5 * z * z);
     //double A = std::pow(n/abs_alpha,n) * std::exp(-0.5*abs_alpha*abs_alpha);
     double nDivAlpha = n/abs_alpha;
     double AA =  std::exp(-0.5*abs_alpha*abs_alpha);
     double B = nDivAlpha -abs_alpha;
     double arg = nDivAlpha/(B-z);
     return AA * std::pow(arg,n);
   }

   /**
       pdf definition of the crystal_ball which is defined only for n > 1 otherwise integral is diverging
    */
   double crystalball_pdf(double x, double alpha, double n, double sigma, double mean = 0) {
    // Inlined to enable clad-auto-derivation for this function.

    // evaluation of the PDF ( is defined only for n >1)
    if (sigma < 0.)     return 0.;
    if ( n <= 1) return std::numeric_limits<double>::quiet_NaN();  // pdf is not normalized for n <=1
    double abs_alpha = std::abs(alpha);
    double C = n/abs_alpha * 1./(n-1.) * std::exp(-alpha*alpha/2.);
    double D = std::sqrt(M_PI/2.)*(1.+TMath::Erf(abs_alpha/std::sqrt(2.)));
    double N = 1./(sigma*(C+D));
    return N * crystalball_function(x,alpha,n,sigma,mean);
   }
   double crystalball_integral(double x, double alpha, double n, double sigma, double mean)
   {
      // compute the integral of the crystal ball function (ROOT::Math::crystalball_function)
      // If alpha > 0 the integral is the right tail integral.
      // If alpha < 0 is the left tail integrals which are always finite for finite x.     
      // parameters:
      // alpha : is non equal to zero, define the # of sigma from which it becomes a power-law function (from mean-alpha*sigma)
      // n > 1 : is integrer, is the poxswer of the low  tail
      // add a value xmin for cases when n <=1 the integral diverges 
      if (sigma == 0)   return 0;
      if (alpha==0)
      {
         MATH_ERROR_MSG("crystalball_integral","CrystalBall function not defined at alpha=0");
         return 0.;
      }
      bool useLog = (n == 1.0); 
      if (n<=0)   MATH_WARN_MSG("crystalball_integral","No physical meaning when n<=0");

      double z = (x-mean)/sigma;
      if (alpha < 0 ) z = -z;
      
      double abs_alpha = std::abs(alpha);
      
      //double D = *(1.+TMath::Erf(abs_alpha/std::sqrt(2.)));
      //double N = 1./(sigma*(C+D));
      double intgaus = 0.;
      double intpow  = 0.;
 
      const double sqrtpiover2 = std::sqrt(M_PI/2.);
      const double sqrt2pi = std::sqrt( 2.*M_PI); 
      const double oneoversqrt2 = 1./sqrt(2.);
      if (z <= -abs_alpha)
      {
         double A = std::pow(n/abs_alpha,n) * std::exp(-0.5 * alpha*alpha);
         double B = n/abs_alpha - abs_alpha;

         if (!useLog) {
            double C = (n/abs_alpha) * (1./(n-1)) * std::exp(-alpha*alpha/2.);
            intpow  = C - A /(n-1.) * std::pow(B-z,-n+1) ;
         }
         else {
            // for n=1 the primitive of 1/x is log(x)
            intpow = -A * std::log( n / abs_alpha ) + A * std::log( B -z );
         }
         intgaus =  sqrtpiover2*(1.+TMath::Erf(abs_alpha*oneoversqrt2));
      }
      else
      {
         intgaus = ROOT::Math::gaussian_cdf_c(z, 1);
         intgaus *= sqrt2pi;
         intpow  =  0;  
      }
      return sigma * (intgaus + intpow);
   }
   double crystalball_cdf(double x, double alpha, double n, double sigma, double mean )
   {
      if (n <= 1.) {
         MATH_ERROR_MSG("crystalball_cdf","CrystalBall cdf not defined for n <=1");
         return std::numeric_limits<double>::quiet_NaN();
      }

      double abs_alpha = std::abs(alpha);
      double C = n/abs_alpha * 1./(n-1.) * std::exp(-alpha*alpha/2.);
      double D = std::sqrt(M_PI/2.)*(1.+TMath::Erf(abs_alpha/std::sqrt(2.)));
      double totIntegral = sigma*(C+D);

      double integral = crystalball_integral(x,alpha,n,sigma,mean); 
      return (alpha > 0) ? 1. - integral/totIntegral : integral/totIntegral; 
   }
   double crystalball_cdf_c(double x, double alpha, double n, double sigma, double mean )
   {
      if (n <= 1.) {
         MATH_ERROR_MSG("crystalball_cdf_c","CrystalBall cdf not defined for n <=1");
         return std::numeric_limits<double>::quiet_NaN();
      }
      double abs_alpha = std::abs(alpha);
      double C = n/abs_alpha * 1./(n-1.) * std::exp(-alpha*alpha/2.);
      double D = std::sqrt(M_PI/2.)*(1.+TMath::Erf(abs_alpha/std::sqrt(2.)));
      double totIntegral = sigma*(C+D);

      double integral = crystalball_integral(x,alpha,n,sigma,mean);      
      return (alpha > 0) ? integral/totIntegral : 1. - (integral/totIntegral); 
   }
// } // namespace Math
// } // namespace ROOT
#endif /* end of export from ROOT6 */
//________________________________________________________________________________
Double_t crystalBall(Double_t *x, Double_t *p) {
  Double_t normL = p[0];
  Double_t mu    = p[1];
  Double_t sigma = p[2];
  Double_t n     = p[3];
  Double_t alpha = p[4];
  //  return TMath::Exp(normL)*crystalball_function(x[0], alpha, n, sigma, mu);
  return TMath::Exp(normL)*crystalball_pdf(x[0], alpha, n, sigma, mu);
}
//________________________________________________________________________________
TF1 *CrystalBall() {
  static TF1 *f = 0;
  if (! f) {
    f = new TF1("CrystalBall",crystalBall,-5,5,5);
    f->SetParNames("norl","mu","sigma","n","alpha");
    f->SetParLimits(3,1,10);
  }
  f->SetParameters(0,0.0,1,2,-10);
  //  f->SetParLimits(1,0.6,1.0);
  //  f->SetParLimits(3,-0.1,4.0);
  return f;
}
#if 0
//--------------------------------------------------------------------------------
Double_t gausexp(Double_t *x, Double_t *p) {
  // Souvik Das, "A simple alternative to the Crystal Ball function"
  // https://arxiv.org/pdf/1603.08591.pdf
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
#endif
//________________________________________________________________________________
TF1 *GausExp() {
  static TF1 *f = 0;
  if (! f) {
    f = new TF1("GausExp",StdEdxModel::gausexp,-15,15,5);
    f->SetLineColor(2);
    f->SetParNames("norl","mu","sigma","k","l");
    f->SetParLimits(1,2.,5.);
    f->SetParLimits(2,0.05,1.0);
    f->SetParLimits(3,0.01,2.0);
    f->SetParameters(0.0,0.0,1,2,0);
  }
  f->FixParameter(4,0);
  //  f->SetParLimits(1,0.6,1.0);
  //  f->SetParLimits(3,-0.1,4.0);
  return f;
}

#if 0
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
#endif
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
#if 0
  return ggauso(x, par);
#else
  return StdEdxModel::instance()->gausw(x, par);
#endif
}
//________________________________________________________________________________
TF1 *GG() {
  static TF1 *f = 0;
  if (! f) {
    f = new TF1("GG",ggaus,-5,5,4);
    f->SetParNames("norl","mu","sigma","alpha");
  }
  f->SetNpx(1000);
  f->SetParameters(0,0.8,1,1);
  f->SetParLimits(3,-10,10);
  //  f->SetParLimits(1,0.6,1.0);
  //  f->SetParLimits(3,-0.1,4.0);
  return f;
}
//________________________________________________________________________________
Double_t ggaus2(Double_t *x, Double_t *p) {
  Double_t X = x[0];
  Double_t NormL = p[0];
  Double_t mu    = p[1]; // Mode = ksi + w *m_0(alpha); most propable value
  Double_t sigma = p[2]; // Sqrt(Variance);
  Double_t alpha = p[3];
  Double_t dnormL = p[4];
  Double_t dmu   = p[5];
  Double_t dsigma = p[6];
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
  Double_t Value =  StdEdxModel::instance()->gausw(x, par);
  if (dnormL > -9) {
    Value += TMath::Exp(dnormL)*TMath::Gaus(X, mu + dmu, dsigma, kTRUE);
  }
  return Value;
}
//________________________________________________________________________________
TF1 *GG2() {
  static TF1 *f = 0;
  if (! f) {
    f = new TF1("GG2",ggaus2,-5,5,7);
    f->SetParNames("norl","mu","sigma","alpha","dnorll","dmu","dsigma");
  }
  f->SetParameters(0,0.8,1,1);
  //  f->SetParLimits(1,0.6,1.0);
  //  f->SetParLimits(3,-0.1,4.0);
  return f;
}
//________________________________________________________________________________
TF1 *FitGG(TH1 *proj, Option_t *opt="RQ") {
  if (! proj) return 0;
  Double_t params[9];
  proj->Fit("gaus");
  TF1 *gaus = (TF1 *) proj->GetListOfFunctions()->FindObject("gaus");
  gaus->GetParameters(params);
  params[0] = TMath::Log(params[0]);
  params[3] = 10;
  params[4] = 0;
  TF1 *g = GG();
  g->SetParameters(params);
  proj->Fit(g,opt);
  return g;
}
//________________________________________________________________________________
TF1 *FitCB(TH1 *proj, Option_t *opt="RQ") {
  if (! proj) return 0;
  Double_t params[9];
  TF1 *g = CrystalBall();
  proj->Fit(g,opt);
  return g;
}
#if 0
//________________________________________________________________________________
enum EMDFPolyType {
  kMonomials,
  kChebyshev,
  kLegendre
};
class MDFCorrection {
public:
  UChar_t idx; 
  UChar_t nrows; 
  UChar_t PolyType; 
  UChar_t NVariables; 
  UChar_t NCoefficients; 
  UChar_t Power[100]; 
  Double_t DMean; 
  Double_t XMin[2]; 
  Double_t XMax[2]; 
  Double_t Coefficients[50]; 
  Double_t CoefficientsRMS[50]; 
  Double_t MDFunc(Double_t *x);
  Double_t EvalFactor(Int_t p, Double_t x) const;
};
//____________________________________________________________________
Double_t MDFCorrection::EvalFactor(Int_t p, Double_t x) const {
  // Evaluate function with power p at variable value x
  Int_t    i   = 0;
  Double_t p1  = 1;
  Double_t p2  = 0;
  Double_t p3  = 0;
  Double_t r   = 0;

  switch(p) {
  case 1:
    r = 1;
    break;
  case 2:
    r =  x;
    break;
  default:
    p2 = x;
    for (i = 3; i <= p; i++) {
      p3 = p2 * x;
      if (PolyType == kLegendre)
	p3 = ((2 * i - 3) * p2 * x - (i - 2) * p1) / (i - 1);
      else if (PolyType == kChebyshev)
	p3 = 2 * x * p2 - p1;
      p1 = p2;
      p2 = p3;
    }
    r = p3;
  }
  return r;
}
//________________________________________________________________________________
Double_t MDFCorrection::MDFunc(Double_t *x) {
  Double_t returnValue = DMean;
  Double_t term        = 0;
  UChar_t    i, j;
  for (i = 0; i < NCoefficients; i++) {
    // Evaluate the ith term in the expansion
    term = Coefficients[i];
    for (j = 0; j < NVariables; j++) {
      // Evaluate the factor (polynomial) in the j-th variable.
      Int_t    p  =  Power[i * NVariables + j];
      Double_t y  =  1 + 2. / (XMax[j] - XMin[j])
	* (x[j] - XMax[j]);
      term        *= EvalFactor(p,y);
    }
    // Add this term to the final result
    returnValue += term;
  }
  return returnValue;
}
TMultiDimFit* fit = 0;
MDFCorrection **rows = 0;
//________________________________________________________________________________
MDFCorrection *MDFcor(Int_t i = 0) {
  if (! rows) {
    rows = new MDFCorrection*[3];
    Int_t k = 0;
    rows[k] = new MDFCorrection;
    rows[k]->nrows = 3;
    rows[k]->idx = 1;
    // mu
    rows[k]->PolyType =        0;
    rows[k]->NVariables =      2;
    rows[k]->NCoefficients =   22;
    rows[k]->XMin[ 0] =       3.25;  rows[k]->XMin[ 1] =     1.5272;
    rows[k]->XMax[ 0] =      10.35;  rows[k]->XMax[ 1] =     2.6258;
    rows[k]->Power[ 0] =  1;  rows[k]->Power[ 1] =  1;
    rows[k]->Power[ 2] =  1;  rows[k]->Power[ 3] =  2;
    rows[k]->Power[ 4] =  1;  rows[k]->Power[ 5] =  3;
    rows[k]->Power[ 6] =  2;  rows[k]->Power[ 7] =  1;
    rows[k]->Power[ 8] =  2;  rows[k]->Power[ 9] =  2;
    rows[k]->Power[10] =  1;  rows[k]->Power[11] =  5;
    rows[k]->Power[12] =  3;  rows[k]->Power[13] =  1;
    rows[k]->Power[14] =  2;  rows[k]->Power[15] =  3;
    rows[k]->Power[16] =  2;  rows[k]->Power[17] =  4;
    rows[k]->Power[18] =  3;  rows[k]->Power[19] =  3;
    rows[k]->Power[20] =  2;  rows[k]->Power[21] =  5;
    rows[k]->Power[22] =  3;  rows[k]->Power[23] =  2;
    rows[k]->Power[24] =  1;  rows[k]->Power[25] =  6;
    rows[k]->Power[26] =  3;  rows[k]->Power[27] =  4;
    rows[k]->Power[28] =  5;  rows[k]->Power[29] =  6;
    rows[k]->Power[30] =  4;  rows[k]->Power[31] =  1;
    rows[k]->Power[32] =  4;  rows[k]->Power[33] =  2;
    rows[k]->Power[34] =  4;  rows[k]->Power[35] =  6;
    rows[k]->Power[36] =  4;  rows[k]->Power[37] =  3;
    rows[k]->Power[38] =  5;  rows[k]->Power[39] =  3;
    rows[k]->Power[40] =  5;  rows[k]->Power[41] =  5;
    rows[k]->Power[42] =  6;  rows[k]->Power[43] =  5;
    rows[k]->DMean =   0.5915;
    rows[k]->Coefficients[ 0]    =         0.12142;  rows[k]->Coefficients[ 1]    =           0.518;
    rows[k]->Coefficients[ 2]    =        -0.69565;  rows[k]->Coefficients[ 3]    =        0.035468;
    rows[k]->Coefficients[ 4]    =         0.24901;  rows[k]->Coefficients[ 5]    =         0.32729;
    rows[k]->Coefficients[ 6]    =       -0.082096;  rows[k]->Coefficients[ 7]    =          0.2748;
    rows[k]->Coefficients[ 8]    =        -0.15529;  rows[k]->Coefficients[ 9]    =        0.053036;
    rows[k]->Coefficients[10]    =        -0.18635;  rows[k]->Coefficients[11]    =        -0.21226;
    rows[k]->Coefficients[12]    =       -0.095869;  rows[k]->Coefficients[13]    =         0.46573;
    rows[k]->Coefficients[14]    =        -0.26165;  rows[k]->Coefficients[15]    =        0.039502;
    rows[k]->Coefficients[16]    =       -0.057019;  rows[k]->Coefficients[17]    =        0.080737;
    rows[k]->Coefficients[18]    =        -0.10641;  rows[k]->Coefficients[19]    =        0.072822;
    rows[k]->Coefficients[20]    =       -0.057196;  rows[k]->Coefficients[21]    =        0.072271;
    rows[k]->CoefficientsRMS[ 0] =      2.7331e-05;  rows[k]->CoefficientsRMS[ 1] =      6.8688e-05;
    rows[k]->CoefficientsRMS[ 2] =      0.00017815;  rows[k]->CoefficientsRMS[ 3] =      6.4463e-05;
    rows[k]->CoefficientsRMS[ 4] =      0.00017854;  rows[k]->CoefficientsRMS[ 5] =      0.00019005;
    rows[k]->CoefficientsRMS[ 6] =      0.00010505;  rows[k]->CoefficientsRMS[ 7] =      0.00035043;
    rows[k]->CoefficientsRMS[ 8] =      0.00030078;  rows[k]->CoefficientsRMS[ 9] =      0.00040523;
    rows[k]->CoefficientsRMS[10] =       0.0002739;  rows[k]->CoefficientsRMS[11] =      0.00023002;
    rows[k]->CoefficientsRMS[12] =       9.581e-05;  rows[k]->CoefficientsRMS[13] =      0.00037056;
    rows[k]->CoefficientsRMS[14] =      0.00044255;  rows[k]->CoefficientsRMS[15] =      9.5917e-05;
    rows[k]->CoefficientsRMS[16] =      0.00019139;  rows[k]->CoefficientsRMS[17] =      0.00038451;
    rows[k]->CoefficientsRMS[18] =      0.00043657;  rows[k]->CoefficientsRMS[19] =      0.00037608;
    rows[k]->CoefficientsRMS[20] =      0.00048424;  rows[k]->CoefficientsRMS[21] =      0.00036469;
    k = 1;
    rows[k] = new MDFCorrection;
    rows[k]->nrows = 3;
    rows[k]->idx = 2;
    // sigma
    rows[k]->PolyType =        0;
    rows[k]->NVariables =      2;
    rows[k]->NCoefficients =   21;
    rows[k]->XMin[ 0] =       3.25;  rows[k]->XMin[ 1] =     1.5272;
    rows[k]->XMax[ 0] =      10.35;  rows[k]->XMax[ 1] =     2.6258;
    rows[k]->Power[ 0] =  1;  rows[k]->Power[ 1] =  1;
    rows[k]->Power[ 2] =  2;  rows[k]->Power[ 3] =  1;
    rows[k]->Power[ 4] =  1;  rows[k]->Power[ 5] =  2;
    rows[k]->Power[ 6] =  3;  rows[k]->Power[ 7] =  1;
    rows[k]->Power[ 8] =  2;  rows[k]->Power[ 9] =  2;
    rows[k]->Power[10] =  2;  rows[k]->Power[11] =  3;
    rows[k]->Power[12] =  4;  rows[k]->Power[13] =  1;
    rows[k]->Power[14] =  1;  rows[k]->Power[15] =  3;
    rows[k]->Power[16] =  2;  rows[k]->Power[17] =  5;
    rows[k]->Power[18] =  5;  rows[k]->Power[19] =  2;
    rows[k]->Power[20] =  1;  rows[k]->Power[21] =  4;
    rows[k]->Power[22] =  1;  rows[k]->Power[23] =  5;
    rows[k]->Power[24] =  5;  rows[k]->Power[25] =  1;
    rows[k]->Power[26] =  3;  rows[k]->Power[27] =  4;
    rows[k]->Power[28] =  5;  rows[k]->Power[29] =  3;
    rows[k]->Power[30] =  6;  rows[k]->Power[31] =  2;
    rows[k]->Power[32] =  3;  rows[k]->Power[33] =  5;
    rows[k]->Power[34] =  4;  rows[k]->Power[35] =  5;
    rows[k]->Power[36] =  5;  rows[k]->Power[37] =  4;
    rows[k]->Power[38] =  6;  rows[k]->Power[39] =  6;
    rows[k]->Power[40] =  2;  rows[k]->Power[41] =  4;
    rows[k]->DMean =   0.1415;
    rows[k]->Coefficients[ 0]    =       -0.038573;  rows[k]->Coefficients[ 1]    =        -0.15368;
    rows[k]->Coefficients[ 2]    =        0.078618;  rows[k]->Coefficients[ 3]    =        0.076764;
    rows[k]->Coefficients[ 4]    =       -0.021717;  rows[k]->Coefficients[ 5]    =         0.18198;
    rows[k]->Coefficients[ 6]    =       -0.080556;  rows[k]->Coefficients[ 7]    =       -0.041054;
    rows[k]->Coefficients[ 8]    =        -0.12834;  rows[k]->Coefficients[ 9]    =      -0.0067788;
    rows[k]->Coefficients[10]    =       -0.040617;  rows[k]->Coefficients[11]    =        0.015402;
    rows[k]->Coefficients[12]    =         0.07045;  rows[k]->Coefficients[13]    =        0.026555;
    rows[k]->Coefficients[14]    =       -0.074055;  rows[k]->Coefficients[15]    =       -0.034786;
    rows[k]->Coefficients[16]    =        0.041479;  rows[k]->Coefficients[17]    =        0.023544;
    rows[k]->Coefficients[18]    =        0.031308;  rows[k]->Coefficients[19]    =        0.017737;
    rows[k]->Coefficients[20]    =       -0.011397;
    rows[k]->CoefficientsRMS[ 0] =      1.9296e-05;  rows[k]->CoefficientsRMS[ 1] =      5.7026e-05;
    rows[k]->CoefficientsRMS[ 2] =      6.0565e-05;  rows[k]->CoefficientsRMS[ 3] =      7.3436e-05;
    rows[k]->CoefficientsRMS[ 4] =      0.00010982;  rows[k]->CoefficientsRMS[ 5] =      0.00020313;
    rows[k]->CoefficientsRMS[ 6] =      0.00011317;  rows[k]->CoefficientsRMS[ 7] =       0.0001075;
    rows[k]->CoefficientsRMS[ 8] =      0.00026135;  rows[k]->CoefficientsRMS[ 9] =      0.00019875;
    rows[k]->CoefficientsRMS[10] =      9.2522e-05;  rows[k]->CoefficientsRMS[11] =      0.00012114;
    rows[k]->CoefficientsRMS[12] =        0.000102;  rows[k]->CoefficientsRMS[13] =      0.00027102;
    rows[k]->CoefficientsRMS[14] =      0.00017262;  rows[k]->CoefficientsRMS[15] =      0.00015424;
    rows[k]->CoefficientsRMS[16] =      0.00021758;  rows[k]->CoefficientsRMS[17] =      0.00021629;
    rows[k]->CoefficientsRMS[18] =      0.00030315;  rows[k]->CoefficientsRMS[19] =      0.00015055;
    rows[k]->CoefficientsRMS[20] =       0.0001791;
    k = 2;
    rows[k] = new MDFCorrection;
    rows[k]->nrows = 3;
    rows[k]->idx = 3;
    // 1/a0
    rows[k]->PolyType =        0;
    rows[k]->NVariables =      2;
    rows[k]->NCoefficients =   21;
    rows[k]->XMin[ 0] =       3.25;  rows[k]->XMin[ 1] =     1.5272;
    rows[k]->XMax[ 0] =      10.35;  rows[k]->XMax[ 1] =     2.6258;
    rows[k]->Power[ 0] =  1;  rows[k]->Power[ 1] =  1;
    rows[k]->Power[ 2] =  1;  rows[k]->Power[ 3] =  2;
    rows[k]->Power[ 4] =  1;  rows[k]->Power[ 5] =  3;
    rows[k]->Power[ 6] =  3;  rows[k]->Power[ 7] =  1;
    rows[k]->Power[ 8] =  2;  rows[k]->Power[ 9] =  2;
    rows[k]->Power[10] =  2;  rows[k]->Power[11] =  3;
    rows[k]->Power[12] =  4;  rows[k]->Power[13] =  1;
    rows[k]->Power[14] =  3;  rows[k]->Power[15] =  2;
    rows[k]->Power[16] =  1;  rows[k]->Power[17] =  5;
    rows[k]->Power[18] =  2;  rows[k]->Power[19] =  4;
    rows[k]->Power[20] =  3;  rows[k]->Power[21] =  3;
    rows[k]->Power[22] =  4;  rows[k]->Power[23] =  6;
    rows[k]->Power[24] =  2;  rows[k]->Power[25] =  1;
    rows[k]->Power[26] =  1;  rows[k]->Power[27] =  4;
    rows[k]->Power[28] =  4;  rows[k]->Power[29] =  2;
    rows[k]->Power[30] =  4;  rows[k]->Power[31] =  3;
    rows[k]->Power[32] =  5;  rows[k]->Power[33] =  5;
    rows[k]->Power[34] =  3;  rows[k]->Power[35] =  4;
    rows[k]->Power[36] =  4;  rows[k]->Power[37] =  5;
    rows[k]->Power[38] =  5;  rows[k]->Power[39] =  1;
    rows[k]->Power[40] =  5;  rows[k]->Power[41] =  6;
    rows[k]->DMean =   0.6806;
    rows[k]->Coefficients[ 0]    =        -0.50957;  rows[k]->Coefficients[ 1]    =          1.3324;
    rows[k]->Coefficients[ 2]    =          2.2306;  rows[k]->Coefficients[ 3]    =        0.018721;
    rows[k]->Coefficients[ 4]    =         -1.9494;  rows[k]->Coefficients[ 5]    =          1.4649;
    rows[k]->Coefficients[ 6]    =         0.46458;  rows[k]->Coefficients[ 7]    =        -0.16929;
    rows[k]->Coefficients[ 8]    =         -1.2244;  rows[k]->Coefficients[ 9]    =          2.6239;
    rows[k]->Coefficients[10]    =         -2.0547;  rows[k]->Coefficients[11]    =         -1.8734;
    rows[k]->Coefficients[12]    =        -0.47307;  rows[k]->Coefficients[13]    =        -0.18053;
    rows[k]->Coefficients[14]    =         0.74014;  rows[k]->Coefficients[15]    =         -1.0493;
    rows[k]->Coefficients[16]    =           1.299;  rows[k]->Coefficients[17]    =        -0.61577;
    rows[k]->Coefficients[18]    =        -0.32173;  rows[k]->Coefficients[19]    =       -0.088406;
    rows[k]->Coefficients[20]    =         0.22538;
    rows[k]->CoefficientsRMS[ 0] =      9.0093e-05;  rows[k]->CoefficientsRMS[ 1] =      0.00054122;
    rows[k]->CoefficientsRMS[ 2] =      0.00098388;  rows[k]->CoefficientsRMS[ 3] =       0.0005543;
    rows[k]->CoefficientsRMS[ 4] =      0.00097002;  rows[k]->CoefficientsRMS[ 5] =       0.0018305;
    rows[k]->CoefficientsRMS[ 6] =      0.00052539;  rows[k]->CoefficientsRMS[ 7] =       0.0014684;
    rows[k]->CoefficientsRMS[ 8] =       0.0010563;  rows[k]->CoefficientsRMS[ 9] =       0.0019343;
    rows[k]->CoefficientsRMS[10] =        0.002814;  rows[k]->CoefficientsRMS[11] =       0.0030169;
    rows[k]->CoefficientsRMS[12] =      0.00026871;  rows[k]->CoefficientsRMS[13] =       0.0011874;
    rows[k]->CoefficientsRMS[14] =       0.0016735;  rows[k]->CoefficientsRMS[15] =       0.0029385;
    rows[k]->CoefficientsRMS[16] =       0.0035866;  rows[k]->CoefficientsRMS[17] =        0.003569;
    rows[k]->CoefficientsRMS[18] =       0.0033066;  rows[k]->CoefficientsRMS[19] =      0.00073237;
    rows[k]->CoefficientsRMS[20] =       0.0037895;
  }
  return rows[i];
}
#endif
#include "GEXNor.C"
//________________________________________________________________________________
TF1 *FitGEX3(TH1 *proj, Option_t *opt="RQM", Fitx_t *Fit = 0) {
  if (! proj) return 0;
  TF1 *g = new TF1("GausExp",StdEdxModel::gausexp,-2,5,5);
  g->SetNpx(500);
  g->SetLineColor(2);
  g->SetParNames("norl","mu","sigma","k","l");
  g->SetParLimits(0,0.,20.);
  g->SetParLimits(1,-5.,5.);
  g->SetParLimits(2,0.001,5.0);
  g->SetParameters(0.0,0.0,1,2,0);
  g->FixParameter(4,0);
  Double_t entries, mean, rms;
  if (Fit) {
    entries = Fit->entries;
    mean = Fit->mean;
    rms = Fit->rms;
  } else {
    entries = proj->Integral();
    mean = proj->GetMean();
    rms  = proj->GetRMS();
  }
  if (entries < 100) return 0;
  g->SetParameter(0, TMath::Log(entries));
  g->SetParameter(1, mean);
  g->SetParameter(2, rms);
  g->FixParameter(3,10.0);
  if (Fit) {
    g->FixParameter(1, muPar(Fit->x));
    g->FixParameter(2, sigmaPar(Fit->x));
    g->FixParameter(3, a0Par(Fit->x));
  }
  proj->Fit(g,opt);
  return g;
}
//________________________________________________________________________________
TF1 *FitGEX2(TH1 *proj, Option_t *opt="RQM", Fitx_t *Fit = 0) {
  if (! proj) return 0;
  TF1 *g = new TF1("GausExp",StdEdxModel::gausexp,-2,5,5);
  g->SetNpx(500);
  g->SetLineColor(2);
  g->SetParNames("norl","mu","sigma","k","l");
  g->SetParLimits(0,0.,20.);
  g->SetParLimits(1,-5.,5.);
  g->SetParLimits(2,0.001,5.0);
  g->SetParameters(0.0,0.0,1,2,0);
  g->FixParameter(4,0);
  Double_t entries, mean, rms;
  if (Fit) {
    entries = Fit->entries;
    mean = Fit->mean;
    rms = Fit->rms;
  } else {
    entries = proj->Integral();
    mean = proj->GetMean();
    rms  = proj->GetRMS();
  }
  if (entries < 100) return 0;
  g->SetParameter(0, TMath::Log(entries));
  g->SetParameter(1, mean);
  g->SetParameter(2, rms);
  g->FixParameter(3,10.0);
  if (Fit) {
    g->SetParameter(1, muPar(Fit->x));
    g->SetParLimits(1, 0.8*g->GetParameter(1), 1.2*g->GetParameter(1));
    g->FixParameter(2, sigmaPar(Fit->x));
    g->FixParameter(3, a0Par(Fit->x));
  }
  proj->Fit(g,opt);
  return g;
}
//________________________________________________________________________________
TF1 *FitGEX1(TH1 *proj, Option_t *opt="RQM", Fitx_t *Fit = 0) {
  if (! proj) return 0;
  TF1 *g = new TF1("GausExp",StdEdxModel::gausexp,-2,5,5);
  g->SetNpx(500);
  g->SetLineColor(2);
  g->SetParNames("norl","mu","sigma","k","l");
  g->SetParLimits(0,0.,20.);
  g->SetParLimits(1,-5.,5.);
  g->SetParLimits(2,0.001,5.0);
  g->SetParameters(0.0,0.0,1,2,0);
  g->FixParameter(4,0);
  Double_t entries, mean, rms;
  if (Fit) {
    entries = Fit->entries;
    mean = Fit->mean;
    rms = Fit->rms;
  } else {
    entries = proj->Integral();
    mean = proj->GetMean();
    rms  = proj->GetRMS();
  }
  if (entries < 100) return 0;
  g->SetParameter(0, TMath::Log(entries));
  g->SetParameter(1, mean);
  g->SetParameter(2, rms);
  g->FixParameter(3,10.0);
  if (Fit) {
    g->SetParameter(1, muPar(Fit->x));
    g->ReleaseParameter(1);
    g->FixParameter(2, sigmaPar(Fit->x));
    g->SetParameter(3, a0Par(Fit->x));
    g->ReleaseParameter(3);
  }
  proj->Fit(g,opt);
  return g;
}
//________________________________________________________________________________
TF1 *FitGEX(TH1 *proj, Option_t *opt="RQM", Fitx_t *Fit = 0) {
  if (! proj) return 0;
  static Int_t NF = 2;
  static Int_t F1 = 0;
  static TF1 *f[3] = {0};
  if (! f[F1]) {
    for (Int_t i = F1; i < NF; i++) {
      f[i] = new TF1(Form("GausExp%i",i),StdEdxModel::gausexp,-5,5,5);
      f[i]->SetLineColor(2+i);
      f[i]->SetParNames("norl","mu","sigma","k","l");
      f[i]->SetParLimits(0,0.,20.);
      f[i]->SetParLimits(1,-5.,5.);
      f[i]->SetParLimits(2,0.001,5.0);
      f[i]->SetParameters(0.0,0.0,1,2,0);
      f[i]->FixParameter(4,0);
      if (i == 0) {
	f[i]->FixParameter(3,10.0);
      } else if (i == 1) {
	f[i]->ReleaseParameter(3);
	f[i]->SetParLimits(3,0.1,10);
	f[i]->SetParameter(3,2.0);
      } else if (i == 2) {
	f[i]->ReleaseParameter(3);
	f[i]->SetParLimits(3,-10.,-0.1);
	f[i]->SetParameter(3,-2.0);
      }
    }
  }
  TF1 *g = 0;
  Double_t Chisquares[3] = {0};
  Double_t entries, mean, rms;
  if (Fit) {
    entries = Fit->entries;
    mean = Fit->mean;
    rms = Fit->rms;
  } else {
    entries = proj->Integral();
    mean = proj->GetMean();
    rms  = proj->GetRMS();
  }
  if (entries < 100) return 0;
  for (Int_t i = F1; i < NF; i++) {
    g = f[i];
    g->SetParameter(0, TMath::Log(entries));
    g->SetParameter(1, mean);
    g->SetParameter(2, rms);
    g->FixParameter(3,10.0);
    if (i == 1) {
      g->ReleaseParameter(3);
      g->SetParameter(3,1.0);
      g->SetParLimits(3,0.1,10);
    } else if (i == 2) {
      g->ReleaseParameter(3);
      g->SetParameter(3,-1.0);
      g->SetParLimits(3,-10.,-0.1);
    }
    proj->Fit(g,opt);
    Chisquares[i] = g->GetChisquare();
    //    if (Chisquares[i] > 1e-2) return g;
  }
  Int_t iBest = -1;
  Double_t ChisquareBest = 1e20;;
  for (Int_t i = F1; i < NF; i++) {
    if (! f[i]) continue;
    if (Chisquares[i] < ChisquareBest) {iBest = i; ChisquareBest = Chisquares[i];}
  }
  if (iBest < 0) return 0;;
  g = f[iBest];
  proj->Fit(g,opt);
  //  if (g->GetChisquare() < 1e-3) return 0;
  return g;
}
//________________________________________________________________________________
TF1 *FitGE(TH1 *proj, Option_t *opt="RQ") {
  if (! proj) return 0;
  TF1 *g = GausExp();
  proj->Fit(g,opt);

  return g;
}
//________________________________________________________________________________
TF1 *FitGE2(TH1 *proj, Option_t *opt="RQ", Double_t fitX=0) {
  if (! proj) return 0;
  Double_t params[9];
  TF1 *g = GausExp();
  Double_t k = 6.70823e-01;
  if (fitX > 2) {
    static Double_t parsk[7] = {    -8.959,     13.454,    -6.6549,     1.6064,   -0.20527,     0.0133, -0.00034269};
    static TF1 *pol6 = 0;
    Double_t X = fitX;
    if (! pol6) {
      pol6 = (TF1 *) gROOT->GetListOfFunctions()->FindObject("pol6");
      if (! pol6) {
	TF1::InitStandardFunctions();
	pol6 = (TF1 *) gROOT->GetListOfFunctions()->FindObject("pol6");
      }
    }
    Double_t k = pol6->EvalPar(&X, parsk);
  }
  g->SetParameter(1,proj->GetMean());
  g->SetParameter(2,proj->GetRMS());
  g->FixParameter(3, k);
  Int_t status = proj->Fit(g,opt);
  if (status) {
    g->SetParameter(1,proj->GetMean());
    g->SetParameter(2,proj->GetRMS());
    g->FixParameter(3, k);
    status = proj->Fit(g,opt);
    if (status) {
      cout << "Failed 2-nd time" << endl;
    }
  }
  return g;
}
//________________________________________________________________________________
TF1 *FitGE3(TH1 *proj, Option_t *opt="RQ", Double_t fitX=0) {
  if (! proj) return 0;
  Double_t params[9];
  TF1 *g = GausExp();
  static TF1 *pol6 = 0;
  static TF1 *pol2 = 0;
  if (! pol6) {
    pol6 = (TF1 *) gROOT->GetListOfFunctions()->FindObject("pol6");
    pol2 = (TF1 *) gROOT->GetListOfFunctions()->FindObject("pol2");
    if (! pol6) {
      TF1::InitStandardFunctions();
      pol6 = (TF1 *) gROOT->GetListOfFunctions()->FindObject("pol6");
      pol2 = (TF1 *) gROOT->GetListOfFunctions()->FindObject("pol2");
    }
  }
  Double_t k = 6.70823e-01;
  if (fitX > 2) {
    static Double_t parsk[7] = {    -8.959,     13.454,    -6.6549,     1.6064,   -0.20527,     0.0133, -0.00034269};
    k = pol6->EvalPar(&fitX, parsk);
  }
  Double_t pars6Sig[7] = {   0.82995,   0.033141,   -0.36552,   -0.18875,     0.2694,  -0.073444,  0.0038834};
  Double_t X = TMath::Log(fitX);
  Double_t sigma = pol6->EvalPar(&X, pars6Sig);
  g->FixParameter(3, k);
  g->FixParameter(2, sigma);
  g->SetParameter(1,proj->GetMean());
  Int_t status = proj->Fit(g,opt);
  if (status) {
    g->SetParameter(1,proj->GetMean());
    g->FixParameter(2, sigma);
    g->FixParameter(3, k);
    status = proj->Fit(g,opt);
    if (status) {
      cout << "Failed 2-nd time" << endl;
    }
  }
  return g;
}
//________________________________________________________________________________
TF1 *FitGE4(TH1 *proj, Option_t *opt="RQ", Double_t fitX=0) {
  if (! proj) return 0;
  Double_t params[9];
  TF1 *g = GausExp();
  static TF1 *pol6 = 0;
  static TF1 *pol2 = 0;
  if (! pol6) {
    pol6 = (TF1 *) gROOT->GetListOfFunctions()->FindObject("pol6");
    pol2 = (TF1 *) gROOT->GetListOfFunctions()->FindObject("pol2");
    if (! pol6) {
      TF1::InitStandardFunctions();
      pol6 = (TF1 *) gROOT->GetListOfFunctions()->FindObject("pol6");
      pol2 = (TF1 *) gROOT->GetListOfFunctions()->FindObject("pol2");
    }
  }
  Double_t k = 6.70823e-01;
  if (fitX > 2) {
    static Double_t parsk[7] = {    -8.959,     13.454,    -6.6549,     1.6064,   -0.20527,     0.0133, -0.00034269};
    k = pol6->EvalPar(&fitX, parsk);
  }
  Double_t pars6Sig[7] = {   0.82995,   0.033141,   -0.36552,   -0.18875,     0.2694,  -0.073444,  0.0038834};
  Double_t X = TMath::Log(fitX);
  Double_t sigma = pol6->EvalPar(&X, pars6Sig);
  //  Double_t parsMu1[7] = {    3.0608,    0.04283,    -0.9276,    0.99929,  -0.058499,   -0.22824,    0.06416};
  //  Double_t parsMu2[7] = {   0.33622,     3.2863,    -1.3061,    0.27857,  -0.032664,  0.0019921, -4.944e-05};
  Double_t parsMu1[7] = {    3.0767,  -0.054267,   -0.69433,    0.71462,    0.12861,   -0.29135,   0.072728};
  Double_t parsMu2[7] = {   0.33622,     3.2863,    -1.3061,    0.27857,  -0.032664,  0.0019922, -4.944e-05};
  Double_t mu = 0;
  if (fitX < 2) mu = pol6->EvalPar(&fitX, parsMu1);
  else          mu = pol6->EvalPar(&fitX, parsMu2);
  g->FixParameter(3, k);
  g->FixParameter(2, sigma);
  g->FixParameter(1, mu);
  Int_t status = proj->Fit(g,opt);
  if (status) {
    g->FixParameter(1,mu);
    g->FixParameter(2, sigma);
    g->FixParameter(3, k);
    status = proj->Fit(g,opt);
    if (status) {
      cout << "Failed 2-nd time" << endl;
    }
  }
  return g;
}
//________________________________________________________________________________
Double_t gf4Func(Double_t *x, Double_t *par) {
  // par[0] - norm
  // par[1] - pion position wrt Z_pion (Bichsel prediction)
  // par[2] - sigma 
  // par[3] - proton signal
  // par[4] - Kaon    -"-
  // par[5] - electorn -"-
  // par[6] - deuteron -"-
  // par[7] - Total
  // par[8] - case (-1 all, >-0 hyp no.)
  // par[9] - scale 
  Double_t mu    = par[1];
  Double_t sigma = par[2];
  Double_t scale = par[9];
  Double_t frac[5];
  Int_t i;
  frac[0] = 1;
  for (i = 1; i < 5; i++) {
    frac[i] = TMath::Sin(par[2+i]);
    frac[i] *= frac[i];
    frac[0] -= frac[i];
  }
  if (frac[0] < 0.4 && frac[1] < 0.4) return 0;
  static Double_t parMIP[5][4] = {
    /*   particle          norml,         mu,      sigma,      alpha */
    /*       pion */ {  14.08797,   -0.07141,    0.40063,    2.91620},
    /*     proton */ {  14.12205,    1.13661,    0.29324,    1.93451},
    /*       kaon */ {  14.29133,    0.33162,    0.36903,    2.62903},
    /*   electron */ {  14.33684,    0.21674,    0.36806,    2.60754},
    /*   deuteron */ {  11.63070,    2.17617,    0.15694,    0.53711}
  };
  Double_t Value = 0;
  Int_t icase = (Int_t) par[8];
  Int_t i1 = 0;
  Int_t i2 = 4;
  if (icase >= 0) {i1 = i2 = icase;}
  TF1 *g = GG();
  for (i = i1; i <= i2; i++) { 
    Double_t pars[4] = {0, parMIP[i][1] + mu, parMIP[i][2] + sigma, parMIP[i][3]};
    Value += frac[i]*g->EvalPar(x, pars);
  }
  return par[7]*TMath::Exp(par[0])*Value;
}
//________________________________________________________________________________
TF1 *FitG4F(TH1 *proj, Option_t *opt="") {
  // fit in momentum range p = 0.526 +/- 0.05;
  if (! proj) return 0;
  TString Opt(opt);
  //  Bool_t quet = Opt.Contains("Q",TString::kIgnoreCase);
  TF1 *g2 = (TF1*) gROOT->GetFunction("G4F");
  if (! g2) {
    g2 = new TF1("G4F",gf4Func, -5, 5, 10);
    g2->SetParName(0,"norm"); g2->SetParLimits(0,-80,80);
    g2->SetParName(1,"mu");     g2->SetParLimits(1,-0.5,0.5);
    g2->SetParName(2,"Sigma");  g2->SetParLimits(2,-0.1,0.5);
    g2->SetParName(3,"P");      g2->SetParLimits(3,0,1.5);
    g2->SetParName(4,"K");      g2->SetParLimits(4,0.0,0.5);
    g2->SetParName(5,"e");      g2->SetParLimits(5,0.0,0.5);
    g2->SetParName(6,"d");      g2->SetParLimits(6,0.0,0.5);
    g2->SetParName(7,"Total");
    g2->SetParName(8,"Case");
    g2->SetParName(9,"scale");  g2->FixParameter(9,1.);
    //    g2->SetParName(7,"factor"); g2->SetParLimits(7,-.1,0.1);
  }
  PreSetParameters(proj, g2);
  proj->Fit(g2,Opt.Data());
  g2->ReleaseParameter(3); g2->SetParLimits(3,0.0,1.5);
  g2->ReleaseParameter(4); g2->SetParLimits(4,0.0,0.5);
  g2->ReleaseParameter(5); g2->SetParLimits(5,0.0,0.5);
  g2->ReleaseParameter(6); g2->SetParLimits(6,0.0,0.5);
  //  g2->ReleaseParameter(9); g2->SetParLimits(9,0.5,2.0);
  Int_t iok = proj->Fit(g2,Opt.Data());
  if ( iok < 0) {
    cout << g2->GetName() << " fit has failed with " << iok << " for " 
	 << proj->GetName() << "/" << proj->GetTitle() << " Try one again" << endl; 
    proj->Fit(g2,Opt.Data());
  }
  Opt += "m";
  iok = proj->Fit(g2,Opt.Data());
  if (iok < 0 ) return 0;
  if (! Opt.Contains("q",TString::kIgnoreCase)) {
    Double_t params[10];
    g2->GetParameters(params);
    Double_t X = params[1];
    Double_t Y = TMath::Exp(params[0]);
    TPolyMarker *pm = new TPolyMarker(1, &X, &Y);
    proj->GetListOfFunctions()->Add(pm);
    pm->SetMarkerStyle(23);
    pm->SetMarkerColor(kRed);
    pm->SetMarkerSize(1.3);
    for (int i = 0; i <= 4; i++) {
      TF1 *f = new TF1(*g2);
      f->SetName(Peaks[i].Name);
      f->FixParameter(8,i);
      f->SetLineColor(i+2);
      proj->GetListOfFunctions()->Add(f);
    }
    proj->Draw();
  }
  return g2;
}
#include "FitG4EX.C"
#include "FitG4EY.C"
#include "FitG4EG.C"
//________________________________________________________________________________
Double_t gf4EFunc(Double_t *x, Double_t *par) {
  // par[0] - norm
  // par[1] - pion position wrt Z_pion (Bichsel prediction)
  // par[2] - sigma 
  // par[3] - proton signal
  // par[4] - Kaon    -"-
  // par[5] - electorn -"-
  // par[6] - deuteron -"-
  // par[7] - Total
  // par[8] - case (-1 all, >-0 hyp no.)
  // par[9] - scale 
  Double_t mu    = par[1];
  Double_t sigma = par[2];
  Double_t scale = par[9];
  Int_t IO = par[10];
  Int_t sign = par[11];
  Double_t frac[5];
  Double_t ff[5] = {0};
  for (Int_t i = 1; i < 5; i++) {
    ff[i] = TMath::Sin(par[2+i]);
    ff[i] *= ff[i];
  }
  frac[1] = ff[1];
  //  frac[0] = (1 - frac[1])/(1. + ff[2] + ff[3] + ff[4]);
  frac[0] = (1 - ff[1]*(1 + ff[4]))/(1 + ff[2] + ff[3]);
  frac[2] = frac[0]*ff[2];
  frac[3] = frac[0]*ff[3];
  frac[4] = ff[1]*ff[4];
  if (frac[0] < 0.4 && frac[1] < 0.4) return 0;
  /* Ajsustemt  root.exe Sec*.root FitPDraw.C+
0       Add SecRow3G4E3p85GeV_fixedTarget_2021.root
1       Add SecRow3PG4E3p85GeV_fixedTarget_2021.root
   .L Diff.C
   TH1D *d = Diff(P1,P0)
   d->Fit("pol0","er","",40.5,72.5)
    1  p0          -1.25473e-01   2.89467e-03   2.89467e-03  -5.15025e-07 // change sign -1.25473e-01 => +1.25473e-01
   */
  static const Char_t *hNames[5] = {"pion",       "proton",  "kaon",    "electron","deuteron"};
  static Double_t     hMasses[5] = {0.13956995, 0.93827231,0.493677, 0.51099907e-3,   2.80923};
  static Double_t pMoMIP = 0.526;
  static Double_t betaL[5] = {0};
  if (betaL[0] == 0.0) {
    for (Int_t i = 0; i < 5; i++) {
      Double_t bg = pMoMIP/hMasses[i];
      Double_t beta = bg/TMath::Sqrt(1. + bg*bg);
      betaL[i] = TMath::Log(beta);
    }
  }
#if 0 
  static Double_t parMIP[6][3][3][4] = {
   //          particle,      norml,         mu,     sigma,           k
   {{
       /*    zIpionN */ {  12.42300,   -0.00664,    0.29301,    0.74215},
       /*    zOpionN */ {  12.41324,   -0.02095,    0.28555,    0.80819},
       /*  zAllpionN */ {  13.11166,   -0.01478,    0.28886,    0.76870} },{ 
       /*    zIpionP */ {  12.83827,   -0.02013,    0.28956,    0.74146},
       /*    zOpionP */ {  12.82934,   -0.03569,    0.28089,    0.79885},
       /*  zAllpionP */ {  13.52736,   -0.02900,    0.28473,    0.76340} },{   
       /*     zIpion */ {  13.34498,   -0.01416,    0.29125,    0.74540},
       /*     zOpion */ {  13.33591,   -0.02949,    0.28302,    0.80502},
       /*   zAllpion */ {  14.03383,   -0.02268,    0.28667,    0.76995} }},{{ // -0.02268
       /*  zIprotonN */ {  12.49591,    1.15523,    0.25375,    1.03596},
       /*  zOprotonN */ {  12.29111,    1.18479,    0.24367,    1.33151},
       /* zAllprotonN*/ {  13.09377,    1.16757,    0.24943,    1.12090} },{
       /*  zIprotonP */ {  12.92588,    1.15708,    0.25315,    1.01766},
       /*  zOprotonP */ {  12.72925,    1.18975,    0.24382,    1.29504},
       /* zAllprotonP */{  13.52698,    1.17157,    0.24949,    1.10982 }},{// 
       /*   zIproton */ {  13.42702,    1.15650 ,    0.25351,    1.02661},
       /*   zOproton */ {  13.22752,    1.18763,    0.24378,    1.30888},
       /* zAllproton */ {  14.02682,    1.17009 ,    0.24952,    1.11551} }},{{ // 1.17009 - (-0.02268) = 1.19277
       /*   zIkaonN */ {  12.90181,    0.36992,    0.28164,    0.81581},
       /*   zOkaonN */ {  12.79121,    0.38884,    0.27210,    0.88391},
       /* zAllkaonN */ {  13.54178,    0.37845,    0.27692,    0.84168} },{
       /*   zIkaonP */ {  12.87831,    0.36249,    0.28037,    0.80669},
       /*   zOkaonP */ {  12.75407,    0.38233,    0.26843,    0.87318},
       /* zAllkaonP */ {  13.51208,    0.37188,    0.27530,    0.83466} },{
       /*    zIkaon */ {  13.58344,    0.36643,    0.28128,    0.81239},
       /*    zOkaon */ {  13.46610,    0.38541,    0.27003,    0.87692}, // + 5.63223e-02 + 8.13512e-02 -3.45257e-02 -1.23638e-01
       /*  zAllkaon */ {  14.22024,    0.37560,    0.27654,    0.84091} }},{{
       /* zIelectronN */ {  12.79014,    0.26079,    0.28365,    0.83437},
       /* zOelectronN */ {  12.70627,    0.27283,    0.27261,    0.90759},
       /* zAllelectronN */ {  13.44280,    0.26670,    0.27874,  0.86728} },{
       /* zIelectronP */ {  13.06649,    0.25402,    0.28324,    0.84412},
       /* zOelectronP */ {  12.97719,    0.26409,    0.26981,    0.89421},
       /* zAllelectronP */ {  13.69577,    0.27292,    0.28816,  0.97473} },{
       /* zIelectron */ {  13.63138,    0.25653,    0.28332,    0.83659},
       /* zOelectron */ {  13.54403,    0.26806,    0.27114,    0.90162},
       /* zAllelectron */ {  14.28232,    0.26226,    0.27796,    0.86641} }},{{
       /* zIdeuteronP */ {  11.41663,    2.20880,    0.16037,    2.41671},
       /* zOdeuteronP */ {   9.98541,    2.15613,    0.13790,    2.15994},
       /* zAlldeuteronP */ {  11.63118,    2.19846,    0.15727,    2.27099} },{
       /* zIdeuteronP */ {  11.41663,    2.20880,    0.16037,    2.41671},
       /* zOdeuteronP */ {   9.98541,    2.15613,    0.13790,    2.15994},
       /* zAlldeuteronP */ {  11.63118, 2.19846,    0.15727,    2.27099} },{
       /* zIdeuteron */ {  11.41663,    2.20880,    0.16037,    2.41671},
       /* zOdeuteron */ {   9.98541,    2.15613,    0.13790,    2.15995}, // + 1.05829e-01 - 2.00132e-02 + 6.06047e-02
       /* zAlldeuteron */ {  11.63118,    2.19846,    0.15727,    2.27099} }}
  };
#else
  static Double_t parMIP[6][3][3][4] = {
    {{
	// particle, norml, mu, sigma, alpha
	/*    zIpionN */ {  12.49894,   -0.01516,    0.28723,    0.73392},
	/*    zOpionN */ {  12.47612,   -0.05624,    0.27891,    0.79258},
	/*  zAllpionN */ {  13.18116,   -0.03714,    0.28285,    0.75505} },{ 
	/*    zIpionP */ {  12.89333,   -0.03122,    0.29183,    0.74512},
	/*    zOpionP */ {  12.87095,   -0.06885,    0.28490,    0.81509},
	/*  zAllpionP */ {  13.57531,   -0.05025,    0.28874,    0.77848} },{ 
	/*     zIpion */ {  13.40867,   -0.02404,    0.29067,    0.74409},
	/*     zOpion */ {  13.38591,   -0.06234,    0.28372,    0.81507},
	/*   zAllpion */ {  14.09004,   -0.04273,    0.28789,    0.78230} }},{{ 
	/*  zIprotonN */ {  12.58330,    1.14718,    0.25439,    1.04387},
	/*  zOprotonN */ {  12.36705,    1.15124,    0.24542,    1.36157},
	/* zAllprotonN */ {  13.17569,    1.14660,    0.24898,    1.11903} },{ 
	/*  zIprotonP */ {  13.02326,    1.15043,    0.25458,    1.03516},
	/*  zOprotonP */ {  12.81453,    1.15696,    0.24587,    1.33466},
	/* zAllprotonP */ {  13.61879,    1.15141,    0.24936,    1.11147} },{ 
	/*   zIproton */ {  13.52026,    1.14975,    0.25495,    1.04605},
	/*   zOproton */ {  13.30892,    1.15473,    0.24580,    1.34502},
	/* zAllproton */ {  14.11464,    1.14993,    0.24953,    1.12048} }},{{
	/*    zIkaonN */ {  12.99150,    0.36075,    0.27950,    0.81084},
	/*    zOkaonN */ {  12.86706,    0.35384,    0.27093,    0.88852},
	/*  zAllkaonN */ {  13.62526,    0.35530,    0.27391,    0.82993} },{ 
	/*    zIkaonP */ {  12.94309,    0.35420,    0.28282,    0.81350},
	/*    zOkaonP */ {  12.80451,    0.34797,    0.26976,    0.87855},
	/*  zAllkaonP */ {  13.57026,    0.35084,    0.27710,    0.83889} }},{{
	/*     zIkaon */ {  13.66098,    0.35760,    0.28132,    0.81212},
	/*     zOkaon */ {  13.52978,    0.35092,    0.27047,    0.88260},
	/*   zAllkaon */ {  14.29137,    0.35335,    0.27575,    0.83559} },{ 
	/* zIelectronN */ {  12.91914,    0.25140,    0.27990,    0.82704},
	/* zOelectronN */ {  12.82214,    0.23696,    0.26943,    0.89888},
	/* zAllelectronN */ {  13.56552,    0.24354,    0.27446,    0.85314} },{ 
	/* zIelectronP */ {  13.06714,    0.24452,    0.28483,    0.84283},
	/* zOelectronP */ {  12.96374,    0.23128,    0.27261,    0.90743},
	/* zAllelectronP */ {  13.71046,    0.23747,    0.27877,    0.86712} }},{{ 
	/* zIelectron */ {  13.68898,    0.24821,    0.28292,    0.83882},
	/* zOelectron */ {  13.54404,    0.26266,    0.29212,    3.06713},
	/* zAllelectron */ {  14.33371,    0.24064,    0.27704,    0.86315} },{ 
	/* zIdeuteronP */ {  11.41599,    2.19135,    0.15619,    2.54299},
	/* zOdeuteronP */ {   9.98441,    2.10469,    0.13408,    2.40409},
	/* zAlldeuteronP */ {  11.63081,    2.17448,    0.15585,    2.43176}  },{ 
	/* zIdeuteron */ {  11.41599,    2.19135,    0.15619,    2.54298},
	/* zOdeuteron */ {   9.98441,    2.10469,    0.13408,    2.40409},
	/* zAlldeuteron */ {  11.63081,    2.17448,    0.15585,    2.43176} }}
  };
#endif
  Double_t Value = 0;
  Int_t icase = (Int_t) par[8];
  Int_t i1 = 0;
  Int_t i2 = 4;
  if (icase >= 0) {i1 = i2 = icase;}
  TF1 *g = GausExp();
  for (Int_t i = i1; i <= i2; i++) { 
    Double_t Mu = mu + parMIP[i][sign][IO][1] - parMIP[0][sign][IO][1];
    Double_t dbetaL = betaL[i] - betaL[0];
    if (dbetaL > 0) dbetaL = 0;
    Mu += scale*dbetaL;
    Double_t pars[4] = {0, Mu, parMIP[i][sign][IO][2] + sigma, parMIP[i][sign][IO][3]};
    Value += frac[i]*g->EvalPar(x, pars);
  }
  return par[7]*TMath::Exp(par[0])*Value;
}
//________________________________________________________________________________
TF1 *FitG4E(TH1 *proj, Option_t *opt="", Int_t IO = 2, Int_t Sign = 2) {
  // fit in momentum range p = 0.526 +/- 0.05;
  if (! proj) return 0;
  TString Opt(opt);
  //  Bool_t quet = Opt.Contains("Q",TString::kIgnoreCase);
  TF1 *g2 = (TF1*) gROOT->GetFunction("G4E");
  if (! g2) {
    g2 = new TF1("G4E",gf4EFunc, -5, 5, 12);
    g2->SetParName(0,"norm");   g2->SetParLimits(0,-80,80); // g2->FixParameter(0,0.0); // 
    g2->SetParName(1,"mu");     g2->SetParLimits(1,-1.2,1.2);
    g2->SetParName(2,"Sigma");  g2->FixParameter(2,0.0); //g2->SetParLimits(2,-0.1,0.1);
    g2->SetParName(3,"P");      g2->SetParLimits(3,0.0,TMath::Pi()/2);
    g2->SetParName(4,"K");      g2->SetParLimits(4,0.1,TMath::Pi()/2); 
    g2->SetParName(5,"e");      g2->SetParLimits(5,0.1,TMath::Pi()/2);
    g2->SetParName(6,"d");      g2->SetParLimits(6,0.0,TMath::Pi()/2);
    g2->SetParName(7,"Total");
    g2->SetParName(8,"Case");
    g2->SetParName(9,"scale");  g2->FixParameter(9,-0.113);
    g2->SetParName(10,"IO"); g2->FixParameter(10,IO); 
    g2->SetParName(11,"sign"); g2->FixParameter(11,Sign); 
    //    g2->SetParName(7,"factor"); g2->SetParLimits(7,-.1,0.1);
  }
  PreSetParameters(proj, g2);
  //g2->FixParameter(2, 0.0);
  g2->SetParameter(4,0.15);
  g2->SetParameter(5,0.15);
  g2->FixParameter(9,0.);
  g2->FixParameter(10,IO); 
  g2->FixParameter(11,Sign); 
  proj->Fit(g2,Opt.Data());
  g2->ReleaseParameter(3); //g2->SetParLimits(3,0.0,1.5);
  g2->ReleaseParameter(4); //g2->SetParLimits(4,0.0,0.5);
  g2->ReleaseParameter(5); //g2->SetParLimits(5,0.0,0.5);
  g2->ReleaseParameter(6); //g2->SetParLimits(6,0.0,0.5);
  //  g2->ReleaseParameter(9);
  //  g2->ReleaseParameter(9); g2->SetParLimits(9,-2.0,2.0);
  g2->FixParameter(9,-0.113);
  Int_t iok = proj->Fit(g2,Opt.Data());
  if ( iok < 0) {
    cout << g2->GetName() << " fit has failed with " << iok << " for " 
	 << proj->GetName() << "/" << proj->GetTitle() << " Try one again" << endl; 
    proj->Fit(g2,Opt.Data());
  }
  Opt += "m";
  iok = proj->Fit(g2,Opt.Data());
  if (iok < 0 ) return 0;
  if (! Opt.Contains("q",TString::kIgnoreCase)) {
    Double_t params[10];
    g2->GetParameters(params);
    Double_t X = params[1];
    Double_t Y = TMath::Exp(params[0]);
    TPolyMarker *pm = new TPolyMarker(1, &X, &Y);
    proj->GetListOfFunctions()->Add(pm);
    pm->SetMarkerStyle(23);
    pm->SetMarkerColor(kRed);
    pm->SetMarkerSize(1.3);
    for (int i = 0; i <= 4; i++) {
      TF1 *f = new TF1(*g2);
      f->SetName(Peaks[i].Name);
      f->FixParameter(8,i);
      f->SetLineColor(i+2);
      proj->GetListOfFunctions()->Add(f);
    }
    proj->Draw();
  }
  return g2;
}
//________________________________________________________________________________
TF1 *FitGG2(TH1 *proj, Option_t *opt="RQ", Double_t fitX=0, Double_t fitY=0) {
  //  root.exe AdcSparseD4.root lBichsel.C 'dEdxFit.C+("nePI+nePO","GG2")'
  //  root.exe AdcSparseD4.root lBichsel.C 'dEdxFit.C+("nePI+nePO","GG2","R",-1,-1,2)'
  //  root.exe AdcSparseD4.root lBichsel.C 'dEdxFit.C+("nePI+nePO","GG2","R",-1,-1,3)'
  //  root.exe AdcSparseD4.root lBichsel.C 'dEdxFit.C+("nePI+nePO","GG2","R",-1,-1,4)'
  //  root.exe AdcSparseD4.root lBichsel.C 'dEdxFit.C+("nePI+nePO","GG2","R",-1,-1,5)'
  if (! proj) return 0;
  Double_t params[9];
  TF1 *g = GG();
#if 0
#if 0
  Double_t xx = TMath::Max(3.1, TMath::Min(6.5, fitX));
  Double_t sigma =        1.10043e+00   + xx * (-2.14927e-01   + xx * 9.77528e-03);
#else /* 09/14/2022  all->Draw("sigma:log(x)","i&&j&&dmu<0.01&&dsigma<0.01&&dp3<0.5&&chisq<2e2","prof") */
  Double_t xx = TMath::Log(fitX);
#if 0
  Double_t pars[3] = {    2.2404,    -1.8257,    0.37502}; // pol2
#else /* 09/25/2022  */
  Double_t pars[3] = {    2.1392,    -1.7129,    0.34465}; //
#endif
  Double_t sigma = pars[0] + xx * ( pars[1] + xx * pars[2]);
#endif 
#else /* 10/27/22 */
  Double_t parsS[3] = {    1.6924,    -1.2912,    0.24698}; //sigma versus log(x)	 
  Double_t xx = (fitX > 0) ? TMath::Log(fitX) : 0;
  Double_t sigma = parsS[0] + xx * ( parsS[1] + xx * parsS[2]);
#endif
  g->FixParameter(2, sigma);
  proj->Fit(g,opt);
  return g;
}
//________________________________________________________________________________
TF1 *FitGG3(TH1 *proj, Option_t *opt="RQ", Double_t fitX=0, Double_t fitY=0) {
  //  root.exe AdcSparseD4.root lBichsel.C 'dEdxFit.C+("nePI+nePO","GG3")'
  //  root.exe AdcSparseD4.root lBichsel.C 'dEdxFit.C+("nePI+nePO","GG3","R",-1,-1,2)'
  //  root.exe AdcSparseD4.root lBichsel.C 'dEdxFit.C+("nePI+nePO","GG3","R",-1,-1,3)'
  //  root.exe AdcSparseD4.root lBichsel.C 'dEdxFit.C+("nePI+nePO","GG3","R",-1,-1,4)'
  //  root.exe AdcSparseD4.root lBichsel.C 'dEdxFit.C+("nePI+nePO","GG3","R",-1,-1,5)'
  if (! proj) return 0;
  Double_t params[9];
  TF1 *g = GG();
#if 0
#if 0
  Double_t xx = TMath::Max(3.1, TMath::Min(6.5, fitX));
  Double_t sigma =        1.10043e+00   + xx * (-2.14927e-01   + xx * 9.77528e-03);
  // tChain->Draw("a0:x>>alpha(40,3,7)","i&&j&&dmu<0.01&&a0>-0.1&&a0<3.9&&mu>0.65","prof")
  Double_t alpha =    -22.6936   + xx*(    16.2998   + xx*(   -3.28817   + xx*   0.209148 ));
#else /* 09/14/2022   all->Draw("p3:x>>ap3","i&&j&&dmu<0.01&&dsigma<0.01&&dp3<0.5&&chisq<2e2","prof");  ap3->Fit("pol3","e") */
  Double_t xx = TMath::Log(fitX);
#if 0 
  Double_t parsS[3] = {    2.2404,    -1.8257,    0.37502}; // pol2
  Double_t sigma = parsS[0] + xx * ( parsS[1] + xx * parsS[2]);
  Double_t parsA[4] = {    25.659,    -8.4331,    0.97394,  -0.037646}; //
  xx = fitX;
  Double_t alpha  = parsA[0] + xx * ( parsA[1] + xx * (parsA[2] + xx * parsA[3]));
#else /* 09/25/2022 */
  Double_t parsS[3] = {    2.1392,    -1.7129,    0.34465}; // log(x)
  Double_t sigma = parsS[0] + xx * ( parsS[1] + xx * parsS[2]);
  Double_t parsA[4] = {   -55.709,     106.35,    -62.082,     11.565}; // log(x)
  Double_t alpha  = parsA[0] + xx * ( parsA[1] + xx * (parsA[2] + xx * parsA[3]));
#endif
#endif
#else /* 10/27/22 */
  Double_t parsS[3] = {    1.6924,    -1.2912,    0.24698}; //sigma versus log(x)	 
  Double_t xx = (fitX > 0) ? TMath::Log(fitX) : 0;
  Double_t sigma = parsS[0] + xx * ( parsS[1] + xx * parsS[2]);
  Double_t parsA[2] = {    5.4634,   -0.57598}; //alpha x
  Double_t alpha  = parsA[0] + fitX *  parsA[1];
#endif
  g->FixParameter(2, sigma);
  g->FixParameter(3, alpha);
  proj->Fit(g,opt);
  return g;
}
//________________________________________________________________________________
TF1 *FitGG5(TH1 *proj, Option_t *opt="RQ", Double_t fitX=0, Int_t IO=1) {
  if (! proj) return 0;
  Double_t params[9];
  TF1 *g = GG();
  // 10/26/22
  Double_t xx = fitX;
  Double_t parsA[2] = {    5.9122,   -0.63601}; //alpha
  Double_t alpha  = 0;
  if (xx < 8.0) alpha = parsA[0] + xx * parsA[1];
  Double_t parsS[4] = {    1.2538,   -0.27805,   0.017227, -0.00012342}; //Sigma
  Double_t sigma = parsS[0] + xx * ( parsS[1] + xx * (parsS[2] + xx * parsS[3]));
#if 0
  Double_t parsM[3] = {  0.014451,    0.21908,  -0.013445}; //Mu
  xx = TMath::Max(4.9, TMath::Min(9.0, xx));
  Double_t mu     = parsM[0] + xx * ( parsM[1] + xx * parsM[2]);
  g->SetParameter(1, mu);
#else
  Double_t parsM[2][3] = {
    {   0.61239,   0.036834,          0}, //mu Inner
    {   0.10942,    0.19366,  -0.011761}  //mu Outer
  };
  xx = TMath::Max(4.9, TMath::Min(9.0, xx));
  Double_t mu     = parsM[IO][0] + xx * ( parsM[IO][1] + xx * parsM[IO][2]);
  g->FixParameter(1, mu);
#endif
  g->FixParameter(2, sigma);
  g->FixParameter(3, alpha);
  proj->Fit(g,opt);
  return g;
}
//________________________________________________________________________________
TF1 *FitGG4(TH1 *proj, Option_t *opt="RQ", Double_t fitX=0, Double_t fitY=0) {
  //  root.exe AdcSparseD4.root lBichsel.C 'dEdxFit.C+("nePI+nePO","GG4")'
  //  root.exe AdcSparseD4.root lBichsel.C 'dEdxFit.C+("nePI+nePO","GG4","R",-1,-1,2)'
  //  root.exe AdcSparseD4.root lBichsel.C 'dEdxFit.C+("nePI+nePO","GG4","R",-1,-1,3)'
  //  root.exe AdcSparseD4.root lBichsel.C 'dEdxFit.C+("nePI+nePO","GG4","R",-1,-1,4)'
  //  root.exe AdcSparseD4.root lBichsel.C 'dEdxFit.C+("nePI+nePO","GG4","R",-1,-1,5)'
  if (! proj) return 0;
  Double_t params[9];
  TF1 *g = GG();
  /* 10/27/22 */
  Double_t parsA[2] = {    5.4634,   -0.57598}; //alpha x
  Double_t alpha  = parsA[0] + fitX *  parsA[1];
  Double_t parsS[3] = {    1.6924,    -1.2912,    0.24698}; //sigma versus log(x)	 
  Double_t xx = (fitX > 0) ? TMath::Log(fitX) : 0;
  Double_t sigma = parsS[0] + xx * ( parsS[1] + xx * parsS[2]);
  TF1 *pol7 = (TF1 *) gROOT->GetListOfFunctions()->FindObject("pol7");
  if (! pol7) {
    TF1::InitStandardFunctions();
    pol7 = (TF1 *) gROOT->GetListOfFunctions()->FindObject("pol7");
  }
  Double_t parsM[8] = {   -4.3432,     4.6327,    -1.9522,     0.4691,  -0.066615,  0.0055111, -0.00024531, 4.5394e-06}; //mu pol7
  Double_t mu = pol7->EvalPar(&fitX, parsM);
  g->FixParameter(1, mu);
  g->FixParameter(2, sigma);
  g->FixParameter(3, alpha);
  proj->Fit(g,opt);
  return g;
}
//________________________________________________________________________________
TF1 *FitG2(TH1 *proj, Option_t *opt="RQ") {
  if (! proj) return 0;
  Double_t params[9];
  TF1 *gaus = new TF1("gaus","gaus",-5.,5.);
  proj->Fit(gaus,opt);
  params[0] = gaus->GetParameter(0);
  params[1] = gaus->GetParameter(1);
  params[2] = gaus->GetParameter(2);
  params[3] = 1;
  params[4] = 2;
  params[5] = gaus->GetParameter(2);
  params[6] = 0;
  params[7] = 0;
  params[8] = 0;
  TF1 *g = new TF1("g","gaus(0)+gaus(3)",-5.,5.);
  g->SetParameters(params);
  g->SetParLimits(0,0,1.e10);
  g->SetParLimits(1,-5,5);
  g->SetParLimits(2,0.01,5);
  g->SetParLimits(3,0,1.e10);
  g->SetParLimits(4,-5,5);
  g->SetParLimits(5,0.01,5);
  proj->Fit(g,opt);
  g->GetParameters(params);
  if (TMath::Abs(params[1]) > TMath::Abs(params[4])) {
    g->SetParameter(0,params[3]);
    g->SetParameter(1,params[4]);
    g->SetParameter(2,params[5]);
    g->SetParameter(3,params[0]);
    g->SetParameter(4,params[1]);
    g->SetParameter(5,params[2]);
    proj->Fit(g,opt);
  }
  proj->Fit(g,opt);
  delete gaus;
  return g;
}
//________________________________________________________________________________
TF1 *FitG3(TH1 *proj, Option_t *opt="RQ") {
  if (! proj) return 0;
  Double_t params[9];
  TF1 *gaus = new TF1("gaus","gaus",-5.,5.);
  TF1 *g = 0;
  proj->Fit(gaus,opt);
  params[0] = gaus->GetParameter(0);
  params[1] = gaus->GetParameter(1);
  params[2] = gaus->GetParameter(2);
  params[3] = 1;
  params[4] = 2;
  params[5] = gaus->GetParameter(2);
  params[6] = 0;
  params[7] = 0;
  params[8] = 0;
  delete gaus;
  g = new TF1("g","gaus(0)+gaus(3)",-5.,5.);
  g->SetParameters(params);
  g->SetParLimits(0,0,1.e10);
  g->SetParLimits(1,-5,5);
  g->SetParLimits(2,0.1,5);
  g->SetParLimits(3,0,1.e10);
  g->SetParLimits(4,-5,5);
  g->SetParLimits(5,0.1,5);
  proj->Fit(g,opt);
  g->GetParameters(params);
  if (TMath::Abs(params[1]) > TMath::Abs(params[4])) {
    g->SetParameter(0,params[3]);
    g->SetParameter(1,params[4]);
    g->SetParameter(2,params[5]);
    g->SetParameter(3,params[0]);
    g->SetParameter(4,params[1]);
    g->SetParameter(5,params[2]);
  }
  proj->Fit(g,opt);
  if (g->GetProb() > 0) return g;
  g->GetParameters(params);
  delete g;
  g = new TF1("g","gaus(0)+gaus(3)+gaus(6)",-5.,5.);
  g->SetParLimits(6,0,1.e10);
  g->SetParLimits(7,-5,5);
  g->SetParLimits(8,0.1,5);
  params[6] = 10.;
  params[7] = 0.5*(params[1]+params[4]);
  params[8] = params[2];
  g->SetParameters(params);
  proj->Fit(g,opt);
  return g;
}
//________________________________________________________________________________
void FitB4G(Int_t icase = 0, Int_t hyp=-1, Int_t bin=0, 
	    Double_t xmin1=-1.0, Double_t xmax1 = 1.0,
		     Double_t Mu2 = -.5,Double_t xmin2=-2., Double_t xmax2 = 4.,
		     Double_t Mu3 = 0.1,Double_t xmin3=-2., Double_t xmax3 = 4.,
		     Double_t Mu4 = 0.3
	    )
{   // icase = 0 z; icase != 0 70
  Double_t sigmas[2] = {0.06,0.12};
  const Int_t nHYPS = NHYPS;
  TH2 *hists[nHYPS];
  TProfile *histp[nHYPS];
  TFile *fRootFile = (TFile *) gDirectory->GetFile();
  if (! fRootFile ) {printf("Cannot find/open %s",fRootFile->GetName()); return;}
  TFile *f = 0;
  if (bin == 0) {
    TString newfile("FitB4G");
    newfile += gSystem->BaseName(fRootFile->GetName());
    f = new TFile(newfile.Data(),"update");
  }
  Int_t i;
  for (i = 0; i< nHYPS; i++) {
    if (hyp >= 0 && hyp != i) continue;
    if (! icase) {
      hists[i] = (TH2 *) fRootFile->Get(HistNames[i]);
      if (!hists[i]) {printf("Cannot histogram %s\n",HistNames[i]); return;}
    }
    else {
      hists[i] = (TH2 *) fRootFile->Get(HistNames70[i]);
      if (!hists[i]) {printf("Cannot histogram %s\n",HistNames70[i]); return;}
    }
    histp[i] = (TProfile *) fRootFile->Get(HistNameP[i]);
    if (!histp[i]) {printf("Cannot histogram %s\n",HistNameP[i]); return;}
  }
  TF1 *g1 = new TF1("g1","gaus",xmin1,xmax1);
  TF1 *g = 0, *g2 = 0, *g3 = 0, *g4 = 0, *ga = 0;
  if (! canvas)  canvas = new TCanvas("canvas","canvas");
  if (Mu2 != 0) {
    if (Mu2 < -1.) {
      if   (Mu2 <= -4.) ga = new TF1("ga","gaus(0)+exp(pol3(3))",xmin2,xmax2);
      else {
	if (Mu2 <= -3.) ga = new TF1("ga","gaus(0)+exp(pol2(3))",xmin2,xmax2);
	else            ga = new TF1("ga","gaus(0)+exp(pol1(3))",xmin2,xmax2);
      }
      g = ga;
      g->SetParName(0,"Constant1");
      g->SetParName(1,"Mean1"); 
      g->SetParName(2,"Sigma1"); g->SetParLimits(2,sigmas[0],sigmas[1]);
      g->SetParName(3,"Const");
      g->SetParName(4,"Slope1"); 
      g->SetParName(5,"Slope2"); 
      g->SetParName(6,"Slope3"); 
      g->SetParName(7,"Slope4"); 
    }
    else {
      g2 = new TF1("g2","gaus(0)+gaus(3)",xmin2,xmax2);
      g = g2;
      g->SetParName(0,"Constant1");
      g->SetParName(1,"Mean1"); 
      g->SetParName(2,"Sigma1"); g->SetParLimits(2,sigmas[0],sigmas[1]);
      g->SetParName(3,"Constant2");
      g->SetParName(4,"Mean2"); 
      g->SetParName(5,"Sigma2"); g->SetParLimits(5,sigmas[0],sigmas[1]);
      if (Mu3 != 0) { 
	g3= new TF1("g3","gaus(0)+gaus(3)+gaus(6)",xmin3,xmax3);
	g = g3;
	g->SetParName(0,"Constant1");
	g->SetParName(1,"Mean1"); 
	g->SetParName(2,"Sigma1"); g->SetParLimits(2,sigmas[0],sigmas[1]);
	g->SetParName(3,"Constant2");
	g->SetParName(4,"Mean2"); 
	g->SetParName(5,"Sigma2"); g->SetParLimits(5,sigmas[0],sigmas[1]);
	g->SetParName(6,"Constant3");
	g->SetParName(7,"Mean3"); 
	g->SetParName(8,"Sigma3"); g->SetParLimits(8,sigmas[0],sigmas[1]);
	if (Mu4 != 0) { 
	  g4= new TF1("g4","gaus(0)+gaus(3)+gaus(6)+gaus(9)",xmin3,xmax3);
	  g4->SetParName(0,"Constant1");
	  g4->SetParName(1,"Mean1"); 
	  g4->SetParName(2,"Sigma1"); g4->SetParLimits(2,sigmas[0],sigmas[1]);
	  g4->SetParName(3,"Constant2");
	  g4->SetParName(4,"Mean2"); 
	  g4->SetParName(5,"Sigma2"); g4->SetParLimits(5,sigmas[0],sigmas[1]);
	  g4->SetParName(6,"Constant3");
	  g4->SetParName(7,"Mean3"); 
	  g4->SetParName(8,"Sigma3"); g4->SetParLimits(8,sigmas[0],sigmas[1]);
	  g4->SetParName(9,"Constant4");
	  g4->SetParName(10,"Mean4"); 
	  g4->SetParName(11,"Sigma4"); g4->SetParLimits(11,sigmas[0],sigmas[1]);
	}
      }
    }
  }
  TH1 *proj = 0;
  Double_t params[12];
  Int_t k;//,ibin;
  Int_t Bin = TMath::Abs(bin);
  for (k = 0; k<nHYPS; k++) {
    if (hyp != -1 && k != hyp) continue;
    TH2 *hist = hists[k];
    Int_t nx = hist->GetNbinsX();
    for (i=1; i<=nx; i++) {
      if (bin != 0 && Bin != i) continue;
      //      if (proj) delete  proj;
      Char_t line[40];
      sprintf(line,"%s_%i",hist->GetName(),i);
      TString name(line);
      proj = hist->ProjectionY(name.Data(),i,i);
      Int_t ix1=proj->GetXaxis()->FindBin(-.1);
      Int_t ix2=proj->GetXaxis()->FindBin(0.1);
      if (proj->Integral(ix1,ix2) < 100.) {
	printf("hist:%s bin %i for hyp %i has only %10.0f entries\n",hist->GetName(),Bin,k,proj->Integral());
	delete proj;
	continue;
      }
      Int_t NFitPoints = 0;
      Double_t chisq;
      Double_t xrange = 0.6;
      g = g1;
      g->SetParLimits(0,0,1.e7);
      g->SetParLimits(1,-xrange,xrange); 
      g->SetParLimits(2,sigmas[0],sigmas[1]);
      proj->Fit(g->GetName(),"r");
      g->GetParameters(params);
      Int_t kcase = -1;
      if (xmin1 < -0.5 && xmax1 > 0.5 && g->GetChisquare() < 1.e3 // g->GetProb() > 1.e-3 
	  ) goto Done;
	//	  && TMath::Abs(g->GetParameter(1)) < 0.05) goto Done;
     if (ga) {
	g = ga;
	g1->GetParameters(params);
	params[1] = 0.;
	params[3] = 0.;
	params[4] = 0.;
	params[5] = 0.;
	params[6] = 0.;
	params[7] = 0.;
	params[8] = 0.;
	g->SetParameters(params);
	kcase = 0;
	if (g->GetChisquare() < 1.e3 // g->GetProb() > 1.e-3
	    ) goto Done;
      }
      else {
	if (g2) {
	  params[1] = 0.;
	  params[3] = params[0];
	  params[4] = Mu2;
	  params[5] = 2.0*params[2];
	  g = g2;
	  g->SetParameters(params);
	  g->SetParLimits(0,0,1.e7);
	  g->SetParLimits(1,-xrange,xrange); 
	  //	  g->SetParLimits(1,-.3,.3);
	  g->SetParLimits(2,sigmas[0],sigmas[1]);
	  g->SetParLimits(3,0,1.e7);
	  g->SetParLimits(5,sigmas[0],sigmas[1]);
	  proj->Fit(g->GetName(),"r");
	  g->GetParameters(params);
	  kcase = 2;
	  if (g->GetChisquare() < 1.e3 // g->GetProb() > 1.e-3
	      ) goto Done;
	  if (g3) {
	    params[1] = 0.;
	    params[6] = params[0];
	    params[7] = Mu3;
	    params[8] = 2.0*params[2];
	    g = g3;
	    g->SetParameters(params);
	    g->SetParLimits(0,0,1.e7);
	    g->SetParLimits(1,-xrange,xrange); 
	    //	    g->SetParLimits(1,-.1,.1);
	    g->SetParLimits(2,sigmas[0],sigmas[1]);
	    g->SetParLimits(3,0,1.e7);
	    g->SetParLimits(5,sigmas[0],sigmas[1]);
	    g->SetParLimits(6,0,1.e7);
	    g->SetParLimits(8,sigmas[0],sigmas[1]);
	    proj->Fit(g->GetName(),"r");
	    g->GetParameters(params);
	    kcase = 3;
	    if (g->GetChisquare() < 1.e3 // g->GetProb() > 1.e-3
		) goto Done;
	    if (g4) {
	      params[1] = 0.;
	      params[9] = params[0];
	      params[10] = Mu4;
	      params[11] = 2.0*params[2];
	      g = g4;
	      g->SetParameters(params);
	      g->SetParLimits(0,0,1.e7);
	      g->SetParLimits(1,-xrange,xrange); 
      //	      g->SetParLimits(1,-.1,.1);
	      g->SetParLimits(2,sigmas[0],sigmas[1]);
	      g->SetParLimits(3,0,1.e7);
	      g->SetParLimits(5,sigmas[0],sigmas[1]);
	      g->SetParLimits(6,0,1.e7);
	      g->SetParLimits(8,sigmas[0],sigmas[1]);
	      g->SetParLimits(9,0,1.e7);
	      g->SetParLimits(11,sigmas[0],sigmas[1]);
	      proj->Fit(g->GetName(),"r");
	      kcase = 4;
	      g->GetParameters(params);
	    }
	  }
	}
      }
    Done:
     if (g) {
       proj->Fit(g->GetName(),"RM");
       canvas->Update();
       Int_t l = 1;
       Double_t mu =  g->GetParameter(l);
       for (int m=2;m<=kcase;m++) {
	 if (TMath::Abs(mu) > TMath::Abs(g->GetParameter(3*m-2))) {
	   l = 3*m-2; mu =  g->GetParameter(l);
	   //	    printf("l=%i\n",l);
	 }
       }
       Nu[N]  = g->GetParameter(l);// printf("l=%i Nu=%f\n",l,Nu[N]);
       Mu[N]  = Nu[N] + histp[k]->GetBinContent(i);
       dMu[N] = g->GetParError(l);//Mu[N];
       //    Mu[N] = TMath::Log(Mu[N]);
       Sigma[N]  = g->GetParameter(2);
       dSigma[N] = g->GetParError(2);
       NFitPoints = g->GetNumberFitPoints();
       Int_t NDF = g->GetNDF();
       Double_t prob = g->GetProb();//TMath::Prob(chisq, NDF);
       chisq      = g->GetChisquare();
       X[N] = hist->GetXaxis()->GetBinCenter(i);
       dX[N] = hist->GetXaxis()->GetBinWidth(i);
       Double_t pionM = Masses[k]*pow(10.,X[N]);
       printf ("%s :hyp = %i bin=%i, Point=%i, x=%f, p=%f, Delta_I=%f, I=%f, Sigma_I=%f,\n"
	       "chisq=%f, NoPoints=%i,ndf=%i, prob=%f\n",
	       Names[k],k,i,N,X[N],pionM,Nu[N],Mu[N],dMu[N],chisq,NFitPoints,NDF,prob);
       //	if (bin < 0 && prob > 1.e-7 && TMath::Abs(Nu[N]) < 0.05) {
       //	if ( bin >= 0 && prob > 1.e-7 && TMath::Abs(Nu[N]) < 0.10 || bin < 0) {
       //       if ( bin >= 0 && TMath::Abs(Nu[N]) < 0.10 || bin < 0) {
       if ( bin >= 0) {
	 printf("{\"%-4s\",%2i,%4i,%6i,%6.3f,%7.3f,%10.6f,%10.6f,%8.5f,%10.3f},//%3i,%3i,%5.3f -- %s\n",
		Names[k],k,i,N,X[N],pionM,Nu[N],Mu[N],dMu[N],chisq,NFitPoints,NDF,prob,g->GetName());
	 TString FileN("FitPars");
	 if (hyp > -1)  FileN += hist->GetName();
	 FileN += ".h";
	 FILE *fp = fopen(FileN.Data(),"a");
	 if (fp) {
	   if (N == 0) {
	     TDatime time;
	     fprintf(fp,"// Date: Time = %i : %i\n",time.GetDate(), time.GetTime());
	     fprintf(fp,
		     "//          bin, Point,     x,      p,   Delta_I,         I, Sigma_I, chisq, NoPoints,ndf, prob\n");
	   }
	   fprintf(fp,
		   "{\"%-4s\",%2i,%4i,%6i,%6.3f,%7.3f,%10.6f,%10.6f,%8.5f,%10.3f},//%3i,%3i,%5.3f -- %s\n",
		   Names[k],k,i,N,X[N],pionM,Nu[N],Mu[N],dMu[N],chisq,NFitPoints,NDF,prob,g->GetName());
	   fclose(fp);
	 }
	 proj->Write();
       }
       else printf ("================== Skip it\n");
     }
     N++; 	
     proj->Draw();// cnew->Update();
    }
  }
  if (f) {delete f;}
}
//________________________________________________________________________________
void Fit4G(Int_t ng=2, Int_t hyp=-1, Int_t bin=0, 
	   Double_t xmin1=-1.0, Double_t xmax1 = 1.0,
	   Double_t Mu2 = -.5,Double_t xmin2=-1., Double_t xmax2 = 1.,
	   Double_t Mu3 = 0.0,Double_t xmin3=-1., Double_t xmax3 = 1.,
	   Double_t Mu4 = 0.0){  
  Double_t sigmas[2] = {0.06,0.12};
  const Int_t nHYPS = 4;
  TH2 *hists[nHYPS];
  TProfile *histp[nHYPS];
  TFile *fRootFile = (TFile *) gDirectory->GetFile();
  if (! fRootFile ) {printf("Cannot find/open %s",fRootFile->GetName()); return;}
  TFile *f = 0;
  if (bin == 0) {
    TString newfile("BBFit");
    newfile += gSystem->BaseName(fRootFile->GetName());
    f = new TFile(newfile.Data(),"update");
  }
  for (int i = 0; i< nHYPS; i++) {
    if (hyp >= 0 && hyp != i) continue;
    hists[i] = (TH2 *) fRootFile->Get(HistNames[i]);
    if (!hists[i]) {printf("Cannot histogram %s\n",HistNames[i]); return;}
    histp[i] = (TProfile *) fRootFile->Get(HistNameP[i]);
    if (!histp[i]) {printf("Cannot histogram %s\n",HistNameP[i]); return;}
  }
  TF1 *g1 = new TF1("g1","gaus",xmin1,xmax1);
  TF1 *g = 0, *g2 = 0, *g3 = 0, *g4 = 0, *ga = 0;
  if (! canvas) canvas = new TCanvas("canvas","canvas");
  if (ng<0) {
    ga = new TF1("ga",Form("gaus(0)+exp(pol%i(3))",-ng),xmin2,xmax2);
    ga->SetParName(0,"Constant1");
    ga->SetParName(1,"Mean1"); 
    ga->SetParName(2,"Sigma1");
    ga->SetParName(3,"Const");
    for (int m = 0; m <= -ng; m++) ga->SetParName(m+3,Form("Slope%i",m)); 
  }
  else {
    for (int m = 2; m <= ng; m++) {
      if (m == 2)  {g2 = new TF1("g2","gaus(0)+gaus(3)",xmin2,xmax2); g = g2;}
      if (m == 3)  {g3 = new TF1("g3","gaus(0)+gaus(3)+gaus(6)",xmin3,xmax3); g = g3;}
      if (m == 4)  {g4 = new TF1("g4","gaus(0)+gaus(3)+gaus(6)+gaus(9)",-1.,1.); g = g4;}
      for (int k = 0; k < 3*m; k += 3) {
	g->SetParName(k  ,Form("Constant%i",m));
	g->SetParName(k+1,Form("Mean%i",m)); 
	g->SetParName(k+2,Form("Sigma%i",m));
      }
    }
  }
  TH1 *proj = 0;
  Double_t params[12];
  Int_t k,i;//,ibin;
  Int_t Bin = TMath::Abs(bin);
  for (k = 0; k<nHYPS; k++) {
    if (hyp != -1 && k != hyp) continue;
    TH2 *hist = hists[k];
    Int_t nx = hist->GetNbinsX();
    for (i=1; i<=nx; i++) {
      if (bin != 0 && Bin != i) continue;
      //      if (proj) delete  proj;
      Char_t line[40];
      sprintf(line,"%s_%i",hist->GetName(),i);
      TString name(line);
      proj = hist->ProjectionY(name.Data(),i,i);
      Int_t ix1=proj->GetXaxis()->FindBin(-.1);
      Int_t ix2=proj->GetXaxis()->FindBin(0.1);
      if (proj->Integral(ix1,ix2) < 100) {
	printf("hist:%s bin %i for hyp %i has only %10.0f entries\n",hist->GetName(),Bin,k,proj->Integral());
	delete proj;
	continue;
      }
      Int_t NFitPoints = 0;
      Double_t xrange = 0.6;
      Double_t chisq;
      g = g1;
      g->SetParLimits(0,0,1.e7);
      g->SetParLimits(1,-xrange,xrange); 
      //      g->SetParLimits(1,-.1,.1);
      g->SetParLimits(2,sigmas[0],sigmas[1]);
      proj->Fit(g->GetName(),"ri");
      g->GetParameters(params);
      Int_t kcase = -1;
      if (xmin1 < -0.5 && xmax1 > 0.5 && g->GetProb() > 1.e-7 
	  && TMath::Abs(g->GetParameter(1)) < 0.05) goto Done;
     if (ga) {
	g = ga;
	g1->GetParameters(params);
	params[1] = 0.;
	params[3] = 0.;
	params[4] = 0.;
	params[5] = 0.;
	params[6] = 0.;
	params[7] = 0.;
	params[8] = 0.;
	g->SetParameters(params);
	kcase = 0;
	if (g->GetProb() > 1.e-7) goto Done;
      }
      else {
	if (g2) {
	  params[1] = 0.;
	  params[3] = params[0];
	  params[4] = Mu2;
	  params[5] = 2.0*params[2];
	  g = g2;
	  g->SetParameters(params);
	  g->SetParLimits(0,0,1.e7);
	  g->SetParLimits(1,-xrange,xrange); 
	  //	  g->SetParLimits(1,-.1,.1);
	  g->SetParLimits(2,sigmas[0],sigmas[1]);
	  g->SetParLimits(3,0,1.e7);
	  g->SetParLimits(5,sigmas[0],sigmas[1]);
	  proj->Fit(g->GetName(),"ri");
	  g->GetParameters(params);
	  kcase = 2;
	  if (g->GetProb() > 1.e-7) goto Done;
	  if (g3) {
	    params[1] = 0.;
	    params[6] = params[0];
	    params[7] = Mu3;
	    params[8] = 2.0*params[2];
	    g = g3;
	    g->SetParameters(params);
	    g->SetParLimits(0,0,1.e7);
	    g->SetParLimits(1,-xrange,xrange); 
      //	    g->SetParLimits(1,-.1,.1);
	    g->SetParLimits(2,sigmas[0],sigmas[1]);
	    g->SetParLimits(3,0,1.e7);
	    g->SetParLimits(5,sigmas[0],sigmas[1]);
	    g->SetParLimits(6,0,1.e7);
	    g->SetParLimits(8,sigmas[0],sigmas[1]);
	    proj->Fit(g->GetName(),"ri");
	    g->GetParameters(params);
	    kcase = 3;
	    if (g->GetProb() > 1.e-7) goto Done;
	    if (g4) {
	      params[1] = 0.;
	      params[9] = params[0];
	      params[10] = Mu4;
	      params[11] = 2.0*params[2];
	      g = g4;
	      g->SetParameters(params);
	      g->SetParLimits(0,0,1.e7);
      g->SetParLimits(1,-xrange,xrange); 
	      //	      g->SetParLimits(1,-.1,.1);
	      g->SetParLimits(2,sigmas[0],sigmas[1]);
	      g->SetParLimits(3,0,1.e7);
	      g->SetParLimits(5,sigmas[0],sigmas[1]);
	      g->SetParLimits(6,0,1.e7);
	      g->SetParLimits(8,sigmas[0],sigmas[1]);
	      g->SetParLimits(9,0,1.e7);
	      g->SetParLimits(11,sigmas[0],sigmas[1]);
	      proj->Fit(g->GetName(),"ri");
	      kcase = 4;
	      g->GetParameters(params);
	    }
	  }
	}
      }
    Done:
      if (g) {
	proj->Fit(g->GetName(),"RIM");
	canvas->Update();
	Int_t l = 1;
	Double_t mu =  g->GetParameter(l);
	for (int m=2;m<=kcase;m++) {
	  if (TMath::Abs(mu) > TMath::Abs(g->GetParameter(3*m-2))) {
	    l = 3*m-2; mu =  g->GetParameter(l);
	    //	    printf("l=%i\n",l);
	  }
	}
	Nu[N]  = g->GetParameter(l);// printf("l=%i Nu=%f\n",l,Nu[N]);
	Mu[N]  = Nu[N] + histp[k]->GetBinContent(i);
	dMu[N] = g->GetParError(l);///Mu[N];
	//    Mu[N] = TMath::Log(Mu[N]);
	Sigma[N]  = g->GetParameter(2);
	dSigma[N] = g->GetParError(2);
	NFitPoints = g->GetNumberFitPoints();
	Int_t NDF = g->GetNDF();
	Double_t prob = g->GetProb();//TMath::Prob(chisq, NDF);
	chisq      = g->GetChisquare();
	X[N] = hist->GetXaxis()->GetBinCenter(i);
	dX[N] = hist->GetXaxis()->GetBinWidth(i);
	Double_t pionM = Masses[k]*pow(10.,X[N]);
	printf ("%s :hyp = %i bin=%i, Point=%i, x=%f, p=%f, Delta_I=%f, I=%f, Sigma_I=%f,\n"
		"chisq=%f, NoPoints=%i,ndf=%i, prob=%f\n",
		Names[k],k,i,N,X[N],pionM,Nu[N],Mu[N],dMu[N],chisq,NFitPoints,NDF,prob);
	//	if (bin < 0 && prob > 1.e-7 && TMath::Abs(Nu[N]) < 0.05) {
	//	if ( bin >= 0 && prob > 1.e-7 && TMath::Abs(Nu[N]) < 0.10 || bin < 0) {
	if ( (bin >= 0 && TMath::Abs(Nu[N]) < 0.10) || bin < 0) {
	  printf("{\"%-4s\",%2i,%4i,%6i,%6.3f,%7.3f,%10.6f,%10.6f,%8.5f,%10.3f},//%3i,%3i,%5.3f -- %s\n",
		 Names[k],k,i,N,X[N],pionM,Nu[N],Mu[N],dMu[N],chisq,NFitPoints,NDF,prob,g->GetName());
	  TString FileN("FitPars");
	  if (hyp > -1)  FileN += HistNames[hyp];
	  FileN += ".h";
	  FILE *fp = fopen(FileN.Data(),"a");
	  if (fp) {
	    if (N == 0) {
	      TDatime time;
	      fprintf(fp,"// Date: Time = %i : %i\n",time.GetDate(), time.GetTime());
	      fprintf(fp,
"//          bin, Point,     x,      p,   Delta_I,         I, Sigma_I, chisq, NoPoints,ndf, prob\n");
	    }
	  fprintf(fp,
		  "{\"%-4s\",%2i,%4i,%6i,%6.3f,%7.3f,%10.6f,%10.6f,%8.5f,%10.3f},//%3i,%3i,%5.3f -- %s\n",
		  Names[k],k,i,N,X[N],pionM,Nu[N],Mu[N],dMu[N],chisq,NFitPoints,NDF,prob,g->GetName());
	    fclose(fp);
	  }
	  proj->Write();
	}
	else printf ("================== Skip it\n");
      }
      N++; 	
      proj->Draw();// cnew->Update();
    }
  }
  if (f) {delete f;}
}
//________________________________________________________________________________
Double_t gmp(Double_t *x, Double_t *p) {
  Double_t normL = p[0];
  Double_t nu    = p[1];
  Double_t sigma = p[2];
  Double_t gamma = p[3];
  Double_t grass = p[4];
  Double_t sign  = p[5];
  Double_t val   = grass;
  if (sigma > 0 && gamma > 1) {
    Double_t t = sign*(x[0] - nu)/sigma + (gamma - 1);
    if (t > 0) { 
      val += TMath::Exp(normL)*TMath::GammaDist(t,gamma,0.,1.);
    }
  }
  return val;
}
//________________________________________________________________________________
TF1 *GMP() { // Fit Gamma + grass
  static TF1 *f = 0;
  if (! f) {
    f = new TF1("GMP",gmp,-5,5,6);
    f->SetParNames("normL", "mu", "sigma", "gamma", "grass","sign");
    f->SetParameters(4.17880, 2.86452, 0.2, 6.0, 0., 1.);
    f->SetParLimits(0,0,50);
    f->SetParLimits(1,-10,10);
    f->SetParLimits(2,0.1,2);
    f->SetParLimits(3,2,50);
    f->SetParLimits(4,0,1e3);
    f->FixParameter(5,1.);
  }
  return f;
}
//________________________________________________________________________________
TF1 *GMN() { // Fit Gamma + grass  with reverse sign
  static TF1 *f = 0;
  if (!f ) {
    f = new TF1("GMN",gmp,-5,5,6);
    f->SetParNames("normL", "mu", "sigma", "gamma", "grass","sign");
    f->SetParameters(4.17880, 2.86452, 0.2, 6.0, 0., 1.);
    f->SetParLimits(0,0,50);
    f->SetParLimits(1,-10,10);
    f->SetParLimits(2,0.1,2);
    f->SetParLimits(3,2,50);
    f->SetParLimits(4,0,1e3);
    f->FixParameter(5,-1.);
  }
  return f;
}
//#define DEBUG
//________________________________________________________________________________
TList *ListOfKeys() {
  TList *list = 0;
  TSeqCollection *files = gROOT->GetListOfFiles();
  TIter next(files);
  TFile *f = 0;
  while ((f = (TFile *) next())) {
    f->cd();
    list = f->GetListOfKeys();
    break;
  }
  return list;
}
//________________________________________________________________________________
TH1 *FindHistograms(const Char_t *Name, const Char_t *fit = "") {
  TH1 *hist = 0;
#ifdef DEBUG
  cout << "FindHistograms(\"" << Name << "\",\"" << fit << "\")" << endl;
#endif  
  TSeqCollection *files = gROOT->GetListOfFiles();
  TIter next(files);
  TFile *f = 0;
  TString Fit(fit);
  if (Fit != "") {
    while ((f = (TFile *) next())) {
#ifdef DEBUG
      cout << "look in " << f->GetName() << endl;
#endif
      TString fName(gSystem->BaseName(f->GetName()));
      if (fName.BeginsWith(Name)) {
	hist = (TH1 *) f->Get(fit);
#ifdef DEBUG
	if (hist) cout << "Found histogram " << hist->GetName() << " in file " << f->GetName() << endl;
#endif
	return hist;
      }
    }
  } else {
    while ((f = (TFile *) next())) {
      f->cd();
#ifdef DEBUG
      cout << "look in " << f->GetName() << endl;
#endif
      hist = (TH1 *) f->Get(Name);
      if (hist) {
#ifdef DEBUG
	cout << "Found histogram " << hist->GetName() << " in file " << f->GetName() << endl;
#endif
	return hist;
      }
    }
  }
  return hist;
}
//________________________________________________________________________________
void DrawSummary0(TFile *f, const Char_t *opt) {
  if (! f) return;
  gStyle->SetTimeOffset(788936400);
  f->cd();
  TList *keys = f->GetListOfKeys();
  if (! keys) return;
  keys->Sort();
  TIter next(keys);
  TKey *key = 0;
  TObjArray hists;
  TString Opt(opt);
  TString Title;
  TF1 *powfit = new TF1("powfit","[0]*pow(x,[1])",40,80);
  powfit->SetParameters(0.5,-0.5);
  while ((key = (TKey *) next())) {
    TH1 *hist  = FindHistograms(key->GetName());
    if (! hist) continue;
    if (hist->GetEntries() < 1) continue;
    if (Opt != "") {
      TString name(hist->GetName());
      if (! name.Contains(Opt.Data())) continue;
    }
    hists.AddLast(hist);
    TString hName(hist->GetName());
    if ((( hist->IsA()->InheritsFrom( "TH3C" )   ||
	   hist->IsA()->InheritsFrom( "TH3S" ) 	 ||
	   hist->IsA()->InheritsFrom( "TH3I" ) 	 ||
	   hist->IsA()->InheritsFrom( "TH3F" ) 	 ||
	   hist->IsA()->InheritsFrom( "TH3D" ) ) || 
	 ( hist->IsA()->InheritsFrom( "TH2C" )   ||
	   hist->IsA()->InheritsFrom( "TH2S" ) 	 ||
	   hist->IsA()->InheritsFrom( "TH2I" ) 	 ||
	   hist->IsA()->InheritsFrom( "TH2F" ) 	 ||
	   hist->IsA()->InheritsFrom( "TH2D" ) ))&& 
	 ( ! (hName.BeginsWith("Points") || hName.BeginsWith("TPoints") || hName.BeginsWith("MPoints")) ) ) {
      TH1 *mu    = FindHistograms(key->GetName(),"mu");
      if (! mu) continue;
      mu->SetName(Form("%s_mu",hist->GetName()));
      hists.AddLast(mu);
      TH1 *sigma = FindHistograms(key->GetName(),"sigma");
      if (! sigma) continue;
      sigma->SetName(Form("%s_sigma",hist->GetName()));
      hists.AddLast(sigma);
    }
  }
  Int_t N = hists.GetEntriesFast();
  Int_t nx = (Int_t) (TMath::Sqrt(N));
  if (nx*nx < N) nx++;
  Int_t ny = N/nx;
  if (nx*ny < N) ny++;
  cout << "N " << N << " nx " << nx << " ny " << ny << endl;
  TString Tag(gSystem->BaseName(f->GetName()));
  Tag.ReplaceAll(".root","");
  Tag += opt;
  //  TCanvas *c1 = new TCanvas(Tag,Tag,200,10,700,780);
  Double_t dx = 0.98/nx;
  Double_t dy = 0.98/ny;
  TObjArray pads(N);
  for (Int_t iy = 0; iy < ny; iy++) {
    for (Int_t ix = 0; ix < nx; ix++) {
      Int_t i = ix + nx*iy;
      if (i >= N) continue;
      TH1 *hist = (TH1 *) hists[i];
      if (! hist) continue;
      Double_t x1 = 0.01 + dx* ix;
      Double_t x2 = 0.01 + dx*(ix+1);
      Double_t y1 = 0.99 - dy*(iy+1);
      Double_t y2 = 0.99 - dy* iy;
      TPad *pad = new TPad(Form("pad_%s",hist->GetName()),hist->GetTitle(),x1,y1,x2,y2);
      //      cout << "Create pad " << pad->GetName() << "/" << pad->GetTitle() << endl;
      pad->Draw();
      pads.AddLast(pad);
    }
  }
  for (Int_t iy = 0; iy < ny; iy++) {
    for (Int_t ix = 0; ix < nx; ix++) {
      Int_t i = ix + nx*iy;
      if (i >= N) continue;
      TH1 *hist = (TH1 *) hists[i];
      if (! hist) continue;
      TPad *pad = (TPad *) pads[i];
      if (! pad) continue;
      pad->cd();
      TProfile *prof = 0;
      if ( hist->IsA()->InheritsFrom( "TProfile" ) ) prof = (TProfile *) hist;
      Int_t NbinsX = hist->GetNbinsX();
      Int_t xmin =  NbinsX;
      Int_t xmax =  0;
      Double_t ymin =  1e9;
      Double_t ymax = -1e9;
      //      cout << "Draw pad " << pad->GetName() << "/" << pad->GetTitle() << "\t x " << ix << " y " << iy << endl;
      //3D
      if ( hist->IsA()->InheritsFrom( "TH3C" ) ||
	   hist->IsA()->InheritsFrom( "TH3S" ) ||
	   hist->IsA()->InheritsFrom( "TH3I" ) ||
	   hist->IsA()->InheritsFrom( "TH3F" ) ||
	   hist->IsA()->InheritsFrom( "TH3D" ) ) {
	((TH3 *)hist)->Project3DProfile("xy")->Draw("colz");
	goto TIMEAxis;
      }
      //2D
      if ( hist->IsA()->InheritsFrom( "TH2C" ) ||
	   hist->IsA()->InheritsFrom( "TH2S" ) ||
	   hist->IsA()->InheritsFrom( "TH2I" ) ||
	   hist->IsA()->InheritsFrom( "TH2F" ) ||
	   hist->IsA()->InheritsFrom( "TH2D" ) ) {
	if (hist->GetMaximum() > 0 && hist->GetMinimum() >= 0) pad->SetLogz(1);
	TString hName(hist->GetName());
	if (hName.BeginsWith("Points") || hName.BeginsWith("TPoints") || hName.BeginsWith("MPoints")) {
	  Title = hist->GetTitle();
	  Title.ReplaceAll("/sigma","");
	  hist->SetTitle(Title);
	  TAxis *y = hist->GetYaxis();
	  Int_t iy1 = y->FindBin(-0.15);
	  Int_t iy2 = y->FindBin( 0.50);
	  y->SetRange(iy1,iy2);
	  ((TH2 *)hist)->Draw("colz");
	  TH1 *mu    = FindHistograms(hist->GetName(),"mu");
	  if (! mu) goto TIMEAxis;
	  mu->SetMarkerColor(1);
	  mu->SetMarkerStyle(20);
	  mu->Draw("same");
	  mu->Fit("pol0","e0r","",10,120);
	  TLegend *leg = new TLegend(0.25,0.6,0.9,0.9,"");
	  TF1 *f = mu->GetFunction("pol0");
	  if (f) {
	    f->Draw("same");
	    Title = Form("#mu  = %5.2f +/- %5.2f %\%",100*f->GetParameter(0),100*f->GetParError(0));
	    cout << Title << endl;
	    leg->AddEntry(mu,Title.Data());
	  }
	  TH1 *sigma = FindHistograms(hist->GetName(),"sigma");
	  if (! sigma) goto TIMEAxis;
	  sigma->SetMarkerColor(1);
	  sigma->SetMarkerStyle(21);
	  sigma->Draw("same");
	  sigma->Fit(powfit,"r0");
	  f = sigma->GetFunction("powfit");
	  if (f) {
	    f->Draw("same");
	    Title = Form("#sigma(@76cm) = %5.2f%\%",100*f->Eval(76));
	    cout << Title << endl;
	    leg->AddEntry(sigma,Title.Data());
	  }
	  leg->Draw();
	} else hist->Draw("colz");
	goto TIMEAxis;
      }
      //1D + Prof
      NbinsX = hist->GetNbinsX();
      xmin =  NbinsX;
      xmax =  0;
      ymin =  1e9;
      ymax = -1e9;
      for (Int_t i = 1; i <= NbinsX; i++) {
	Double_t y = hist->GetBinContent(i);
	if ((prof && prof->GetBinEntries(i)) || y > 0) {
	  Int_t x = i;
	  xmin = TMath::Min(xmin,x);
	  xmax = TMath::Max(xmax,x);
	  ymin = TMath::Min(ymin,y);
	  ymax = TMath::Max(ymax,y);
	}
      }
      if (ymin < ymax) {
	hist->SetMaximum(1.1*ymax);
	hist->SetMinimum(0.9*ymin);
	//	  cout << "Set min/max for " << hist->GetName() << "\t" << hist->GetMinimum() << "/" <<  hist->GetMaximum() << endl;
      }
      if (xmin < xmax) hist->GetXaxis()->SetRange(xmin,xmax);
      hist->Draw();
    TIMEAxis:
      TAxis *xax = hist->GetXaxis();
      if (xax->GetBinLowEdge(1) > 1e6) {
	xax->SetTimeDisplay(1);
	gPad->Modified();
      }
    } 
  }
}
//________________________________________________________________________________
void DrawSummary(const Char_t *opt="") {
  TSeqCollection *files = gROOT->GetListOfFiles();
  TIter next(files);
  TFile *f = 0;
  TString FName("");
  while ((f = (TFile *) next())) {
    FName = gSystem->BaseName(f->GetName());
    if (FName.BeginsWith("Hist")) break;
  }
  if (! f) {
    cout << "Hist* root file has not been found" << endl;
    return;
  }
  DrawSummary0(f,opt);
}
//________________________________________________________________________________
void ShiftMu(TH2 *mu, TH1 *muI=0, TH1 *muJ=0) {
  if (!mu) return;
  if (! muI) {muI = (TH1 *) mu->Clone(); muI->SetName("muI");}
  if (! muJ) {muJ = (TH1 *) mu->Clone(); muJ->SetName("muJ");}
  Int_t nx = mu->GetNbinsX();
  Int_t ny = mu->GetNbinsY();
  for (Int_t i = 1; i <= nx; i++) {
    for (Int_t j = 1; j <= ny; j++) {
      Double_t valI = mu->GetBinContent(i,j) - mu->GetBinContent(0,j);
      Double_t errI = TMath::Sqrt(mu->GetBinContent(i,j)*mu->GetBinContent(i,j) + mu->GetBinContent(0,j)* mu->GetBinContent(0,j));
      muI->SetBinContent(i,j,valI);
      muI->SetBinError(i,j,errI);
      Double_t valJ = mu->GetBinContent(i,j) - mu->GetBinContent(i,0);
      Double_t errJ = TMath::Sqrt(mu->GetBinContent(i,j)*mu->GetBinContent(i,j) + mu->GetBinContent(i,0)* mu->GetBinContent(i,01));
      muJ->SetBinContent(i,j,valJ);
      muJ->SetBinError(i,j,errJ);
    }
  }
}
//________________________________________________________________________________
void ShiftMu(TH1 *mu, TH1 *muI) {
  if (!mu) return;
  if (! muI) {muI = (TH1 *) mu->Clone(); muI->SetName("muI");}
  Int_t nx = mu->GetNbinsX();
  for (Int_t i = 1; i <= nx; i++) {
    Double_t valI = mu->GetBinContent(i) - mu->GetBinContent(0);
    Double_t errI = TMath::Sqrt(mu->GetBinContent(i)*mu->GetBinContent(i) + mu->GetBinContent(0)* mu->GetBinContent(0));
    muI->SetBinContent(i,valI);
    muI->SetBinError(i,errI);
  }
}
//________________________________________________________________________________
void FitH3D(TH1 *hist, TF1 *fun,
	     Option_t *opt="R", 
	     Int_t ix = -1, Int_t jy = -1, 
	     Int_t mergeX=1, Int_t mergeY=1, 
	     Double_t nSigma=3, Int_t pow=1) {
  if (! fun) return;
  TString Opt(opt);
  if (! Opt.Contains("Q",TString::kIgnoreCase)) {
    canvas = (TCanvas *) gROOT->GetListOfCanvases()->FindObject("Fit");
    if (! canvas) canvas = new TCanvas("Fit","Fit results");
    else          canvas->Clear();
  }
  if (! hist) return;
  TAxis *xax = hist->GetXaxis();
  Int_t nx = xax->GetNbins(); printf ("nx = %i",nx);
  Axis_t xmin = xax->GetXmin(); printf (" xmin = %f",xmin);
  Axis_t xmax = xax->GetXmax(); printf (" xmax = %f\n",xmax);
  TAxis *yax = hist->GetYaxis();
  Int_t dim = hist->GetDimension();
  Int_t ny = yax->GetNbins();
  Int_t NX = nx;
  Int_t NY = ny;
  if (dim < 3) ny = 1;  printf ("ny = %i",ny);
  Axis_t ymin = yax->GetXmin(); printf (" ymin = %f",ymin);
  Axis_t ymax = yax->GetXmax(); printf (" ymax = %f\n",ymax);
  struct Fit_t {
    Float_t i;
    Float_t j;
    Float_t x;
    Float_t y;
    Float_t mean;
    Float_t rms;
    Float_t peak;
    Float_t entries;
    Float_t chisq;
    Float_t prob;
    Float_t Npar;
    Float_t params[20]; // params & errors
  };
  Fit_t Fit;
  TDirectory *fRootFile = hist->GetDirectory();
  //  TString NewRootFile(gSystem->DirName(fRootFile->GetName()));
  TString NewRootFile(gSystem->DirName(gSystem->BaseName(fRootFile->GetName())));
  NewRootFile += "/";
  NewRootFile += hist->GetName();
  NewRootFile += fun->GetName();
  if (ix >= 0) NewRootFile += Form("_X%i",ix);
  if (jy >= 0) NewRootFile += Form("_Y%i",jy);
  if (mergeX != 1) NewRootFile += Form("_x%i",mergeX);
  if (mergeY != 1) NewRootFile += Form("_y%i",mergeY);
  //  NewRootFile += "_2_";
  NewRootFile += gSystem->BaseName(fRootFile->GetName());
  if (! FitP) {
    if (! fOut) {
      fOut = new TFile(NewRootFile.Data(),"update");
      if (! fOut) fOut = new TFile(NewRootFile.Data(),"new");
      if (fOut) cout << NewRootFile << " has been opened." << endl;
      else {cout << "Failed to open " << NewRootFile << endl; return;}
    }
    FitP = (TNtuple *) fOut->Get("FitP");
  }
  Double_t params[10] = {0};
  Double_t parerr[10] = {0};
  TString  parNames[10];
  Int_t imu = -1, isigma = -1;
  TString VarN("i:j:x:y:mean:rms:peak:entries:chisq:prob:Npar");
  TString ParN, dParN;
  Int_t npar = fun->GetNpar();
  for (Int_t p = 0; p < npar; p++) {
    parNames[p] = fun->GetParName(p);;
    VarN += ":"; VarN += parNames[p];
    VarN += ":d"; VarN += parNames[p];
    if        (parNames[p].Contains("mu",TString::kIgnoreCase)) {
      imu = p;
    } else if (parNames[p].Contains("sigma",TString::kIgnoreCase)) {
      isigma = p;
    }
  }
  if (! FitP) {
    FitP = new TNtuple("FitP","Fit results",VarN);
    FitP->SetMarkerStyle(20);
    FitP->SetLineWidth(2);
  }
  TH1 *mean = (TH1 *) fOut->Get("mean");
  TH1 *rms  = (TH1 *) fOut->Get("rms");    
  TH1 *entries = (TH1 *) fOut->Get("entries");
  TH1 *mu   = (TH1 *) fOut->Get("mu");
  TH1 *sigma= (TH1 *) fOut->Get("sigma");
  TH1 *chisq= (TH1 *) fOut->Get("chisq");
  if (! mu) {
    if (dim == 3) {
      mean    = new TH2D("mean",hist->GetTitle(),nx,xmin,xmax,ny,ymin,ymax);
      rms     = new TH2D("rms",hist->GetTitle(),nx,xmin,xmax,ny,ymin,ymax);
      entries = new TH2D("entries",hist->GetTitle(),nx,xmin,xmax,ny,ymin,ymax);
      mu      = new TH2D("mu",hist->GetTitle(),nx,xmin,xmax,ny,ymin,ymax);
      sigma   = new TH2D("sigma",hist->GetTitle(),nx,xmin,xmax,ny,ymin,ymax);
      chisq   = new TH2D("chisq",hist->GetTitle(),nx,xmin,xmax,ny,ymin,ymax);
    }
    else {
      if (dim == 2 || dim == 1) {
	mean    = new TH1D("mean",hist->GetTitle(),nx,xmin,xmax);
	rms     = new TH1D("rms",hist->GetTitle(),nx,xmin,xmax);
	entries = new TH1D("entries",hist->GetTitle(),nx,xmin,xmax);
	mu      = new TH1D("mu",hist->GetTitle(),nx,xmin,xmax);
	sigma   = new TH1D("sigma",hist->GetTitle(),nx,xmin,xmax);
	chisq   = new TH1D("chisq",hist->GetTitle(),nx,xmin,xmax);
      }
      else {
	printf("Histogram %s has wrong dimension %i\n", hist->GetName(),dim);
	return;
      }
    }
  }
  TH1 *proj = 0;
  TF1 *g = 0;
  Int_t ix1 = ix, jy1 = jy;
  if (ix >= 0) nx = ix;
  if (jy >= 0) ny = jy;
  if (ix1 < 0) ix1 = 0;
  if (jy1 < 0) jy1 = 0;
  for (int i=ix1;i<=nx-mergeX+1;i++){
    Int_t ir0 = i;
    Int_t ir1=i+mergeX-1;
    if (i == 0) {ir0 = 1; ir1 = nx;}
    for (int j=jy1;j<=ny-mergeY+1;j++){
      Int_t jr0 = j;
      Int_t jr1 = j+mergeY-1;
      if (j == 0) {jr0 = 1; jr1 = ny;}
      if (dim == 3) {
	if (ir0 == ir1 && jr0 == jr1) {
	  proj = ((TH3 *) hist)->ProjectionZ(Form("f%i_%i",      ir0,    jr0    ),ir0,ir1,jr0,jr1); 
	  cout<<"Histogram "<<proj->GetName()<<" was created"<<endl;
	} else {  
	  proj = ((TH3 *) hist)->ProjectionZ(Form("f%i_%i_%i_%i",ir0,ir1,jr0,jr1),ir0,ir1,jr0,jr1);
	  cout<<"Histogram "<<proj->GetName()<<" was created"<<endl;
	}
	if (! proj) continue;
	TString title(proj->GetTitle());
	title += Form(" in x [%5.1f,%5.1f] and y [%5.1f,%5.1f] range",
		      xax->GetBinLowEdge(ir0), xax->GetBinUpEdge(ir1),
		      yax->GetBinLowEdge(jr0), yax->GetBinUpEdge(jr1));
	proj->SetTitle(title.Data());
      }
      else {
	if (ir0 == ir1) 
	  {  proj = ((TH2 *) hist)->ProjectionY(Form("f%i",   ir0),    ir0,ir1);cout<<"Histogram "<<proj->GetName()<<" was created"<<endl;}
	else 
	  {  proj = ((TH2 *) hist)->ProjectionY(Form("f%i_%i",ir0,ir1),ir0,ir1);cout<<"Histogram "<<proj->GetName()<<" was created"<<endl;}
	if (! proj) continue;
	TString title(proj->GetTitle());
	title += Form(" in x [%5.1f,%5.1f] range",
		      xax->GetBinLowEdge(ir0), xax->GetBinUpEdge(ir1));
	proj->SetTitle(title.Data());
      }
      cout << "i/j\t" << i << "/" << j << "\t" <<  proj->GetName() << "\t" << proj->GetTitle() << "\t" <<  proj->Integral() << endl;
      //      continue;
      memset (&Fit, 0, sizeof(Fit_t));
      Fit.i = (2.*i+mergeX-1.)/2;
      Fit.j = (2.*j+mergeY-1.)/2;
      Fit.x = 0.5*(xax->GetBinLowEdge(i) + xax->GetBinUpEdge(i+mergeX-1));
      Fit.y = 0.5*(yax->GetBinLowEdge(j) + yax->GetBinUpEdge(j+mergeY-1));
      Fit.mean = proj->GetMean();
      Fit.rms  = proj->GetRMS();
      Fit.chisq = -100;
      Fit.prob  = 0;
      Fit.entries = proj->Integral();
      mean->SetBinContent(i,j,Fit.mean);
      rms->SetBinContent(i,j,Fit.rms);
      entries->SetBinContent(i,j,Fit.entries);
      chisq->SetBinContent(i,j,Fit.chisq);
      if (Fit.entries < 100) {delete proj; continue;}
      fun->SetParameter(imu,Fit.mean);
      fun->SetParameter(isigma,Fit.rms);
      proj->Fit(fun,opt);
      fun->GetParameters(params);
      const Double_t *params = fun->GetParameters();
      const Double_t *parerr = fun->GetParErrors();
      Int_t Npar = fun->GetNpar();
      Fit.Npar  = Npar;
      Fit.chisq = fun->GetChisquare();
      Fit.prob  = fun->GetProb();
      for (Int_t p = 0; p < Npar; p++) {
	Fit.params[2*p+0] = params[p];
	Fit.params[2*p+1] = parerr[p];
	if        (parNames[p].Contains("mu",TString::kIgnoreCase)) {
	  mu->SetBinContent(i,j,params[p]);
	  mu->SetBinError(i,j,parerr[p]);
	  //	  imu = p;
	} else if (parNames[p].Contains("sigma",TString::kIgnoreCase)) {
	  sigma->SetBinContent(i,j,params[p]);
	  sigma->SetBinError(i,j,parerr[p]);
	  //	  isigma = p;
	}
      }
      if (imu >= 0 && isigma >= 0) 
      printf("%i/%i %f/%f mean %f rms = %f entries = %f mu = %f sigma = %f chisq = %f prob = %f\n",
	    i,j,Fit.x,Fit.y,Fit.mean,Fit.rms,Fit.entries,Fit.params[imu],Fit.params[isigma],Fit.chisq,Fit.prob);
      if (FitP)  FitP->Fill(&Fit.i);
      if (canvas) {
	canvas->Update();
      }
      fOut->cd();
      proj->Write();
      SafeDelete(proj);
    }
  }
  fOut->cd();
  FitP->Write();
  mean->Write();
  rms->Write();
  entries->Write();
  mu->Write();
  sigma->Write();
  chisq->Write();
} 
//________________________________________________________________________________
void dEdxFit() {}
//________________________________________________________________________________
void dEdxFitSparse(THnSparse *hist, const Char_t *FitName = "GP", 
	     Option_t *opt="R", 
	     Int_t ix = -1, Int_t jy = -1, 
	     Int_t mergeX=1, Int_t mergeY=1, 
	     Double_t nSigma=3, Int_t pow=0,
	     Double_t zmin = -1, Double_t zmax = 2) {
  if (! hist) return;
  struct Fit_t {
    Float_t i;
    Float_t j;
    Float_t k[7];
    Float_t x;
    Float_t y;
    Float_t z[7];
    Float_t mean;
    Float_t rms;
    Float_t peak;
    Float_t mu;
    Float_t sigma;
    Float_t entries;
    Float_t chisq;
    Float_t prob;
    Float_t a0;
    Float_t a1;
    Float_t a2;
    Float_t a3;
    Float_t a4;
    Float_t a5;
    Float_t a6;
    Float_t Npar;
    Float_t dpeak;
    Float_t dmu;
    Float_t dsigma;
    Float_t da0;
    Float_t da1;
    Float_t da2;
    Float_t da3;
    Float_t da4;
    Float_t da5;
    Float_t da6;
    Float_t muJ;
    Float_t dmuJ;
  };
  TString varList("i:j");
  TString kS, zS;
  for (Int_t i = 0; i < 7; i++) {
    kS += ":k"; kS += i;
    zS += ":z"; zS += i;
  }
  varList += kS;
  varList += ":x:y";
  varList += zS;
  varList += ":mean:rms:peak:mu:sigma:entries:chisq:prob:a0:a1:a2:a3:a4:a5:a6:Npar:dpeak:dmu:dsigma:da0:da1:da2:da3:da4:da5:da6:muJ:dmuJ";
  Fit_t Fit;
  //  TString NewRootFile(gSystem->DirName(fRootFile->GetName()));
  TFile *fRootFile = (TFile *) gDirectory->GetFile();
  TString NewRootFile(gSystem->DirName(gSystem->BaseName(fRootFile->GetName())));
  NewRootFile += "/";
  NewRootFile += hist->GetName();
  NewRootFile += FitName;
  if (ix >= 0) NewRootFile += Form("_X%i",ix);
  if (jy >= 0) NewRootFile += Form("_Y%i",jy);
  if (mergeX != 1) NewRootFile += Form("_x%i",mergeX);
  if (mergeY != 1) NewRootFile += Form("_y%i",mergeY);
  NewRootFile += gSystem->BaseName(fRootFile->GetName());
  if (! FitP) {
    if (! fOut) {
      fOut = new TFile(NewRootFile.Data(),"recreate");
      if (fOut) cout << NewRootFile << " has been opened." << endl;
      else {cout << "Failed to open " << NewRootFile << endl; return;}
    }
    FitP = (TNtuple *) fOut->Get("FitP");
  }
  if (! FitP) {
    FitP = new TNtuple("FitP","Fit results",varList);
    FitP->SetMarkerStyle(20);
    FitP->SetLineWidth(2);
  }
  
  THnSparseProject ProjHS(hist,ix);
  Int_t Ndim = ProjHS.Ndim();
  //  TH1D **projs = new TH1D*[nbins]; memset (projs, 0, nbins*sizeof(TH1D*));
  //  ProjectSparse(hist, Ndim, Nbins,  projs);
  Double_t params[20] = {0};
  TH1 *proj = 0;
  TF1 *g = 0;
  while (proj = ProjHS.Next()) {
    if (! proj) continue;
    memset (&Fit, 0, sizeof(Fit_t));
    Float_t *idxF = &Fit.i;
    Float_t *xx   = &Fit.x;
    for (Int_t k = 0; k < Ndim - 1; k++) {
      idxF[k] = ProjHS.GetBins()[k];
      xx[k]   = ProjHS.GetVars()[k];
    }
    Fit.mean = proj->GetMean();
    Fit.rms  = proj->GetRMS();
    Fit.chisq = -100;
    Fit.prob  = 0;
    Fit.entries = proj->Integral();
    cout << "i/j\t" << Fit.i << "/" << Fit.j << "\t" <<  proj->GetName() << "\t" << proj->GetTitle() << "\tentries = " <<  Fit.entries<< endl;
    if (Fit.entries < 100 || TMath::IsNaN(Fit.entries) ) {
      if (FitP)  FitP->Fill(&Fit.i);
      delete proj; continue;
    }
    if (TString(FitName) == "GP") {
      g = FitGP(proj,opt,nSigma,0,zmin,zmax);
    } else if (TString(FitName) == "LN") { 
      g = FitLN(proj,opt,nSigma,0,zmin,zmax);
    }
    else if (TString(FitName) == "ADC") g = FitADC(proj,opt,nSigma,pow);
    else if (TString(FitName) == "G2") g = FitG2(proj,opt);
    else if (TString(FitName) == "GG") g = FitGG(proj,opt);
    else if (TString(FitName) == "Freq") g = FitFreq(proj,opt,zmin,zmax);
    else {cout << FitName << " has not been definded" << endl; break;}
    if ( g ) {
      Int_t kpeak = 0;
      if (TString(FitName) == "RL5" || TString(FitName) == "RL1") kpeak = 10;
      g->GetParameters(params);
      Fit.Npar  = g->GetNpar();
      Fit.chisq = g->GetChisquare();
      Fit.prob  = g->GetProb();
      Fit.peak = params[kpeak]; // norm, Mu for RL5
      Fit.mu = params[1];
      Fit.sigma = TMath::Abs(params[2]);
      Fit.a0  = params[3]; // FitGF "P"
      Fit.a1  = params[4]; //       "K"
      Fit.a2  = params[5]; //       "e"
      Fit.a3  = params[6]; //       "d"
      Fit.a4  = params[7]; //       "Total"
      Fit.a5  = params[8]; //       "Case", sigma of Landau for L5
      Fit.a6  = params[9]; //       "scale"
      Fit.dpeak  = g->GetParError(kpeak);
      Fit.dmu    = g->GetParError(1);
      Fit.dsigma = g->GetParError(2);
      Fit.da0    = g->GetParError(3);
      Fit.da1    = g->GetParError(4);
      Fit.da2	   = g->GetParError(5);
      Fit.da3	   = g->GetParError(6);
      Fit.da4	   = g->GetParError(7);
      Fit.da5	   = g->GetParError(8);
      Fit.da6	   = g->GetParError(9);
    } else {
      delete proj; continue;
    }
    printf("%i/%i %f/%f mean %f rms = %f entries = %f mu = %f sigma = %f chisq = %f prob = %f\n",
	   Fit.i,Fit.j,Fit.x,Fit.y,Fit.mean,Fit.rms,Fit.entries,Fit.mu,Fit.sigma,Fit.chisq,Fit.prob);
    if (FitP)  FitP->Fill(&Fit.i);
    fOut->cd();
    proj->Write();
    if (canvas) {
      canvas->Update();if (Ask()) goto ENDL;
      
    }
    SafeDelete(proj);
  }
 ENDL:
  fOut->cd();
  FitP->Write();
}
//________________________________________________________________________________
void dEdxFit(const Char_t *HistName,const Char_t *FitName = "GP", 
	     Option_t *opt="RM", 
	     Int_t ix = -1, Int_t jy = -1, 
	     Int_t mergeX=1, Int_t mergeY=1, 
	     Double_t nSigma=3, Int_t pow=1,
	     Double_t zmin = -2, Double_t zmax = 5) {
  TString Opt(opt);
  if (! Opt.Contains("Q",TString::kIgnoreCase)) {
    canvas = (TCanvas *) gROOT->GetListOfCanvases()->FindObject("Fit");
    if (! canvas) canvas = new TCanvas("Fit","Fit results");
    else          canvas->Clear();
  }
  TList *list = (TList *) gROOT->GetListOfFiles();
  if (! list) {printf("File list is empty\n"); return;}
  TIter next(list);
  TFile *fRootFile = (TFile *) next(); // gDirectory->GetFile();
  if (! fRootFile ) {printf("There is no opened file\n"); return;}
  TH1 *hist = 0;// = (TH1 *) fRootFile->Get(HistName);
  //  fRootFile->GetObject(HistName,hist);
  TString HName(HistName);
  TObjArray *objArray = HName.Tokenize("+");
  for (Int_t l = 0; l < objArray->GetEntries(); l++) {
    const Char_t *histName = ((TObjString *) objArray->At(l))->GetName();
    TObject *obj = fRootFile->Get(histName);
    if (!obj) {printf("Cannot find %s\n", histName); return;}
    if (obj->IsA()->InheritsFrom( "TH1" )) {
      if (! hist) {hist = (TH1 *) obj; hist->SetName(HistName);}
      else        {hist->Add((TH1 *) obj);}
    } else if (obj->IsA()->InheritsFrom( "THnSparse" )) {
      if (TString(histName).Contains("Time")) {
	if (! hist) hist = ((THnSparse *) obj)->Projection(1,0); 
	else        hist->Add(((THnSparse *) obj)->Projection(1,0));
      } else {
	THnSparse *sparse = dynamic_cast<THnSparse *>(obj);
	if (sparse) dEdxFitSparse(sparse, FitName,opt,ix);
	return;
      }
    }
  }
  if (! hist) return;
  TAxis *xax = hist->GetXaxis();
  Int_t nx = xax->GetNbins(); printf ("nx = %i",nx);
  Axis_t xmin = xax->GetXmin(); printf (" xmin = %f",xmin);
  Axis_t xmax = xax->GetXmax(); printf (" xmax = %f\n",xmax);
  TAxis *yax = hist->GetYaxis();
  Int_t dim = hist->GetDimension();
  Int_t ny = yax->GetNbins();
  Int_t NX = nx;
  Int_t NY = ny;
  if (dim < 3) ny = 1;  
  printf ("ny = %i",ny);
  Axis_t ymin = yax->GetXmin(); printf (" ymin = %f",ymin);
  Axis_t ymax = yax->GetXmax(); printf (" ymax = %f\n",ymax);
  Int_t IY = -1; // File type
  if (dim == 2) {
    TDirectory *dir = hist->GetDirectory();
    if (dir) {
      TString name(dir->GetName());
      name.ReplaceAll("_"," ");
      name.ReplaceAll(".root","");
      Int_t start = name.Index(" ");
      if (start < 0) start = 0;
      Int_t length = name.Length();
      TString Nn(name.Data()+start,length-start);
      Double_t YY = Nn.Atof();
      IY = YY;
#if 0
      Int_t nok = sscanf(name.Data(),"%*s %f",&YY);
      if (nok != 1) IY = -1;
      else          IY = YY;
#endif
    }
  }
  Fitx_t Fit;
  //  TString NewRootFile(gSystem->DirName(fRootFile->GetName()));
  TString NewRootFile(gSystem->BaseName(gSystem->DirName(fRootFile->GetName())));
  NewRootFile += "/";
  NewRootFile += HistName;
  NewRootFile += FitName;
  //  NewRootFile += Opt;
  if (ix >= 0) NewRootFile += Form("_X%i",ix);
  if (jy >= 0) NewRootFile += Form("_Y%i",jy);
  if (mergeX != 1) NewRootFile += Form("_x%i",mergeX);
  if (mergeY != 1) NewRootFile += Form("_y%i",mergeY);
  //  NewRootFile += "_2_";
  NewRootFile += gSystem->BaseName(fRootFile->GetName());
  if (! FitP) {
    if (! fOut) {
      fOut = new TFile(NewRootFile.Data(),"recreate");
      if (fOut) cout << NewRootFile << " has been opened." << endl;
      else {cout << "Failed to open " << NewRootFile << endl; return;}
    }
    FitP = (TNtuple *) fOut->Get("FitP");
  }
  if (! FitP) {
    FitP = new TNtuple("FitP","Fit results", Fitx_VarList);
    FitP->SetMarkerStyle(20);
    FitP->SetLineWidth(2);
  }
  TH1 *mean = (TH1 *) fOut->Get("mean");
  TH1 *rms  = (TH1 *) fOut->Get("rms");    
  TH1 *entries = (TH1 *) fOut->Get("entries");
  TH1 *mu   = (TH1 *) fOut->Get("mu");
  TH1 *muI  = 0;
  TH1 *muJ  = 0;
  TH1 *sigma= (TH1 *) fOut->Get("sigma");
  TH1 *chisq= (TH1 *) fOut->Get("chisq");
  if (! mu) {
    if (dim == 3) {
      mean    = new TH2D("mean",hist->GetTitle(),nx,xmin,xmax,ny,ymin,ymax);
      rms     = new TH2D("rms",hist->GetTitle(),nx,xmin,xmax,ny,ymin,ymax);
      entries = new TH2D("entries",hist->GetTitle(),nx,xmin,xmax,ny,ymin,ymax);
      mu      = new TH2D("mu",hist->GetTitle(),nx,xmin,xmax,ny,ymin,ymax);
      muI     = new TH2D("muI",hist->GetTitle(),nx,xmin,xmax,ny,ymin,ymax);
      muJ     = new TH2D("muJ",hist->GetTitle(),nx,xmin,xmax,ny,ymin,ymax);
      sigma   = new TH2D("sigma",hist->GetTitle(),nx,xmin,xmax,ny,ymin,ymax);
      chisq   = new TH2D("chisq",hist->GetTitle(),nx,xmin,xmax,ny,ymin,ymax);
    }
    else {
      if (dim == 2 || dim == 1) {
	mean    = new TH1D("mean",hist->GetTitle(),nx,xmin,xmax);
	rms     = new TH1D("rms",hist->GetTitle(),nx,xmin,xmax);
	entries = new TH1D("entries",hist->GetTitle(),nx,xmin,xmax);
	mu      = new TH1D("mu",hist->GetTitle(),nx,xmin,xmax);
	muI     = new TH1D("muI",hist->GetTitle(),nx,xmin,xmax);
	sigma   = new TH1D("sigma",hist->GetTitle(),nx,xmin,xmax);
	chisq   = new TH1D("chisq",hist->GetTitle(),nx,xmin,xmax);
      }
      else {
	printf("Histogram %s has wrong dimension %i\n", hist->GetName(),dim);
	return;
      }
    }
  }
  Double_t params[20] = {0};
  TH1 *proj = 0;
  TF1 *g = 0;
  Int_t ix1 = 0;
  Int_t ix2 = nx-mergeX+1;
  Int_t jy1 = 0;
  Int_t jy2 = ny-mergeY+1;
  if (ix > 0) ix1 = ix2 = ix;
  if (ny == 1) {
    jy1 = jy2 = 1;
  } else {
    if (jy > 0) {jy1 = jy2 = jy;}
  }
  for (int i=ix1;i<=ix2;i++){
    Int_t ir0 = i;
    Int_t ir1=i+mergeX-1;
    if (i == 0) {ir0 = 1; ir1 = nx;}
    for (int j=jy1;j<=jy2;j++){
      Int_t jr0 = j;
      Int_t jr1 = j+mergeY-1;
      if (j == 0) {jr0 = 1; jr1 = ny;}
      if (dim == 3) {
	if (j == 0) 
	  proj = ((TH3 *) hist)->ProjectionZ(Form("fX%i", ir0),ir0,ir1,1,ny); 
	else if (i == 0) 
	  proj = ((TH3 *) hist)->ProjectionZ(Form("fY%i", jr0),1, nx, jr0, jr1); 
	else if (ir0 == ir1 && jr0 == jr1) {
	  proj = ((TH3 *) hist)->ProjectionZ(Form("f%i_%i",      ir0,    jr0    ),ir0,ir1,jr0,jr1); 
	} else if (ir0 == ir1) {  
	  proj = ((TH3 *) hist)->ProjectionZ(Form("f%i_%i_%i",ir0, jr0,jr1),ir0,ir1,jr0,jr1);
	} else if (jr0 == jr1) {  
	  proj = ((TH3 *) hist)->ProjectionZ(Form("f%i_%i_%i",ir0,ir1,jr0),ir0,ir1,jr0,jr1);
	} else {  
	  proj = ((TH3 *) hist)->ProjectionZ(Form("f%i_%i_%i_%i",ir0,ir1,jr0,jr1),ir0,ir1,jr0,jr1);
	}
	if (! proj) continue;
	TString title(proj->GetTitle());
	title += Form(" in x [%5.1f,%5.1f] and y [%5.1f,%5.1f] range",
		      xax->GetBinLowEdge(ir0), xax->GetBinUpEdge(ir1),
		      yax->GetBinLowEdge(jr0), yax->GetBinUpEdge(jr1));
	proj->SetTitle(title.Data());
	cout<<"Histogram "<<proj->GetName()<<" was created"<<endl;
      }
      else {
	if (ir0 == ir1) 
	  {  proj = ((TH2 *) hist)->ProjectionY(Form("f%i",   ir0),    ir0,ir1);cout<<"Histogram "<<proj->GetName()<<" was created"<<endl;}
	else 
	  {  proj = ((TH2 *) hist)->ProjectionY(Form("f%i_%i",ir0,ir1),ir0,ir1);cout<<"Histogram "<<proj->GetName()<<" was created"<<endl;}
	if (! proj) continue;
	TString title(proj->GetTitle());
	title += Form(" in x [%5.1f,%5.1f] range",
		      xax->GetBinLowEdge(ir0), xax->GetBinUpEdge(ir1));
	proj->SetTitle(title.Data());
      }
      if      (Opt.Contains("B2",TString::kIgnoreCase)) proj->Rebin(2); 
      else if (Opt.Contains("B3",TString::kIgnoreCase)) proj->Rebin(3); 
      else if (Opt.Contains("B4",TString::kIgnoreCase)) proj->Rebin(4); 
      else if (Opt.Contains("B5",TString::kIgnoreCase)) proj->Rebin(5); 
      else if (Opt.Contains("B6",TString::kIgnoreCase)) proj->Rebin(6); 
      //      continue;
      memset (&Fit, 0, sizeof(Fitx_t));
      Fit.i = (2.*i+mergeX-1.)/2;
      Fit.j = (2.*j+mergeY-1.)/2;
      Fit.x = 0.5*(xax->GetBinLowEdge(i) + xax->GetBinUpEdge(i+mergeX-1));
      Fit.y = 0.5*(yax->GetBinLowEdge(j) + yax->GetBinUpEdge(j+mergeY-1));
      if (IY > 0) Fit.y = IY;
      Fit.mean = proj->GetMean();
      Fit.rms  = proj->GetRMS();
      Fit.chisq = -100;
      Fit.prob  = 0;
      Fit.entries = proj->Integral();
      Int_t kx1 = proj->FindFirstBinAbove(1e-5);
      Int_t kx2 = proj->FindLastBinAbove(1e-5);
      cout << "i/j\t" << i << "/" << j << "\t" <<  proj->GetName() << "\t" << proj->GetTitle() << "\tentries = " <<  Fit.entries<< " k1,2 " << kx1 << "," << kx2 << endl;
      if (kx1 < kx2) proj->GetXaxis()->SetRange(kx1,kx2);
      if (Fit.entries < 300 || 
	  TMath::IsNaN(Fit.entries) || 
	  kx2 - kx1 <= 3 ) {
	delete proj; continue;
      }
      if (TString(FitName) == "G0P") {
	if (! i) {
	  delete proj; continue;
	}
	g = FitG0P(proj,opt);
      } else if (TString(FitName) == "GP") {
	if (TString(HistName).Contains("TPoint")) {
	  g = FitGP(proj,opt,3,-1,-1,1);
	} else {
	  g = FitGP(proj,opt,nSigma,pow,zmin,zmax);
	}
      } else if (TString(FitName) == "LN") { 
	Double_t SX = -1;
#if 1
	/*
	  Attaching file nePI+nePOLNAdcSparseD3.root as _file0...
	  FitP->Draw("sigma:x>>SX","(i&&j&&abs(mu)<1&&dsigma<02e-3&&dmu<2e-3)/dsigma**2","profg")
	  root.exe [14] SX->Fit("pol2","+")

	  ****************************************
	  Minimizer is Linear
	  Chi2                      =  6.91369e+21
	  NDf                       =           11
	  p0                        =      1.07622   +/-   2.02152e-12 
	  p1                        =    -0.209312   +/-   7.4276e-13  
	  p2                        =    0.0104733   +/-   6.79137e-14 
	*/
	if (Fit.i > 0) {
	  //	  SX = TMath::Exp(8.86919e-01 -4.26364e-01*Fit.x);
	  SX =  1.07622  + Fit.x*(-0.209312 + Fit.x*0.0104733);
	}
#endif
	g = FitLN(proj,opt,nSigma,0,-2,5,SX);
      }      
      else if (TString(FitName) == "ADC") g = FitADC(proj,opt,nSigma,pow);
      else if (TString(FitName) == "G")   g = FitG(proj,opt);
      else if (TString(FitName) == "G2") g = FitG2(proj,opt);
      else if (TString(FitName) == "CB") g = FitCB(proj,opt);
      else if (TString(FitName) == "GE") g = FitGE(proj,opt);
      else if (TString(FitName) == "GE2") g = FitGE2(proj,opt, Fit.x);
      else if (TString(FitName) == "GE3") g = FitGE3(proj,opt, Fit.x);
      else if (TString(FitName) == "GE4") g = FitGE4(proj,opt, Fit.x);
      else if (TString(FitName) == "GEX") g = FitGEX(proj,opt, &Fit);
      else if (TString(FitName) == "GEX1") g = FitGEX1(proj,opt, &Fit);
      else if (TString(FitName) == "GEX2") g = FitGEX2(proj,opt, &Fit);
      else if (TString(FitName) == "GEX3") g = FitGEX3(proj,opt, &Fit);
      else if (TString(FitName) == "GG") g = FitGG(proj,opt);
      else if (TString(FitName) == "GG2") g = FitGG2(proj,opt, Fit.x, Fit.y);
      else if (TString(FitName) == "GG3") g = FitGG3(proj,opt, Fit.x, Fit.y);
      else if (TString(FitName) == "GG4") g = FitGG4(proj,opt, Fit.x, Fit.y);
      else if (TString(FitName) == "GG5") {
	Int_t IO = 1;
	if (HName.BeginsWith("I") || HName.EndsWith("I"))   IO = 0;
	g = FitGG5(proj,opt, Fit.x, IO);
      }
      else if (TString(FitName) == "Freq") g = FitFreq(proj,opt,zmin,zmax);
      else if (TString(FitName) == "GF") g = FitGF(proj,opt);
      else if (TString(FitName) == "G4F") g = FitG4F(proj,opt);
      else if (TString(FitName) == "G4E" || TString(FitName) == "G4EX" || TString(FitName) == "G4EY" || TString(FitName) == "G4EG") {
	Int_t Sign = 2;
	Int_t IO = 1;
	if (dim == 3) {
	  Sign = 0;
	  if (HName.Contains("3P")) {
	    if (HName.Contains("+")) Sign = 2;
	    else                     Sign = 1;
	  } 
	  if (nx == 72 || nx == 145) {
	    IO = 0;
	    if (TMath::Abs(Fit.x) > 40.5) IO = 1;
	  } else if (ny == 72 || ny == 145) {
	    IO = 0;
	    if (TMath::Abs(Fit.y) > 40.5) IO = 1;
	  } else if (ny == 2*24*20) {// xyPad3qB
	    Sign = 2; // "+" & "-"
	    Double_t y = Fit.y - TMath::Nint(Fit.y);
	    IO = 0;
	    if (y > 0) IO = 1;
	  }
	}
	if (TString(FitName) == "G4E")  	g = FitG4E(proj,opt,IO, Sign);
	if (TString(FitName) == "G4EX") 	g = FitG4EX(proj,opt,IO, Sign);
	if (TString(FitName) == "G4EY") 	g = FitG4EY(proj,opt,IO, Sign);
	if (TString(FitName) == "G4EG") 	g = FitG4EG(proj,opt,IO, Sign);
      }
      else if (TString(FitName) == "L5") g = FitL5(proj,opt,5);
      else if (TString(FitName) == "L1") g = FitL5(proj,opt,0);
#ifdef __USE_ROOFIT__
      else if (TString(FitName) == "R1") g  = FitR5(proj,opt,0);
      else if (TString(FitName) == "R5") g  = FitR5(proj,opt,5);
      else if (TString(FitName) == "RL5")
      {
	Bool_t outer = kFALSE;
	if      (NX == 45) {outer = i > 13;}
	else if (NY == 45) {outer = j > 13;}
	g = FitRL5(proj,outer);
	//	g->Print();
	//	cout<<"TEST (SELEKCJA) : i = "<<i<<", j = "<<j<<", outer = "<<outer<<endl;
      }
      else if (TString(FitName) == "RL1") g = FitRL1(proj);
#endif /* __USE_ROOFIT__ */
      else if (TString(FitName) == "GB") {
	Double_t dX = 2.0; // <dX> Outer
	g = FitGB(proj,opt,dX);
      }
      else {cout << FitName << " has not been definded" << endl; break;}
      if ( g ) {
	Int_t kpeak = 0;
	if (TString(FitName) == "RL5" || TString(FitName) == "RL1") kpeak = 10;
	g->GetParameters(params);
	Int_t Npar = g->GetNpar();
	Fit.Npar  = Npar;
	Fit.chisq = g->GetChisquare();
	Fit.prob  = g->GetProb();
	Fit.peak = params[kpeak]; // norm, Mu for RL5
	Fit.dpeak  = g->GetParError(kpeak);
	Fit.mu = params[1];
	Fit.sigma = TMath::Abs(params[2]);
	Float_t *apar = &Fit.NormL;
	Float_t *dapar = &Fit.dNormL;
	for (Int_t i = 0; i < Npar; i++) {
	  apar[i] = params[i];
	  dapar[i] = g->GetParError(i);
	}
      } else {
	delete proj; continue;
      }
      //      Fit.chisq = g3->GetChisquare();
      //      if (Fit.prob > 0) {
      if (dim == 3) {
	mean->SetBinContent(i,j,Fit.mean);
	rms->SetBinContent(i,j,Fit.rms);
	entries->SetBinContent(i,j,Fit.entries);
	mu->SetBinContent(i,j,Fit.mu);
	mu->SetBinError(i,j,g->GetParError(1));
	sigma->SetBinContent(i,j,Fit.sigma);
	sigma->SetBinError(i,j,g->GetParError(2));
	chisq->SetBinContent(i,j,Fit.chisq);
	if (j) {
	  Fit.muJ = mu->GetBinContent(i,0);
	  Fit.dmuJ = mu->GetBinError(i,0);
	}
      } else {
	mean->SetBinContent(i,Fit.mean);
	rms->SetBinContent(i,Fit.rms);
	entries->SetBinContent(i,Fit.entries);
	mu->SetBinContent(i,Fit.mu);
	mu->SetBinError(i,g->GetParError(1));
	sigma->SetBinContent(i,Fit.sigma);
	sigma->SetBinError(i,g->GetParError(2));
	chisq->SetBinContent(i,Fit.chisq);
	}
      //      }
      printf("%i/%i %f/%f mean %f rms = %f entries = %f mu = %f sigma = %f chisq = %f prob = %f\n",
	    i,j,Fit.x,Fit.y,Fit.mean,Fit.rms,Fit.entries,Fit.mu,Fit.sigma,Fit.chisq,Fit.prob);
      if (FitP)  FitP->Fill(&Fit.i);
      fOut->cd();
      proj->Write();
      if (canvas) {
	canvas->Update();
	if (Ask()) goto ENDL;
      }
      SafeDelete(proj);
    }
  }
 ENDL:
  fOut->cd();
  FitP->Write();
  mean->Write();
  rms->Write();
  entries->Write();
  mu->Write();
  sigma->Write();
  chisq->Write();
  if (muI && muJ) {
    ShiftMu((TH2 *) mu, muI, muJ);
    muI->Write();
    muJ->Write();
  } else if (muI) {
    ShiftMu(mu,muI);
    muI->Write();
  }
} 
