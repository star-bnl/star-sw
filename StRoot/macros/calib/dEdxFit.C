/* 
   root.exe -q -b lBichsel.C pionMIP.root 'dEdxFit.C+("SecRow3C","GF")'
*/
#if !defined(__CINT__)
// code that should be seen ONLY by the compiler
#else
#if !defined(__CINT__) || defined(__MAKECINT__)
// code that should be seen by the compiler AND rootcint
#else
// code that should always be seen
#endif
#endif
#if ROOT_VERSION_CODE >= ROOT_VERSION(5,34,18)
//#define __USE_ROOFIT__
#endif
//________________________________________________________________________________
#if !defined(__CINT__) || defined(__MAKECINT__)
#include "Riostream.h"
#include <stdio.h>
#include "TROOT.h"
#include "TSystem.h"
#include "TMath.h"
#include "TH1.h"
#include "TH2.h"
#include "TH3.h"
#include "THnSparse.h"
#include "TStyle.h"
#include "TF1.h"
#include "TProfile.h"
#include "TTree.h"
#include "TChain.h"
#include "TFile.h"
#include "TNtuple.h"
#include "TFitResult.h"
#include "TCanvas.h"
#include "TFileSet.h"
#include "TDataSetIter.h"
#include "TDataSet.h"
#include "TClassTable.h"
//#include "DeDxTree.C"
#include "TMinuit.h"
#include "TSpectrum.h"
#include "StBichsel/Bichsel.h"
#include "StBichsel/StdEdxModel.h"
#include "TString.h"
#include "TLine.h"
#include "TText.h"
#include "TList.h"
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
using namespace RooFit ;
#endif /* __USE_ROOFIT__ */
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
// peak postion at p = 0.475 GeV/c wrt pion
//                        Z     pion
/// <peak postion> at p = [0.45,0.50] GeV/c wrt pion
struct peak_t {Double_t peak, sigma, mass; const Char_t *Name;};
//                                mean         RMS
static const  peak_t Peaks[6] = {
//   {0.      ,       0., 0.13956995, "pion"}, // pion
//   {1.425822, 0.101693, 0.93827231, "proton"}, // proton - pion
//   {0.565455, 0.061626, 0.493677,   "kaon"  }, // Kaon   - pi
//   {0.424916, 0.004081, 0.51099907e-3,"e"}, // e      - pi
//   {2.655586, 0.123754, 1.875613,   "d"}, // d      - pi
//   {0.004178, 0.002484, 0.105658,   "mu"}};// mu     - pi
  // 06/25/10
  {       0.      ,       0.,           0.13956995,       "pion"}, // pion
  {       1.42574,        0.101741,       0.938272,       "proton"},
  {       0.565411,       0.0616611,      0.493677,       "kaon"},
  {       0.424919,       0.00408318,     0.000510999,    "e"},
  {       2.65548,        0.123809,       1.87561,        "deuteron"},
  {       0.000717144,    0.00490783,     0.105658,       "mu"}};

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
  static TPolyMarker *pm = 0;
  if (pm) delete pm;
  pm = new TPolyMarker(1, &X, &Y);
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
  if (! proj) return 0;
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
TF1 *FitGP(TH1 *proj, Option_t *opt="RQ", Double_t nSigma=3, Int_t pow=3) {
  if (! proj) return 0;
  TString Opt(opt);
  //  Bool_t quet = Opt.Contains("Q",TString::kIgnoreCase);
  TF1 *g = 0, *g0 = 0;
  TF1 *gaus = (TF1*) gROOT->GetFunction("gaus");
  if (pow >= 0) g0 = new TF1("g0",Form("gaus(0)+pol%i(3)",pow),-0.2,0.2);
  else          g0 = new TF1("g0","gaus",-0.2,0.2); 
  g0->SetParName(0,"Constant");
  g0->SetParName(1,"Mean");
  g0->SetParName(2,"Sigma");
  for (int i=0; i<=pow;i++) g0->SetParName(3+i,Form("a%i",i));
  TF1 *g1 = new TF1("g1",Form("gaus(0)+pol%i(3)",pow+1),-0.2,0.2);
  g1->SetParName(0,"Constant");
  g1->SetParName(1,"Mean");
  g1->SetParName(2,"Sigma");
  for (int i=0; i<=pow+1;i++) g1->SetParName(3+i,Form("a%i",i));
  TF1 *g2 = new TF1("g2",Form("gaus(0)+pol%i(3)",pow+2),-0.2,0.2);
  g2->SetParName(0,"Constant");
  g2->SetParName(1,"Mean");
  g2->SetParName(2,"Sigma");
  for (int i=0; i<=pow+2;i++) g2->SetParName(3+i,Form("a%i",i));
  Double_t params[9];
  Int_t peak = proj->GetMaximumBin();
  Double_t peakX = proj->GetBinCenter(peak);
  params[0] = proj->GetBinContent(peak);
  if (peakX > 0.5) {
    params[1] = 0;
    params[2] = 0.2;
  }
  else {
    params[1] = peakX;
    params[2] = proj->GetRMS();
    if (params[2] > 0.25) params[2] = 0.25;
  }
  params[3] = 0;
  params[4] = 0;
  params[5] = 0;
  params[6] = 0;
  params[7] = 0;
  params[8] = 0;
  if (gaus) {
    g = gaus;
    g->SetParameters(params);
    g->SetRange(params[1]-nSigma*params[2],params[1]+nSigma*params[2]);
    proj->Fit(g,opt);
    g->GetParameters(params);
    if (g->GetProb() > 0.01) return g;
    params[2] = TMath::Abs(params[2]);
  }
  g = g0;
  g->SetParameters(params);
  g->SetRange(params[1]-nSigma*params[2],params[1]+nSigma*params[2]);
  proj->Fit(g,opt);
  if (g->GetProb() > 0.01) return g;
  g->GetParameters(params);
  g = g1;
  params[2] = TMath::Abs(params[2]);
  g->SetParameters(params);
  g->SetRange(params[1]-nSigma*params[2],params[1]+nSigma*params[2]);
  proj->Fit(g,opt);
  if (g->GetProb() > 0.01) return g;
  g->GetParameters(params);
  g = g2;
  params[2] = TMath::Abs(params[2]);
  g->SetParameters(params);
  g->SetRange(params[1]-nSigma*params[2],params[1]+nSigma*params[2]);
  proj->Fit(g,opt);
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
  Int_t icase = (Int_t) par[8];
  Int_t i1 = 0;
  Int_t i2 = 4;
  if (icase >= 0) {i1 = i2 = icase;}
  for (i = i1; i <= i2; i++) { 
    Double_t Sigma = TMath::Sqrt(sigma*sigma + Peaks[i].sigma*Peaks[i].sigma);
    Value += frac[i]*TMath::Gaus(x[0],par[1]+Peaks[i].peak,Sigma,1);
    //    cout << "i\t" << i << "\tx = " << x[0] << " frac " << frac[i] << "\t" << Value << endl;
  }
  return par[7]*TMath::Exp(par[0])*Value;
}
//________________________________________________________________________________
TF1 *FitGF(TH1 *proj, Option_t *opt="") {
  static TSpectrum *fSpectrum = 0;
  if (! fSpectrum) {
    fSpectrum = new TSpectrum(6);
  }
  // fit in momentum range p = 0.45 - 0.50 GeV/c
  if (! proj) return 0;
  TString Opt(opt);
  //  Bool_t quet = Opt.Contains("Q",TString::kIgnoreCase);
  TF1 *g2 = (TF1*) gROOT->GetFunction("GF");
  if (! g2) {
    g2 = new TF1("GF",gfFunc, -5, 5, 9);
    g2->SetParName(0,"norm"); g2->SetParLimits(0,-80,80);
    g2->SetParName(1,"mu");     g2->SetParLimits(1,-2.5,2.5);
    g2->SetParName(2,"Sigma");  g2->SetParLimits(2,0.2,0.8);
    g2->SetParName(3,"P");      g2->SetParLimits(3,0,0.5);
    g2->SetParName(4,"K");      g2->SetParLimits(4,0.0,0.5);
    g2->SetParName(5,"e");      g2->SetParLimits(5,0.0,0.5);
    g2->SetParName(6,"d");      g2->SetParLimits(6,0.0,0.5);
    g2->SetParName(7,"Total");
    g2->SetParName(8,"Case");
    //    g2->SetParName(7,"factor"); g2->SetParLimits(7,-.1,0.1);
  }
  // Find pion peak
  Int_t nfound = fSpectrum->Search(proj);
  if (nfound < 1) return 0;
  Int_t npeaks = 0;
  Float_t *xpeaks = fSpectrum->GetPositionX();
  Float_t xp = 0;
  if (nfound > 2) nfound = 2;
  Double_t xpi = 9999;
  for (Int_t p = 0; p < nfound; p++) {
    xp = xpeaks[p];
    Int_t bin = proj->GetXaxis()->FindBin(xp);
    Double_t yp = proj->GetBinContent(bin);
    Double_t ep = proj->GetBinError(bin);
    if (yp-5*ep < 0) continue;
    if (xp < xpi) xpi = xp;
  }
  Double_t total = proj->Integral()*proj->GetBinWidth(5);
  //  g2->SetParameters(0, proj->GetMean(), proj->GetRMS(), 0.1, 0.1, 0.1, 0.1,0.1,-1.);
//   Int_t binmax = proj->GetMaximumBin();
//   Double_t xmax = proj->GetXaxis()->GetBinCenter(binmax);
  g2->SetParameters(0, xpi, 0.35, 0.6, 0.1, 0.1, 0.1,0.1,-1.);
  //  g2->FixParameter(3,2.86731e-01);
  g2->FixParameter(4,1e-6);
  g2->FixParameter(5,1e-6);
  g2->FixParameter(6,1e-6);
  g2->FixParameter(7,total);
  g2->FixParameter(8,-1);
  proj->Fit(g2,Opt.Data());
  g2->ReleaseParameter(3); g2->SetParLimits(3,0.0,TMath::Pi()/2);
  g2->ReleaseParameter(4); g2->SetParLimits(4,0.0,TMath::Pi()/2);
  g2->ReleaseParameter(5); g2->SetParLimits(5,0.0,TMath::Pi()/2);
  g2->ReleaseParameter(6); g2->SetParLimits(6,0.0,TMath::Pi()/2);
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
  TCanvas *canvas = new TCanvas("canvas","canvas");
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
Int_t FitG(TH1 *proj, TF1 *g, TF1 *ga){//, Double_t scaleM=-2., Double_t scaleP=2.) {
  Double_t params[10];
  proj->Fit("g","R");
  Double_t chisq = g->GetChisquare();
  if (chisq <= 0. || chisq > 1.e10) return -1;
  g->GetParameters(params);
  params[3] = 0;
  params[4] = 0;
  params[5] = 0;
  params[6] = 0;
  params[7] = 0;
  params[8] = 0;
  params[9] = 0;
  ga->SetParameters(params);
  proj->Fit("ga","r");
  //  proj->Fit("ga","rIM");
  chisq = g->GetChisquare();
  if (chisq <= 0. || chisq > 1.e10) return -1;
  return 0;
}
//________________________________________________________________________________
Int_t FitGG(TH1 *proj, TF1 *g1, TF1 *g2=0, TF1 *ga2=0, Double_t scaleM=-2., Double_t scaleP=2.) {
  Double_t params[9];
  proj->Fit("g1","R");
  g1->GetParameters(params);
  Double_t chisq = g1->GetChisquare();
  if (chisq <= 0. || chisq > 1.e10) return -1;
  params[3] = 1;
  params[4] = params[1]+0.1;
  params[5] = params[2];
  g2->SetParameters(params);
  proj->Fit("g2");
  chisq = g2->GetChisquare();
  if (chisq <= 0. || chisq > 1.e10) return -1;
  g2->GetParameters(params);
  if (ga2) {
    params[6] = 0;
    params[7] = 0;
    params[8] = 0;
    ga2->SetParameters(params);
    proj->Fit("ga2","R");
    ga2->GetParameters(params);
    ga2->SetRange(params[1]+scaleM*params[2],params[1]+scaleP*params[2]);
    proj->Fit("ga2","R");
  }
//   if (gPad) {
//     gPad->Update();
//     gPad->WaitPrimitive();
//   }
  return 0;
}
//________________________________________________________________________________
void FitX(TH2 *hist=0, Double_t range=1, Int_t Ibin = 0) {
  TFile *fRootFile = (TFile *) gDirectory->GetFile();
  if (! fRootFile ) {printf("Cannot find/open %s",fRootFile->GetName()); return;}
  if (!hist) {
    const Char_t *HistName = "FPoints";
    hist = (TH2D *) fRootFile->Get(HistName);
    if (!hist) {printf("Cannot histogram %s\n",HistName); return;}
  }
  //  const Int_t nx = hist->GetNbinsX();
  TF1 *g = new TF1("g","gaus",-range,range);
  //  TF1 *ga = g;
  TString fitN("gaus(0)+exp(pol3(3))");
  if (range > 2.) fitN = "gaus(0)+exp(pol5(3))";
  TF1 *ga= new TF1("ga",fitN.Data(),-range,range);
  ga->SetParName(0,"Area  Pi");
  ga->SetParName(1,"Mean  Pi"); 
  ga->SetParName(2,"Sigma Pi");
  ga->SetParName(3,"A0");
  ga->SetParName(4,"A1");
  ga->SetParName(5,"A2");
  ga->SetParName(6,"A3");
  ga->SetParName(7,"A4");
  ga->SetParName(8,"A5");
  
  TCanvas *c = new TCanvas("Fit","Fit results");
  int i;
  TString name(hist->GetName());
  name += "MuFG";
  Int_t nBins = hist->GetXaxis()->GetNbins();
  Double_t xlow = hist->GetXaxis()->GetXmin();
  Double_t xup  = hist->GetXaxis()->GetXmax();
  TH1D *MuF = new TH1D(name.Data(),"Avarage shift versus no. of measurement points",
		       nBins,  xlow, xup);
  name = hist->GetName();
  name += "SigmaFG";
  TH1D *SigmaF = new TH1D(name.Data(),"Sigma of z versus no. of measurement points",
		       nBins,  xlow, xup);
  Int_t i1 = 1, i2 = nBins;
  if (Ibin > 0 && Ibin <= nBins) {i1 = Ibin; i2 = Ibin;}
  Double_t XFitP, dXFitP,  MuFitP,dMuFitP,SigmaFitP,dSigmaFitP;
  TH1 *proj = 0;
  for (i=i1; i<=i2; i++) {
    if (proj) delete proj;
    proj = hist->ProjectionY("proj",i,i);
    XFitP = hist->GetXaxis()->GetBinCenter(i);
    dXFitP = hist->GetXaxis()->GetBinWidth(i);
    Double_t chisq = -999;
    if (proj->Integral() < 1000) {
      if (N>0) goto NEXT;
      continue;
    }
    if (FitG(proj,g,ga)) goto NEXT;
    //    proj->Fit("g","R");
    if (ga->GetParError(1) <= 0 || ga->GetParError(1) > 0.01 ) {
      printf("============= REJECT ================\n");
      goto NEXT;
    }
    MuFitP  = ga->GetParameter(1);
    dMuFitP = ga->GetParError(1);///MuFitP;
    //    MuFitP = TMath::Log(MuFitP);
    SigmaFitP  = ga->GetParameter(2);
    dSigmaFitP = ga->GetParError(2);
    chisq = ga->GetChisquare();
    if (chisq > 1.e4) goto NEXT; 
    MuF->SetCellContent(i,0,MuFitP);
    MuF->SetCellError(i,0,dMuFitP);
    SigmaF->SetCellContent(i,0,SigmaFitP);
    SigmaF->SetCellError(i,0,dSigmaFitP);
    proj->Draw(); c->Update();
    printf("Bin: %i x: %f +/- %f MuF: %f+/-%f Sigma: %f+/-%f chisq: %f \n",
	   i,XFitP,dXFitP,MuFitP,dMuFitP,SigmaFitP,dSigmaFitP,chisq);
 NEXT:   
    N++;  
    //    delete proj;
  }
  if (! Ibin ) {
    c->Clear();
    //   c->Divide(2,1);
    //   c->cd(1);
    //   MuF->Draw();
    //   c->cd(2);
    SigmaF->SetMarkerStyle(20);
    TF1 *p = new TF1("p","[0]/pow(x,[1])");
    p->SetParameter(0,0.64);
    p->SetParameter(1,0.50);
    SigmaF->Fit("p","R");
    SigmaF->SetMinimum(0.06);
    SigmaF->Draw();
  }
  TString NewRootFile(hist->GetName());
  NewRootFile += fRootFile->GetName();
  TFile *f = new TFile(NewRootFile.Data(),"update");
  MuF->Write();
  SigmaF->Write();
  delete f;
  fRootFile->cd();
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
  TCanvas *canvas = new TCanvas("canvas","canvas");
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
Double_t gNFunc(Double_t *x=0, Double_t *par=0) {
  static Double_t sigmaOLD = -1;
  static Double_t fracpiOld = -1;
  if (! x || ! par) {
    sigmaOLD = -1;
    fracpiOld = -1;
    return 0;
  }
  // par[0] - norm
  // par[1] - pion position wrt Z_pion (Bichsel prediction)
  // par[2] - sigma 
  // par[3] - proton signal
  // par[4] - Kaon    -"-
  // par[5] - electorn -"-
  // par[6] - deuteron -"-
  // par[7] - Total
#ifdef __HEED_MODEL__
  // par[8] - case
  // par[9] - scale
  // par[10]- row
  Int_t row = par[10];
  StdEdxModel::ESector kTpcOuterInner = StdEdxModel::kTpcOuter;
  if (row > 0 && row <= 13) kTpcOuterInner = StdEdxModel::kTpcInner;
#endif /* __HEED_MODEL__ */
  // ratio dN/dx_h / dN/dx_pi:      P        K        e        d
  static Double_t dNdxR[4] = {1.97273, 1.32040, 1.16313, 3.42753};
  static Int_t _debug = 0;
  static TCanvas *c1 = 0;
  if (_debug) {
    c1 = (TCanvas *) gROOT->GetListOfCanvases()->FindObject("c1");
    if (! c1) c1 = new TCanvas();
    else      c1->Clear();
    c1->cd();
  }
  enum {NFIT_HYP = 5, NT=6};
  static TH1D *hists[6] = {0};
  static Int_t    icaseOLD = -1;
  static Double_t meanPion = -1, RMSPion = -1, mpvPion = -1;
  static Double_t ln10 = TMath::Log(10.);
  Double_t sigma = par[2];
  if (sigma != sigmaOLD) {
#ifndef __HEED_MODEL__
    TF1 *zdE = StdEdxModel::instance()->zdEdx();  
#else /* __HEED_MODEL__ */
    TF1 *zdE = StdEdxModel::instance()->zdEdx(kTpcOuterInner);  
#endif /* __HEED_MODEL__ */
    sigmaOLD = sigma;
    fracpiOld = -1;
    static const Char_t *names[5] = {"pi","P","K","e","d"};
    for (Int_t i = 0; i < NFIT_HYP; i++) {
      if (hists[i]) hists[i]->Reset();
      else          {
	hists[i] = new TH1D(Form("dE%s",names[i]),Form("Expected dE for %s",names[i]),100,-5,5);
	hists[i]->SetMarkerColor(i+1);
	hists[i]->SetLineColor(i+1);
      }
      Double_t entries = projNs[i]->GetEntries();
      Double_t mean    = projNs[i]->GetMean();
#ifndef __HEED_MODEL__
      Double_t mpv     = StdEdxModel::instance()->zMPV()->Eval(mean,sigma);
#else /* __HEED_MODEL__ */
      Double_t mpv     = StdEdxModel::instance()->zMPV(kTpcOuterInner)->Eval(mean,sigma);
#endif /* __HEED_MODEL__ */
      Double_t RMS     = projNs[i]->GetRMS();
      if (i == 0) {
	meanPion = mean;
	mpvPion  = mpv;
	RMSPion  = RMS;
      }
      Int_t nx = projNs[i]->GetNbinsX();
      for (Int_t ix = 1; ix <= nx; ix++) {
	Double_t v =  projNs[i]->GetBinContent(ix);
	if (v > 0.0) {
#ifndef __HEED_MODEL__
	  Double_t n_PL10 = projNs[i]->GetBinCenter(ix);
	  Double_t n_P = TMath::Exp(n_PL10*ln10);
#else /* __HEED_MODEL__ */
	  Double_t n_PL = projNs[i]->GetBinCenter(ix);
	  Double_t n_P = TMath::Exp(n_PL);
#endif /* __HEED_MODEL__ */
	  Double_t Sigma = TMath::Sqrt(sigma*sigma + 1./n_P);
	  zdE->SetParameter(1, n_P);
	  zdE->SetParameter(2,mpv - mpvPion);
	  zdE->SetParameter(3,Sigma);
	  hists[i]->Add(zdE,v/entries);
	}
      }
      if (c1) {
	hists[i]->Draw();
	c1->Update();
      }
    }
    if (! hists[NFIT_HYP]) hists[NFIT_HYP] = new TH1D("dEAll","Expected dE for All",100,-5,5);
  }
  Double_t frac[NFIT_HYP] = {0};
  static Double_t fracOld[6] = {-1};
  Int_t i;
  frac[0] = 1;
  Bool_t updatedFractions = kFALSE;
  for (i = 1; i < NFIT_HYP; i++) {
    frac[i] = TMath::Sin(par[2+i]);
    frac[i] *= frac[i];
    if (TMath::Abs(frac[i]-fracOld[i]) > 1e-7) {
      fracOld[i] = frac[i];
      updatedFractions = kTRUE;
    }
    frac[0] -= frac[i];
  }
  if (fracOld[0] != frac[0]) updatedFractions = kTRUE;
  Int_t icase = (Int_t) par[8];
  Int_t i1 = 0;
  Int_t i2 = NFIT_HYP - 1;
  if (icase >= 0) {i1 = i2 = icase;}
  if (icase != icaseOLD || updatedFractions) {
    icaseOLD = icase;
    fracpiOld = frac[0];
    hists[NFIT_HYP]->Reset();
    for (i = i1; i <= i2; i++) { 
      if (frac[i] > 1e-7) {
	hists[NFIT_HYP]->Add(hists[i],  frac[i]);
      }
    }
    if (c1) {
      hists[NFIT_HYP]->Draw();
      c1->Update();
    }
  }  
#ifndef __HEED_MODEL__
  Double_t Value = par[7]*TMath::Exp(par[0])*hists[NFIT_HYP]->Interpolate(x[0]-par[1]);
#else /* __HEED_MODEL__ */
  Double_t Value = par[7]*TMath::Exp(par[0])*hists[NFIT_HYP]->Interpolate(par[9]*(x[0]-par[1]));
#endif /* __HEED_MODEL__ */
  return Value;
}
//________________________________________________________________________________
#ifndef __HEED_MODEL__
TF1 *FitNF(TH1 *proj, Option_t *opt) {// fit with no. of primary clusters
#else /* __HEED_MODEL__ */
TF1 *FitNF(TH1 *proj, Option_t *opt, Int_t row) {// fit with no. of primary clusters
#endif /* __HEED_MODEL__ */
  for (Int_t ih = 0; ih < 5; ih++) {
    if (! projNs[ih]) {
      cout << "projNs[" << ih << "] is not defined. Abort." << endl;
      return 0;
    }
  }
  static TSpectrum *fSpectrum = 0;
  if (! fSpectrum) {
    fSpectrum = new TSpectrum(6);
  }
  // fit in momentum range p = 0.45 - 0.50 GeV/c
  if (! proj) return 0;
  TString Opt(opt);
  //  Bool_t quet = Opt.Contains("Q",TString::kIgnoreCase);
  TF1 *g2 = (TF1*) gROOT->GetFunction("GN");
  enum {NFIT_HYP = 5}; // ignore e and d
  if (! g2) {
#ifndef __HEED_MODEL__
    g2 = new TF1("GN",gNFunc, -5, 5, 9);
    g2->SetParName(0,"norm"); g2->SetParLimits(0,-80,80);
#else /* __HEED_MODEL__ */
    g2 = new TF1("GN",gNFunc, -5, 5, 11);
    g2->SetParName(0,"norm");   g2->SetParLimits(0,-80,80);
#endif /* __HEED_MODEL__ */
    g2->SetParName(1,"mu");     g2->SetParLimits(1,-2.5,2.5);
    g2->SetParName(2,"Sigma");  g2->FixParameter(2, 0); // g2->SetParLimits(2,0.,0.5);
    g2->SetParName(3,"P");      g2->SetParLimits(3,0.0,TMath::Pi()/2);
    g2->SetParName(4,"K");      g2->SetParLimits(4,0.0,0.30);
    g2->SetParName(5,"e");      g2->SetParLimits(5,0.0,0.30); g2->FixParameter(5,0); 
    g2->SetParName(6,"d");      g2->SetParLimits(6,0.0,0.30); g2->FixParameter(6,0);
    //    g2->SetParName(6,"ScaleL"); g2->SetParLimits(6,-2.5,2.5);
    g2->SetParName(7,"Total");
    g2->SetParName(8,"Case");
#ifdef __HEED_MODEL__
    g2->SetParName(9,"Scale"); g2->SetParameter(9,1); g2->SetParLimits(9,0.5,1.5);
    g2->SetParName(10,"row");
#endif /* __HEED_MODEL__ */
    //    g2->SetParName(7,"factor"); g2->SetParLimits(7,-.1,0.1);
  }
  // Find pion peak
  Int_t nfound = fSpectrum->Search(proj);
  if (nfound < 1) return 0;
  Int_t npeaks = 0;
  Float_t *xpeaks = fSpectrum->GetPositionX();
  Float_t xp = 0;
  if (nfound > 2) nfound = 2;
  Double_t xpi = 9999;
  for (Int_t p = 0; p < nfound; p++) {
    xp = xpeaks[p];
    Int_t bin = proj->GetXaxis()->FindBin(xp);
    Double_t yp = proj->GetBinContent(bin);
    Double_t ep = proj->GetBinError(bin);
    if (yp-5*ep < 0) continue;
    if (xp < xpi) xpi = xp;
  }
  Double_t total = proj->Integral()*proj->GetBinWidth(5);
#ifndef __HEED_MODEL__
  g2->SetParameters(0, xpi, 0.0, 0.0, 0.1, 0.1, 0.0,0.0,-1.);
#else /* __HEED_MODEL__ */
  g2->SetParameters(0, xpi, 0.0, 0.0, 0.1, 0.1, 0.0,0.0,-1., 1., row);
#endif /* __HEED_MODEL__ */
  g2->FixParameter(2, 0); 
#ifdef __HEED_MODEL__
  g2->FixParameter(3, 0); 
  g2->FixParameter(4, 0); 
#endif /* __HEED_MODEL__ */
  g2->FixParameter(5,0);
  g2->FixParameter(6,0);
  g2->FixParameter(7,total);
  g2->FixParameter(8,-1);
#ifdef __HEED_MODEL__
  g2->SetParameter(9,1); //TMath::Log(10.));
  g2->FixParameter(10, row);
#endif /* __HEED_MODEL__ */
  gNFunc();
  TFitResultPtr res =  proj->Fit(g2,Opt.Data());
  Int_t iok = res->Status();
  if ( iok < 0) {
    cout << g2->GetName() << " fit has failed with " << iok << " for " 
	  << proj->GetName() << "/" << proj->GetTitle() << " Try one again" << endl; 
    proj->Fit(g2,Opt.Data());
  }
  //  g2->ReleaseParameter(5); g2->SetParameter(5,0.01); g2->SetParLimits(5,0.0,0.30);
  //  g2->ReleaseParameter(6); g2->SetParameter(6,0.01); g2->SetParLimits(6,0.0,0.30);
  Opt += "m";
  gNFunc();
  res = proj->Fit(g2,Opt.Data());
  iok = res->Status();
  if (iok < 0 ) return 0;
  if (! Opt.Contains("q",TString::kIgnoreCase)) {
    gNFunc();
    proj->Draw();
    Double_t params[10];
    g2->GetParameters(params);
    Double_t X = params[1];
    Double_t Y = TMath::Exp(params[0]);
    TPolyMarker *pm = new TPolyMarker(1, &X, &Y);
    proj->GetListOfFunctions()->Add(pm);
    pm->SetMarkerStyle(23);
    pm->SetMarkerColor(kRed);
    pm->SetMarkerSize(1.3);
    for (int i = 0; i < NFIT_HYP; i++) {
      TF1 *f = new TF1(*g2);
      f->SetName(Peaks[i].Name);
      f->FixParameter(8,i);
      f->SetLineColor(i+2);
      gNFunc();
      f->Draw("same");
      proj->GetListOfFunctions()->Add(f);
    }
  }
  return g2;
}
//________________________________________________________________________________
Double_t gXFunc(Double_t *x=0, Double_t *par=0) {
  // par[0] - norm
  // par[1] - pion position wrt Z_pion (Bichsel prediction)
  // par[2] - sigma 
  // par[3] - proton signal
  // par[4] - Kaon    -"-
  // par[5] - electorn -"-
  // par[6] - ScaleL
  // par[7] - Total
  // par[8] - Case
  // par[9] - dX
  // par[10]- ddX
  // ratio dN/dx_h / dN/dx_pi:      P        K        e        d
  //  static Double_t dNdxR[4] = {1.97273, 1.32040, 1.16313, 3.42753};
  //  static Double_t dNdxL10[4] = {0.295068, 0.120707, 0.0656301, 0.534982};
  static Double_t dNdxL10[4] = {0.590131, 0.241411, 0.131261, 1.06996};
  static Int_t _debug = 0;
  static TCanvas *c1 = 0;
  if (_debug) {
    c1 = (TCanvas *) gROOT->GetListOfCanvases()->FindObject("c1");
    if (! c1) c1 = new TCanvas();
    else      c1->Clear();
    c1->cd();
  }
  enum {NFIT_HYP = 3, NT};
  Double_t frac[NT];
  static Double_t sigmaOLD = -1;
  static Double_t fracpiOld = -1;
  static Int_t    icaseOLD = -1;
  static Double_t dNPion = -1, mpvPion = -1;
  static Double_t ln10 = TMath::Log(10.);
  static TH1D *hists[5] = {0};
  if (! x || ! par) {
    sigmaOLD = -1;
    return 0;
  }
  Double_t norm  = par[0];
  Double_t mu    = par[1];
  Double_t sigma = par[2];
  Double_t ScaleL= par[6];
  Double_t dX    = par[9];
  Double_t ddX   = par[10];
  Int_t i;
  frac[0] = 1;
  for (i = 1; i <= NFIT_HYP; i++) {
    frac[i] = TMath::Sin(par[2+i]);
    frac[i] *= frac[i];
    frac[0] -= frac[i];
  }
  TF1 *zdE = StdEdxModel::instance()->zdEdx();  
  if (sigma != sigmaOLD) {
    sigmaOLD = sigma;
    fracpiOld = -1;
    static const Char_t *names[5] = {"pi","P","K","e","d"};
    static Double_t dNdx_pion = 29.22; // 1/cm
    Double_t dNPion = dNdx_pion*dX;
    for (i = 0; i < NFIT_HYP; i++) {
      if (hists[i]) hists[i]->Reset();
      else          {
	hists[i] = new TH1D(Form("dE%s",names[i]),Form("Expected dE for %s",names[i]),100,-5,5);
	hists[i]->SetMarkerColor(i+1);
	hists[i]->SetLineColor(i+1);
      }
      Double_t dN    = dNPion;
      if (i) dN *= TMath::Power(10.,dNdxL10[i-1]);
      Double_t Sigma = TMath::Sqrt(sigma*sigma + 1./dN + (ddX/dX)*(ddX/dX));
      //     if (i) Sigma = TMath::Sqrt(Sigma*Sigma + 0.15*0.15);
      Double_t mpv     = StdEdxModel::instance()->zMPV()->Eval(TMath::Log10(dN),Sigma);
      if (i == 0) {
	mpvPion  = mpv;
      }
      zdE->SetParameter(1, dN);
      zdE->SetParameter(2,mpv - mpvPion);
      zdE->SetParameter(3,Sigma);
      hists[i]->Add(zdE);
      if (c1) {
	hists[i]->Draw();
	c1->Update();
      }
    }
    if (! hists[NFIT_HYP]) hists[NFIT_HYP] = new TH1D("dEAll","Expected dE for All",100,-5,5);
  }
  Int_t icase = (Int_t) par[8];
  Int_t i1 = 0;
  Int_t i2 = NFIT_HYP;
  if (icase >= 0) {i1 = i2 = icase;}
  if (icase != icaseOLD || icase >= 0 || TMath::Abs(fracpiOld - frac[0]) > 1e-7) {
    icaseOLD = icase;
    fracpiOld = frac[0];
    hists[NFIT_HYP]->Reset();
    for (i = i1; i <= i2; i++) { 
      if (frac[i] > 1e-7) {
	hists[NFIT_HYP]->Add(hists[i],  frac[i]);
      }
    }
    if (c1) {
      hists[NFIT_HYP]->Draw();
      c1->Update();
    }
  }  
  Double_t Value = par[7]*TMath::Exp(par[0])*hists[NFIT_HYP]->Interpolate(x[0]-par[1]);
  return Value;
}
//________________________________________________________________________________
TF1 *FitXF(TH1 *proj, Option_t *opt, Double_t dX = 1.25, Double_t ddX = 0.01) {// fit with no. of primary clusters
  static TSpectrum *fSpectrum = 0;
  if (! fSpectrum) {
    fSpectrum = new TSpectrum(6);
  }
  // fit in momentum range p = 0.45 - 0.50 GeV/c
  if (! proj) return 0;
  TString Opt(opt);
  //  Bool_t quet = Opt.Contains("Q",TString::kIgnoreCase);
  TF1 *g2 = (TF1*) gROOT->GetFunction("GX");
  enum {NFIT_HYP = 3}; // ignore e and d
  if (! g2) {
    g2 = new TF1("GX",gXFunc, -5, 5, 11);
    g2->SetParName(0,"norm"); g2->SetParLimits(0,-80,80);
    g2->SetParName(1,"mu");     g2->SetParLimits(1,-2.5,2.5);
    g2->SetParName(2,"Sigma");  g2->SetParLimits(2,0.,0.5);
    g2->SetParName(3,"P");      g2->SetParLimits(3,0.0,TMath::Pi()/2);
    g2->SetParName(4,"K");      g2->SetParLimits(4,0.0,0.30);
    g2->SetParName(5,"e");      g2->FixParameter(5,0);
    g2->SetParName(6,"ScaleL"); g2->FixParameter(6,0);
    g2->SetParName(7,"Total");
    g2->SetParName(8,"Case");
    g2->SetParName(9,"dX");     g2->FixParameter(9,dX);
    g2->SetParName(10,"ddX");   g2->FixParameter(10,ddX);
    //    g2->SetParName(7,"factor"); g2->SetParLimits(7,-.1,0.1);
  }
  // Find pion peak
  Int_t nfound = fSpectrum->Search(proj);
  if (nfound < 1) return 0;
  Int_t npeaks = 0;
  Float_t *xpeaks = fSpectrum->GetPositionX();
  Float_t xp = 0;
  if (nfound > 2) nfound = 2;
  Double_t xpi = 9999;
  for (Int_t p = 0; p < nfound; p++) {
    xp = xpeaks[p];
    Int_t bin = proj->GetXaxis()->FindBin(xp);
    Double_t yp = proj->GetBinContent(bin);
    Double_t ep = proj->GetBinError(bin);
    if (yp-5*ep < 0) continue;
    if (xp < xpi) xpi = xp;
  }
  Double_t total = proj->Integral()*proj->GetBinWidth(5);
  g2->SetParameters(0, xpi, 0.0, 0.6, 0.1, 0.1, 0.0,0.0,-1., dX, ddX);
  g2->FixParameter(5,0);
  g2->FixParameter(6,0);
  g2->FixParameter(7,total);
  g2->FixParameter(8,-1);
  g2->FixParameter(9,dX);
  g2->FixParameter(10,ddX);
  gXFunc();
  TFitResultPtr res =  proj->Fit(g2,Opt.Data());
  Int_t iok = res->Status();
  if ( iok < 0) {
    cout << g2->GetName() << " fit has failed with " << iok << " for " 
	  << proj->GetName() << "/" << proj->GetTitle() << " Try one again" << endl; 
    proj->Fit(g2,Opt.Data());
  }
  //  g2->ReleaseParameter(5); g2->SetParLimits(5,0.0,TMath::Pi()/2);
  Opt += "m";
  gXFunc();
  res = proj->Fit(g2,Opt.Data());
  iok = res->Status();
  if (iok < 0 ) return 0;
  if (! Opt.Contains("q",TString::kIgnoreCase)) {
    gXFunc();
    proj->Draw();
    Double_t params[11];
    g2->GetParameters(params);
    Double_t X = params[1];
    Double_t Y = TMath::Exp(params[0]);
    TPolyMarker *pm = new TPolyMarker(1, &X, &Y);
    proj->GetListOfFunctions()->Add(pm);
    pm->SetMarkerStyle(23);
    pm->SetMarkerColor(kRed);
    pm->SetMarkerSize(1.3);
    for (int i = 0; i < NFIT_HYP; i++) {
      TF1 *f = new TF1(*g2);
      f->SetName(Peaks[i].Name);
      f->FixParameter(8,i);
      f->SetLineColor(i+2);
      gXFunc();
      f->Draw("same");
      proj->GetListOfFunctions()->Add(f);
    }
  }
  return g2;
}
//________________________________________________________________________________
void dEdxFit() {}
//________________________________________________________________________________
void dEdxFit(const Char_t *HistName,const Char_t *FitName = "GP", 
	     Option_t *opt="R", 
	     Int_t ix = -1, Int_t jy = -1, 
	     Int_t mergeX=1, Int_t mergeY=1, 
	     Double_t nSigma=3, Int_t pow=1) {
  TCanvas *canvas = 0;
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
  TObject *obj = fRootFile->Get(HistName);
  if (!obj) {printf("Cannot find %s\n", HistName); return;}
  if (obj->IsA()->InheritsFrom( "TH1" )) {
    hist = (TH1 *) obj;
  } else if (obj->IsA()->InheritsFrom( "THnSparse" )) {
    hist = ((THnSparse *) obj)->Projection(1,0);
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
#ifdef __HEED_MODEL__
    Float_t Scale;
    Float_t dScale;
#endif /* __HEED_MODEL__ */
  };
  Fit_t Fit;
  //  TString NewRootFile(gSystem->DirName(fRootFile->GetName()));
  TString NewRootFile(gSystem->DirName(gSystem->BaseName(fRootFile->GetName())));
  NewRootFile += "/";
  NewRootFile += HistName;
  NewRootFile += FitName;
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
  if (! FitP) {
    FitP = new TNtuple("FitP","Fit results",
#ifndef __HEED_MODEL__
		       "i:j:x:y:mean:rms:peak:mu:sigma:entries:chisq:prob:a0:a1:a2:a3:a4:a5:Npar:dpeak:dmu:dsigma:da0:da1:da2:da3:da4:da5");
#else /* __HEED_MODEL__ */
		       "i:j:x:y:mean:rms:peak:mu:sigma:entries:chisq:prob:a0:a1:a2:a3:a4:a5:Npar:dpeak:dmu:dsigma:da0:da1:da2:da3:da4:da5:Scale:dScale");
#endif /* __HEED_MODEL__ */
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
  Double_t params[20] = {0};
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
	title += Form("in x [%5.1f,%5.1f] range",
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
      if (Fit.entries < 100) {delete proj; continue;}
      if (TString(FitName) == "GP") g = FitGP(proj,opt,nSigma,pow);
      else if (TString(FitName) == "G2") g = FitG2(proj,opt);
      else if (TString(FitName) == "NF" && dim == 3) {
	TH3 *hists[5] = {0};
	static const Char_t *names[5] = {"pi","P","K","e","d"};
	for (Int_t ih = 0; ih < 5; ih++) {
	  hists[ih] = (TH3 *) fRootFile->Get(Form("%s%s",HistName,names[ih]));
	  SafeDelete(projNs[ih]);
	  if (hists[ih]) {
	    if (ir0 == ir1 && jr0 == jr1) {
	      projNs[ih] = ((TH3 *) hists[ih])->ProjectionZ(Form("f%s%i_%i",names[ih],      ir0,    jr0    ),ir0,ir1,jr0,jr1); 
	      cout<<"Histogram "<<projNs[ih]->GetName()<<" was created"<<endl;
	    } else {  
	      projNs[ih] = ((TH3 *) hists[ih])->ProjectionZ(Form("f%s%i_%i_%i_%i",names[ih],ir0,ir1,jr0,jr1),ir0,ir1,jr0,jr1);
	      cout<<"Histogram "<<projNs[ih]->GetName()<<" was created"<<endl;
	    }
	  }
	}
	Opt = opt;
	Opt += "S";
#ifndef __HEED_MODEL__
	g = FitNF(proj,Opt);
#else /* __HEED_MODEL__ */
	g = FitNF(proj,Opt,Fit.j);
#endif /* __HEED_MODEL__ */
      }
      else if (TString(FitName) == "XF" && dim == 3) {
	Opt = opt;
	Opt += "S";
	TProfile2D *pdX = (TProfile2D *) fRootFile->Get(Form("%sdX",HistName));
	if (pdX) {
	  Double_t dX = 0, ddX = 0;
	  Int_t nn = 0;
	  for (Int_t ii = ir0; ii <= ir1; ii++) {
	    for (Int_t jj = jr0; jj <= jr1; jj++) {
	      Double_t d = pdX->GetBinContent(ii,jj);
	      Double_t e = pdX->GetBinError(ii,jj);
	      nn++;
	      dX += d;
	      ddX += e*e;
	    }
	  }
	  dX = dX/nn;
	  ddX = TMath::Sqrt(ddX/nn);
	  g = FitXF(proj,Opt,dX,ddX);
	} else {
	  g = FitXF(proj,Opt);
	}
      }
      else if (TString(FitName) == "GF") g = FitGF(proj,opt);
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
	Fit.dpeak  = g->GetParError(kpeak);
	Fit.dmu    = g->GetParError(1);
	Fit.dsigma = g->GetParError(2);
	Fit.da0    = g->GetParError(3);
	Fit.da1  	 = g->GetParError(4);
	Fit.da2  	 = g->GetParError(5);
	Fit.da3  	 = g->GetParError(6);
	Fit.da4  	 = g->GetParError(7);
	Fit.da5  	 = g->GetParError(8);
#ifdef __HEED_MODEL__
	if (Fit.Npar > 9) {
	  Fit.Scale      = params[9];
	  Fit.dScale     = g->GetParError(9);
	}
#endif /* __HEED_MODEL__ */
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
	}
	else {
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
      if (canvas) {
	canvas->Update();
      }
      fOut->cd();
      proj->Write();
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
