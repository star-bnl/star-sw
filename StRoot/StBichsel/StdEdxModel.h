#ifndef StdEdxModel_h
#define StdEdxModel_h
#include <assert.h>
#include "Riostream.h"
#include "TROOT.h"
#include "TSystem.h"
#include "TMath.h"
#include "TH1.h"
#include "TH2.h"
#include "TFile.h"
#include "TString.h"
#include "TF1.h"
class StdEdxModel {
  // Np = no. of primary clusters
  // ne == n = no. of conductining electorns
 public: 
  enum ETpcType  {kTpcOuter = 0, kTpcInner = 1, kTpcAll};
  enum EValType  {kProb, kdProbdX, kdProbdY};
  virtual ~StdEdxModel();
  static  StdEdxModel* instance();
  static TH1D         *GetdNdx()       {return    mdNdx;}       // dN/dx versus beta*gamma
  static Double_t      gausw(Double_t *x, Double_t *p); // vesus ksi, w, alpha
  static Double_t      ggaus(Double_t *x, Double_t *p);  // versus mu, sigm, alpha
  static Double_t      gausexp(Double_t *x, Double_t *); // versus mu, sigma, k 
  static Double_t      dNdx(Double_t poverm, Double_t charge = 1.0);
  static TF1          *GGaus() {return fGGaus;}
  static TF1          *GausExp() {return fGausExp;}
  static Double_t E(Double_t ne) {return GeVperElectron*ne;} // deposited energy GeV from ne
  static Double_t n(Double_t e) {return e/GeVperElectron;}   // ne  from energy (GeV)
  static Double_t LogE(Double_t Logne) {return LogGeVperElectron  + Logne;} // deposited energy GeV from ne
  static Double_t Logne(Double_t LogE) {return LogE - LogGeVperElectron;}   // ne  from energy (GeV)
  static void     Parameters(Double_t Np, Double_t *pars, Double_t *derivatives = 0); 
  static Double_t Derivative(Double_t Np, Int_t k = 0) {return 0;}
  static Double_t Parameter(Double_t Np, Int_t k = 0);
  static Double_t Mu(Double_t Np)    {return  Parameter(Np, 0);} // Most Probable log (dE/Np) versus Np 
  static Double_t Sigma(Double_t Np) {return  Parameter(Np, 1);} // RMS
  static Double_t Alpha(Double_t Np) {return  Parameter(Np, 2);} // assymetry
  static Double_t K(Double_t Np)     {return  Parameter(Np, 2);} // assymetry
  static Double_t MuDeriv(Double_t Np)    {return  Derivative(Np, 0);} // Most Probable log (dE/Np) versus Np derivateive wrt log(Np)
  static Double_t SigmaDeriv(Double_t Np) {return  Derivative(Np, 1);} // RMS       -"-
  static Double_t AlphaDeriv(Double_t Np) {return  Derivative(Np, 2);} // assymetry -"-
  static Double_t LogdEMPV(Double_t Np)   {return  (Mu(Np) + TMath::Log(Np));} // 
  static Double_t LogdEMPVeV (Double_t Np)   {return  shift2eV + LogdEMPV(Np);}
  static Double_t LogdEMPVkeV(Double_t Np)   {return  shift2keV + LogdEMPV(Np);}
  static Double_t LogdEMPVGeV(Double_t Np)   {return  shift2GeV + LogdEMPV(Np);}
  static Double_t Prob(Double_t ee, Double_t Np); // probability for give log(dE/Np) versus Np
  static Double_t ProbdEGeVlog(Double_t dEGeVLog, Double_t Np); // probability for give log(dE(GeV)) versus Np
  static Double_t Shift2keV() {return shift2keV;}
  static Double_t Shift2GeV() {return shift2GeV;}
  static Double_t Shift2eV () {return shift2eV ;}
  static void     SetScale(Double_t scale = 1.0) {fScale = scale;}
  static Double_t dNdxScale() {return fScale;}
  static Double_t zMPold(Double_t *x, Double_t *p); // most probable log (dE) versus x = log10(p/M) and p[0] = log2dx and p[1] =  charge
  static TF1     *ZMPold(Double_t log2dx = 1);
  static Double_t zMP(Double_t *x, Double_t *p); // most probable log (dE) versus x = log10(p/M) and p[0] = log2dx and p[1] =  charge
  static TF1     *ZMP(Double_t log2dx = 1);
  static Double_t zMPR(Double_t *x, Double_t *p); // + recombination
  static TF1     *ZMPR(Double_t log2dx = 1); // -"-
  // from 100 keV Tcut (GEXNor.C0
  static void InitPar();
  static Double_t muPar(Double_t x, Double_t tCutL10 = 5); // log_10{Tcut[eV]}
  static Double_t sigmaPar(Double_t x, Double_t tCutL10 = 5); // log_10{Tcut[eV]}
  static Double_t a0Par(Double_t x, Double_t tCutL10 = 5); // log_10{Tcut[eV]}
  static void SetOld(Bool_t k = kTRUE) { fOld = k;}
  static Double_t tmaxL10eV(Double_t betagamma); // eV
 private:
  StdEdxModel();
  static StdEdxModel *fgStdEdxModel; //! last instance          
  static Double_t GeVperElectron;
  static Double_t LogGeVperElectron;
  static TH1D         *mdNdx;       // dN/dx versus beta*gamma
  static Double_t      fScale;
  static Double_t      fTmaxL10eV;
  static Int_t        _debug;
  static TF1          *fGGaus;        
  static TF1          *fGausExp;        
  static Double_t      shift2keV;
  static Double_t      shift2GeV;
  static Double_t      shift2eV;
  static Bool_t        fOld;
  static TF1          *fpol2F;
  static TF1          *fpol5F;
  static TF1          *fpol6F;
  ClassDef(StdEdxModel,0)
};
#endif
