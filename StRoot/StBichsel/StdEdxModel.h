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
#include "TSpline.h"
class StdEdxParamerization {
};
class StdEdxModel {
  // Np = no. of primary clusters
  // ne == n = no. of conductining electorns
 public: 
  enum ETpcType  {kTpcOuter = 0, kTpcInner = 1, kTpcAll};
  enum EValType  {kProb, kdProbdX, kdProbdY};
  virtual ~StdEdxModel();
  static  StdEdxModel* instance();
  static Double_t      gausw(Double_t *x, Double_t *p); // vesus ksi, w, alpha
  static Double_t      ggaus(Double_t *x, Double_t *p);  // versus mu, sigm, alpha
  static Double_t      ggausD(Double_t *x, Double_t *p, Double_t *der = 0);  // versus mu, sigm, alpha wth derivatives
  static Double_t      gausexp(Double_t *x, Double_t *p); // versus mu, sigma, k 
  static Double_t      gausexpD(Double_t *x, Double_t *p, Double_t *der = 0); // versus mu, sigma, k 
  Double_t             dNdx(Double_t poverm, Double_t charge = 1.0);
  TF1                 *dNdxL10F();
  static Double_t      dNdxL10func(Double_t *x, Double_t *p);
  Double_t     	       dNdxEff(Double_t poverm, Double_t charge = 1.0, Double_t mass = 0.13956995);
  static Double_t      dNdxEffL10func(Double_t *x, Double_t *p);
  TF1                 *dNdxEffL10F();
  Double_t             dNdE();
  static Double_t      saturationFunc(Double_t *x, Double_t *p); // nP saturation versus beta*gamma from TpcRS (nP/dX - dN/dx_model) 
  TF1          	      *GGaus() {return fGGaus;}										      
  TF1          	      *GausExp() {return fGausExp;}										      
  TF1          	      *Saturation(Int_t particle=0);										      
  static Double_t      saturationTanH(Double_t *x, Double_t *p); // nP saturation versus beta*gamma from TpcRS (nP/dX - dN/dx_model) 
  TF1                 *SaturTanH();                                                                                                       
  static Double_t      extremevalueG(Double_t *x, Double_t *p);  // Sum of extreme value distribution and Gauss
  TF1                 *ExValG();
  Double_t             keVperElectron() {return TMath::Exp(fLogkeVperElectron);}
  Double_t             GeVperElectron() {return 1e-6*keVperElectron();}
  Double_t             E(Double_t ne) {return ne*GeVperElectron();} // deposited energy GeV from ne					       
  Double_t 	       n(Double_t e) {return e/GeVperElectron();}   // ne  from energy (GeV)					       
  Double_t 	       LogE(Double_t Logne) {return fLogkeVperElectron  + Logne + TMath::Log(1e-6);} // deposited energy GeV from ne			       
  Double_t 	       Logne(Double_t LogE) {return LogE - fLogkeVperElectron - TMath::Log(1e-6);}   // ne  from energy (GeV)				       
  void     	       Parameters(Double_t Np, Double_t *pars, Double_t *dPardNp = 0); 						       
  Double_t 	       Parameter(Double_t Np, Int_t k = 0, Double_t *dPardNp = 0);										       
  static Double_t      funParam(Double_t *x, Double_t *p);
  TF1                 *FParam(Int_t l = 0); // l = 0 -> mu, l = 1 -> sigma, l = 2 -> alpha
  Double_t 	       MukeV(Double_t Np);                             // log(dE) (keV)
  Double_t 	       Sigma(Double_t Np) {return Parameter(Np, 1);}								       
  Double_t 	       Alpha(Double_t Np) {return Parameter(Np, 2);}								       
  Double_t 	       LogdEMPV(Double_t Np)      {return LogdEMPVGeV(Np);}
  Double_t 	       LogdEMPVeV (Double_t Np)   {return LogdEMPVkeV(Np) + TMath::Log(1e3);}
  Double_t 	       LogdEMPVkeV(Double_t Np)   {return MukeV(Np);} // log(dE) (keV)  
  Double_t 	       LogdEMPVGeV(Double_t Np)   {return MukeV(Np) + TMath::Log(1e-6);} // log(dE) (keV)  
  Double_t             Prob(Double_t ee, Double_t Np, Double_t *der = 0); // probability for give log(dE/Np) versus Np					       
  static Double_t      funcProb(Double_t *x, Double_t *p);
  TF1                 *FProb();
  static Double_t      funcProbP(Double_t *p, Double_t *x);
  TF1                 *FProbP();
  static Double_t      funcProbDer(Double_t *x, Double_t *p);
  TF1                 *FProbDer();
  Double_t 	       ProbdEGeVlog(Double_t dEGeVLog, Double_t Np, Double_t *der = 0); // probability for give log(dE(GeV)) versus Np			       
  void     	       SetScale(Double_t scale = 1.0) {fScale = scale;}								       
  Double_t 	       dNdxScale() {return fScale;}											       
  static Double_t      zMP(Double_t *x, Double_t *p); // most probable log (dE) versus x = log10(p/M) and p[0] = log2dx, p[1] =  charge, and p[2] = mass
  TF1     	      *ZMP(Double_t log2dx = 1, Double_t charge = 1, Double_t mass = 0.1395699);                                                                                           
  // from 100 keV Tcut (GEXNor.C)
  void InitPar();
  Double_t tmaxL10eV(Double_t betagamma); // eV
  Double_t bgCorrected(Double_t bg); // Correction for reconstructed bega*gamma
  Double_t NpCorrection(Double_t betagamma); // Correction for effective no. of primary clusters which produce conducting electrons 
  TH1D    *protonEff();
 private:
  StdEdxModel();
  static StdEdxModel  *fgStdEdxModel; //! last instance          
  static Int_t  _debug;
  Double_t      fScale;
  Double_t      fTmaxL10eV;
  Char_t        beg[1];                    //!
  Double_t      fLogkeVperElectron;
  TH1D         *mdNdx;                     //!
  TH1D         *mdNdxL10;                  //!
  TH1D         *mLndNdxL10;                //!
  TH1D         *mLndNdxL10Smooth;          //!
  TSpline5     *mLndNdxL10Spline5;         //!
  TH1D         *mdNdEL10;                  //!
  TF1          *fGGaus;                    //!
  TF1          *fGausExp;        	   //!
  TF1          *fpol2F;			   //!
  TF1          *fpol5F;			   //!
  TF1          *fpol6F;			   //!
  TF1          *fpol7F;			   //!
  Char_t        end[1];                    //!
  ClassDef(StdEdxModel,0)
};
#endif
