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
 public: 
  enum ETpcType  {kTpcOuter = 0, kTpcInner = 1, kTpcAll};
  enum EValType  {kProb, kdProbdX, kdProbdY};
  virtual ~StdEdxModel();
  static  StdEdxModel* instance();
  static TH1D         *GetdNdxL10()    {return    mdNdxL10;}    // dN/dx log10(versus beta*gamma)
  static TH1D         *GetdNdx()       {return    mdNdx;}       // dN/dx versus beta*gamma
  static Double_t      gausw(Double_t *x, Double_t *p); // vesus ksi, w, alpha
  static Double_t      ggaus(Double_t *x, Double_t *p);  // versus mu, sigm, alpha
  static Double_t      dNdx(Double_t poverm, Double_t charge = 1.0);
  static TF1          *GGaus() {return fGGaus;}
 private:
  static StdEdxModel *fgStdEdxModel; //! last instance          
  StdEdxModel();
  static TH1D         *mdNdxL10;    // dN/dx versus log10(beta*gamma)
  static TH1D         *mdNdx;       // dN/dx versus beta*gamma
  static Double_t      fScale;
  static Int_t        _debug;
  static TF1          *fGGaus;        
  ClassDef(StdEdxModel,0)
};
#endif

