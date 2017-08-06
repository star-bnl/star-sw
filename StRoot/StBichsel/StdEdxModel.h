//////////////////////////////////////////////////////////
//   This class has been automatically generated 
//     (Thu Nov 29 16:25:03 2001 by ROOT version3.02/01)
//   from TTree StdEdxModel/H.Bichel Calculation Summary
//   found on file: dEdx2T.root
//////////////////////////////////////////////////////////


#ifndef StdEdxModel_h
#define StdEdxModel_h
// Converted from dEdxModel.C 
#include <assert.h>
#include "Riostream.h"
#include "TROOT.h"
#include "TSystem.h"
#include "TMath.h"
#include "TF1.h"
#include "TF2.h"
#include "TH1.h"
#include "TH2.h"
#include "TH3.h"
#include "TFile.h"
#include "TString.h"
#include "TMultiDimFit.h"
//#define _HEED_MODEL__
class StdEdxModel {
 public: 
  virtual ~StdEdxModel();
  static  StdEdxModel* instance();
  static void MakedEdxModel();
  static TH1D         *GetdNdx()    {return    mdNdx;}    // dN/dx versus beta*gamma
  static TH1D         *GetdNdE()    {return    mdNdE;}    // dN/dE 
#ifndef __HEED_MODEL__
  TF2    *zMPV();
  TF1    *zFunc();
  TF1    *zdEdx();   // log(dE) with respect to Most Probable Value (MPV)
  TF1    *zdEdxA();  // log(dE[GeV]) 
  Double_t zdE(Double_t n_P = 30, Double_t sigma = 0.25);
  Double_t dNdx(Double_t betagamma = 4.0, Double_t charge = 1.0) {return charge*charge*mdNdx->Interpolate(betagamma);}
  static Double_t W() {return 45.44e-3;}// keV => eV per conducting electron P10: 26.2e-3*TMath::Exp(5.50667e-01) = 45.44e-3
  static Double_t n_Tz (Double_t z = 0) {return      TMath::Exp(z)/W();} // no. of conducting electron per ln(dE[keV])
  static Double_t n_TzG(Double_t z = 0) {return 1e-6*TMath::Exp(z)/W();} // no. of conducting electron per ln(dE[GeV])
  static void h2MDF(const Char_t  *total = "mu", Int_t max=5, Int_t maxTerm = 20);
  static TMultiDimFit *GetDFit()    {return    mDFit;}    // Parameterization of most probable log(n_t/n_p) value versus log10(n_p) and sigma
  static TH2D         *GetdEdxMPV() {return mdEdxMPV;} // Histogram for most probable log(n_t/n_p) value versus log10(n_p) and sigma
  static TH3F         *GetdEdxFun() {return mdEdxFun;} // Distribution {log10(n_p), sigma, log(n_t/n_p) - log(n_t/n_p)_MPV}
#else /* __HEED_MODEL__ */
  enum ESector  {kTpcOuter = 0, kTpcInner = 1};
  TF2    *zMPV( ESector kTpcOuterInner =  kTpcOuter);
  TF1    *zFunc( ESector kTpcOuterInner =  kTpcOuter);
  TF1    *zdEdx( ESector kTpcOuterInner =  kTpcOuter);   // log(dE) with respect to Most Probable Value (MPV)
  TF1    *zdEdxA( ESector kTpcOuterInner =  kTpcOuter);  // log(dE[GeV]) 
  Double_t zdE(Double_t n_P = 30, Double_t sigma = 0.25, ESector =  kTpcOuter);
  Double_t dNdx(Double_t betagamma = 4.0, Double_t charge = 1.0) {return charge*charge*(mdNdx->Interpolate(betagamma));}
  static Double_t dEz (Double_t z = 0, ESector kTpcOuterInner =  kTpcOuter) {return 1e-3*TMath::Exp(z);} // dE[eV] from per ln(dE[keV])
  static Double_t dEzG(Double_t z = 0, ESector kTpcOuterInner =  kTpcOuter) {return 1e-6*TMath::Exp(z);} // dE[eV] from per ln(dE[GeV])
  static TMultiDimFit *h2MDF(const Char_t  *total = "mu", Int_t max=5, Int_t maxTerm = 20);
  static TMultiDimFit *GetDFit(ESector kTpcOuterInner =  kTpcOuter)    {return    mDFit[kTpcOuterInner];}    // Parameterization of most probable log(n_t/n_p) value versus log(n_p) and sigma
  static TH2D         *GetdEdxMPV(ESector kTpcOuterInner =  kTpcOuter) {return mdEdxMPV[kTpcOuterInner];} // Histogram for most probable log(n_t/n_p) value versus log(n_p) and sigma
  static TH3F         *GetdEdxFun(ESector kTpcOuterInner =  kTpcOuter) {return mdEdxFun[kTpcOuterInner];} // Distribution {log(n_p), sigma, log(n_t/n_p) - log(n_t/n_p)_MPV}
#endif /* __HEED_MODEL__ */
 private:
  static StdEdxModel       *fgStdEdxModel; //! last instance          
  StdEdxModel();
  static Double_t zMPVFunc(Double_t *x, Double_t *p=0);
  static Double_t dEdxFunc(Double_t *x, Double_t *p);
#ifndef __HEED_MODEL__
  static Double_t dLogNtpernPdP(Double_t *x, Double_t *p);
  static TMultiDimFit *mDFit;    // Parameterization of most probable log(n_t/n_p) value versus log10(n_p) and sigma
#else /* __HEED_MODEL__ */
  static Double_t dLogdEpernPdP(Double_t *x, Double_t *p);
  static TMultiDimFit *mDFit[2];    // Parameterization of most probable log(n_t/n_p) value versus log(n_p) and sigma
#endif /* __HEED_MODEL__ */
  static TH1D         *mdNdx;    // dN/dx versus beta*gamma
  static TH1D         *mdNdE;    // dN/dE 
#ifndef __HEED_MODEL__
  static TH2D         *mdEdxMPV; // Histogram for most probable log(n_t/n_p) value versus log10(n_p) and sigma
  static TH3F         *mdEdxFun; // Distribution {log10(n_p), sigma, log(n_t/n_p) - log(n_t/n_p)_MPV}
  static Double_t      mzMin, mzMax, mdZ;  //
  static Double_t      mnPL10min, mnPL10max;
#else /* __HEED_MODEL__ */
  static TH2D         *mdEdxMPV[2]; // Histogram for most probable log(n_t/n_p) value versus log(n_p) and sigma
  static TH3F         *mdEdxFun[2]; // Distribution {log(n_p), sigma, log(n_t/n_p) - log(n_t/n_p)_MPV}
  static Double_t      mzMin[2], mzMax[2], mdZ[2];  //
  static Double_t      mnPLmin, mnPLmax;
  static Char_t *namesOI[2];
  static Char_t *nOI[2];
#endif /* __HEED_MODEL__ */
  static Int_t        _debug;
  ClassDef(StdEdxModel,0)
};
// $Id: StdEdxModel.h,v 1.1 2015/12/24 00:16:25 fisyak Exp $
// $Log: StdEdxModel.h,v $
// Revision 1.1  2015/12/24 00:16:25  fisyak
// Add TpcRS model and macros
//
#endif

