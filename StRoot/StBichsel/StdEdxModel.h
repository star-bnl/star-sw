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
class StdEdxModel {
 public: 
  virtual ~StdEdxModel();
  static  StdEdxModel* instance();
  enum ESector  {kTpcOuter = 0, kTpcInner = 1};
  TF2    *zMPV( ESector kTpcOuterInner =  kTpcOuter);
  TF1    *zFunc( ESector kTpcOuterInner =  kTpcOuter);
  TF1    *zdEdx( ESector kTpcOuterInner =  kTpcOuter);   // log(dE) with respect to Most Probable Value (MPV)
  TF1    *zdEdxA( ESector kTpcOuterInner =  kTpcOuter);  // log(dE[GeV]) 
  Double_t zdE(Double_t n_P = 30, Double_t sigma = 0.25, ESector =  kTpcOuter);
  Double_t dNdx(Double_t betagamma = 4.0, Double_t charge = 1.0) {return charge*charge*(mdNdx->Interpolate(betagamma));}
  static Double_t W(ESector kTpcOuterInner =  kTpcOuter) {return 45.44e-3;}// keV => eV per conducting electron P10: 26.2e-3*TMath::Exp(5.50667e-01) = 45.44e-3
  static Double_t n_Tz (Double_t z = 0, ESector kTpcOuterInner =  kTpcOuter) {return      TMath::Exp(z)/W(kTpcOuterInner);} // no. of conducting electron per ln(dE[keV])
  static Double_t n_TzG(Double_t z = 0, ESector kTpcOuterInner =  kTpcOuter) {return 1e-6*TMath::Exp(z)/W(kTpcOuterInner);} // no. of conducting electron per ln(dE[GeV])
  static void MakedEdxModel();
#if 0
  static void Make2dEdxModel();
#endif
  static TMultiDimFit *h2MDF(const Char_t  *total = "mu", Int_t max=5, Int_t maxTerm = 20);
  static TMultiDimFit *GetDFit(ESector kTpcOuterInner =  kTpcOuter)    {return    mDFit[kTpcOuterInner];}    // Parameterization of most probable log(n_t/n_p) value versus log(n_p) and sigma
  static TH1D         *GetdNdx()    {return    mdNdx;}    // dN/dx versus beta*gamma
  static TH1D         *GetdNdE()    {return    mdNdE;}    // dN/dE 
  static TH2D         *GetdEdxMPV(ESector kTpcOuterInner =  kTpcOuter) {return mdEdxMPV[kTpcOuterInner];} // Histogram for most probable log(n_t/n_p) value versus log(n_p) and sigma
  static TH3F         *GetdEdxFun(ESector kTpcOuterInner =  kTpcOuter) {return mdEdxFun[kTpcOuterInner];} // Distribution {log(n_p), sigma, log(n_t/n_p) - log(n_t/n_p)_MPV}
 private:
  static StdEdxModel       *fgStdEdxModel; //! last instance          
  StdEdxModel();
  static Double_t zMPVFunc(Double_t *x, Double_t *p=0);
  static Double_t dLogNtpernPdP(Double_t *x, Double_t *p);
  static Double_t dEdxFunc(Double_t *x, Double_t *p);
  static TMultiDimFit *mDFit[2];    // Parameterization of most probable log(n_t/n_p) value versus log(n_p) and sigma
  static TH1D         *mdNdx;    // dN/dx versus beta*gamma
  static TH1D         *mdNdE;    // dN/dE 
  static TH2D         *mdEdxMPV[2]; // Histogram for most probable log(n_t/n_p) value versus log(n_p) and sigma
  static TH3F         *mdEdxFun[2]; // Distribution {log(n_p), sigma, log(n_t/n_p) - log(n_t/n_p)_MPV}
  static Double_t      mzMin[2], mzMax[2], mdZ[2];  //
  static Double_t      mnPLmin, mnPLmax;
  static Int_t        _debug;
  static Char_t *namesOI[2];
  static Char_t *nOI[2];
  ClassDef(StdEdxModel,0)
};
// $Id: StdEdxModel.h,v 1.1 2015/12/24 00:16:25 fisyak Exp $
// $Log: StdEdxModel.h,v $
// Revision 1.1  2015/12/24 00:16:25  fisyak
// Add TpcRS model and macros
//
#endif

