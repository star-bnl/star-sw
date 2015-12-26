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
  TF2    *zMPV();
  TF1    *zFunc();
  TF1    *zdEdx();   // log(dE) with respect to Most Probable Value (MPV)
  TF1    *zdEdxA();  // log(dE[GeV]) 
  Double_t zdE(Double_t n_P = 30, Double_t sigma = 0.25);
  Double_t dNdx(Double_t betagamma = 4.0, Double_t charge = 1.0) {return charge*charge*mdNdx->Interpolate(betagamma);}
  static Double_t W() {return 45.44e-3;}// keV => eV per conducting electron P10: 26.2e-3*TMath::Exp(5.50667e-01) = 45.44e-3
  static Double_t n_Tz (Double_t z = 0) {return      TMath::Exp(z)/W();} // no. of conducting electron per ln(dE[keV])
  static Double_t n_TzG(Double_t z = 0) {return 1e-6*TMath::Exp(z)/W();} // no. of conducting electron per ln(dE[GeV])
  static void MakedEdxModel();
#if 0
  static void Make2dEdxModel();
#endif
  static void h2MDF(const Char_t  *total = "mu", Int_t max=5, Int_t maxTerm = 20);
  static TMultiDimFit *GetDFit()    {return    mDFit;}    // Parameterization of most probable log(n_t/n_p) value versus log10(n_p) and sigma
  static TH1D         *GetdNdx()    {return    mdNdx;}    // dN/dx versus beta*gamma
  static TH1D         *GetdNdE()    {return    mdNdE;}    // dN/dE 
  static TH2D         *GetdEdxMPV() {return mdEdxMPV;} // Histogram for most probable log(n_t/n_p) value versus log10(n_p) and sigma
  static TH3F         *GetdEdxFun() {return mdEdxFun;} // Distribution {log10(n_p), sigma, log(n_t/n_p) - log(n_t/n_p)_MPV}
 private:
  static StdEdxModel       *fgStdEdxModel; //! last instance          
  StdEdxModel();
  static Double_t zMPVFunc(Double_t *x, Double_t *p=0);
  static Double_t dLogNtpernPdP(Double_t *x, Double_t *p);
  static Double_t dEdxFunc(Double_t *x, Double_t *p);
  static TMultiDimFit *mDFit;    // Parameterization of most probable log(n_t/n_p) value versus log10(n_p) and sigma
  static TH1D         *mdNdx;    // dN/dx versus beta*gamma
  static TH1D         *mdNdE;    // dN/dE 
  static TH2D         *mdEdxMPV; // Histogram for most probable log(n_t/n_p) value versus log10(n_p) and sigma
  static TH3F         *mdEdxFun; // Distribution {log10(n_p), sigma, log(n_t/n_p) - log(n_t/n_p)_MPV}
  static Double_t      mzMin, mzMax, mdZ;  //
  static Int_t        _debug;
  ClassDef(StdEdxModel,0)
};
// $Id: StdEdxModel.h,v 1.1 2015/12/24 00:16:25 fisyak Exp $
// $Log: StdEdxModel.h,v $
// Revision 1.1  2015/12/24 00:16:25  fisyak
// Add TpcRS model and macros
//
#endif

