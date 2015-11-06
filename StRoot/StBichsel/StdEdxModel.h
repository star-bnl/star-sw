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
  TF1    *zdEdx();
  Double_t zdE(Double_t n_P = 30, Double_t sigma = 0.25);
  Double_t dNdx(Double_t betagamma = 4.0, Double_t charge = 1.0) {return charge*charge*mdNdx->Interpolate(betagamma);}
 private:
  static StdEdxModel       *fgStdEdxModel; //! last instance          
  StdEdxModel();
  static Double_t zMPVFunc(Double_t *x, Double_t *p=0);
  static Double_t dLogNtpernPdP(Double_t *x, Double_t *p);
  static Double_t dEdxFunc(Double_t *x, Double_t *p);
  static TMultiDimFit *mDFit;    // Most probable log(n_t/n_p) value versus log10(n_p0 and sigma
  static TH1D         *mdNdx;    // dN/dx versus beta*gamma
  static TH2D         *mdEdxMPV; // Histogram for most probable log(n_t/n_p) value versus log10(n_p) and sigma
  static TH3F         *mdEdxFun; // Distribution {log10(n_p), sigma, log(n_t/n_p)}
  ClassDef(StdEdxModel,0)
};
// $Id: $
// $Log: $
#endif

