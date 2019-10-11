//////////////////////////////////////////////////////////
//   This class has been automatically generated 
//     (Thu Nov 29 16:25:03 2001 by ROOT version3.02/01)
//   from TTree StdEdNModel/H.Bichel Calculation Summary
//   found on file: dEdx2T.root
//////////////////////////////////////////////////////////


#ifndef StdEdNModel_h
#define StdEdNModel_h
// Converted from dEdNModel.C 
#include <assert.h>
#include "Riostream.h"
#include "TROOT.h"
#include "TSystem.h"
#include "TMath.h"
#include "TH1.h"
#include "TH2.h"
#include "TFile.h"
#include "TString.h"

class StdEdNModel {
 public: 
  enum ETpcType  {kTpcOuter = 0, kTpcInner = 1, kTpcAll};
  enum EValType  {kProb, kdProbdX, kdProbdY};
  virtual ~StdEdNModel();
  static  StdEdNModel* instance();
  static TH1D         *GetdNdxL10()    {return    mdNdxL10;}    // dN/dx versus beta*gamma
  static TH2F         *GetdEdN(EValType val = kProb, ETpcType tpcType = kTpcAll) {return mdEdNModel[tpcType][val];}
  static TH1F         *GetdEdNMPV( ETpcType tpcType = kTpcAll) {return mdEdNMPV[tpcType];}
 private:
  static StdEdNModel       *fgStdEdNModel; //! last instance          
  StdEdNModel();
  static TH1D         *mdNdxL10;    // dN/dx versus beta*gamma
  static TH2F         *mdEdNModel[3][3]; // Tpc [I,O,All] [Prob, dProb/dX, dProb/dY] versus dE/Np,log(Np)
  static TH1F         *mdEdNMPV[3];
  static Int_t        _debug;
  ClassDef(StdEdNModel,0)
};
// $Id: StdEdNModel.h,v 1.5 2018/10/17 20:45:23 fisyak Exp $
// $Log: StdEdNModel.h,v $
#endif

