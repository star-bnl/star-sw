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
  static TH2F         *GetdEdN(EValType val = kProb, ETpcType tpcType = kTpcAll) {return instance()->mdEdNModel[tpcType][val];}
  static TH1F         *GetdEdNMPV( ETpcType tpcType = kTpcAll) {return instance()->mdEdNMPV[tpcType];}
  static TH2F         *GetLogdEdN(EValType val = kProb, ETpcType tpcType = kTpcAll) {return instance()->mLogdEdNModel[tpcType][val];}
  static TH1F         *GetLogdEdNMPV( ETpcType tpcType = kTpcAll) {return instance()->mLogdEdNMPV[tpcType];}
  static Double_t      dNdx(Double_t poverm, Double_t charge = 1.0); 
  static Double_t      zMPVFunc(Double_t *x, Double_t *p=0); // most probable dE versus x[0] = log(N_p) and sector p[0]
  static TF1          *zMPV();                               // -"-
  static Double_t      zdEFunc(Double_t *x, Double_t *p);    // Distributon Log(dE) - log(most probable dE) versus sector p[0]  and  p[1] = log(N_p)
  static TF1          *zdE();                                // -"-
 private:
  static StdEdNModel *fgStdEdNModel; //! last instance          
  StdEdNModel();
  static TH1D         *mdNdxL10;    // dN/dx versus beta*gamma
  static TH2F         *mdEdNModel[3][3]; // Tpc [I,O,All] [Prob, dProb/dX, dProb/dY] versus dE/Np,log(Np)
  static TH1F         *mdEdNMPV[3];
  static TH2F         *mLogdEdNModel[3][3]; // Tpc [I,O,All] [Prob, dProb/dX, dProb/dY] versus Log(dE/Np),log(Np)
  static TH1F         *mLogdEdNMPV[3];
  static Double_t      fScale;
  static Int_t        _debug;
  ClassDef(StdEdNModel,0)
};
// $Id: StdEdNModel.h,v 1.5 2018/10/17 20:45:23 fisyak Exp $
// $Log: StdEdNModel.h,v $
#endif

