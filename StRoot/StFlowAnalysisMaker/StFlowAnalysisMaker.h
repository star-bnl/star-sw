#ifndef StFlowAnalysisMaker_HH
#define StFlowAnalysisMaker_HH

///////////////////////////////////////////////////////////////////////////////
//
// $Id: StFlowAnalysisMaker.h,v 1.6 1999/10/05 16:54:11 posk Exp $
//
// Author: Art Poskanzer and Raimond Snellings, LBNL, Aug 1999
// Description:  Maker to analyze Flow using the FlowTags
//
///////////////////////////////////////////////////////////////////////////////
//
// $Log: StFlowAnalysisMaker.h,v $
// Revision 1.6  1999/10/05 16:54:11  posk
// Added getPhiWeight method for making the event plane isotropic.
//
//
//
///////////////////////////////////////////////////////////////////////////////
#include "StFlowTagMaker/StFlowTagMaker.h"
#include <iostream.h>
#include <stdlib.h>
#include "StMaker.h"
#include "FlowTag.h"
#include "TH1.h"
#include "TH2.h"
#include "TH3.h"
#include "TProfile.h"

class StEvent;

class StFlowAnalysisMaker : public StMaker {

public:

  StFlowAnalysisMaker(const Char_t *name="FlowAnalysis");
  virtual ~StFlowAnalysisMaker();
  Int_t Init();
  Int_t Make();
  Int_t getTags();
  Float_t getPhiWeight(Float_t fPhi, Int_t eventN, Int_t harN);
  void PrintInfo();
  Int_t Finish();

protected:

  // C++ way to define constants in the header
  enum {nHars = 4, nSubs = 4};
 
  // structures for histograms
  // for each harminic and each sub-event
  struct histSubHars {
    TH1F *mHistPsiSubs;
  };

  // for each harminic and each event
  struct histFullHars {
    TH1F *mHistPhi;
    TH1F *mHistPhiWgt;
    TH1F *mHistPhiFlat;
    TH1F *mHistPhiCorr;
    TH1F *mHistPsiSubCorr;
    TH1F *mHistPsiSubCorrDiff;
    TH1F *mHistPsi;
    TH1F *mHistMult;
    TH1F *mHistMeanPt;
    TH1F *mHist_q;
    TH2D *mHistYield2D;
    TH2D *mHistSum_v2D;
    TH2F *mHist_vObs2D;
    TH2F *mHist_v2D;
    TProfile *mHistBinEta;
    TProfile *mHistBinPt;
  };

  struct histSubs;	
  friend struct histSubs;
  struct histSubs {
    struct histSubHars histSubHar[nHars];
  };
  struct histSubs histSub[nSubs]; //!

  // for each event
  struct histFulls;	
  friend struct histFulls;
  struct histFulls {
    TProfile *mHistCos;
    TH1F *mHistRes;
    TH3F *mHistEtaPtPhi3D;
    struct histFullHars histFullHar[nHars];
  };
  struct histFulls histFull[nSubs/2]; //!

private:

  void makeTagHistograms();
  void makeFlowHistograms();

  FlowTag_st*   mFlowTag;          //! pointer to the tag table
  StEvent*      mEvent;            //! pointer to DST data

  Float_t fQxSub[nSubs][nHars];    // flow vector x sub-events
  Float_t fQySub[nSubs][nHars];    // flow vector y sub-events
  Float_t fMulSub[nSubs][nHars];   // multiplicity sub-events
  Float_t fSumPtSub[nSubs][nHars]; // Pt sum sub-events
  Float_t fPsiSub[nSubs][nHars];   // event plane angle subevents
  Float_t fQx[nSubs/2][nHars];     // flow vector x
  Float_t fQy[nSubs/2][nHars];     // flow vector y
  Float_t fMul[nSubs/2][nHars];    // multiplicity
  Float_t fSumPt[nSubs/2][nHars];  // Pt sum
  Float_t fQ[nSubs/2][nHars];      // flow vector magnitude
  Float_t f_q[nSubs/2][nHars];     // Q/sqroot(Mul)
  Float_t fPsi[nSubs/2][nHars];    // event plane angle
  Float_t fRes[nSubs/2][nHars];    // event plane resolution
  Float_t fResErr[nSubs/2][nHars]; // event plane resolution error

  enum {nPhiBins = 60};
  Float_t fPhiWgt[nSubs/2][nHars][nPhiBins]; // PhiWgt

  ClassDef(StFlowAnalysisMaker, 1) // macro for rootcint
};

#endif
