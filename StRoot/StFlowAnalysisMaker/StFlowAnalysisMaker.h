#ifndef StFlowAnalysisMaker_HH
#define StFlowAnalysisMaker_HH

///////////////////////////////////////////////////////////////////////////////
//
// StFlowAnalysisMaker
//
// Description: 
//  Maker to analyze Tags for flow
//
// Environment:
//  Software developed for the STAR Detector at LBNL
//
// Author List: 
//  Raimond Snellings and Art Poskanzer, LBNL, 8/99
//
// History:
//
///////////////////////////////////////////////////////////////////////////////
#include "StFlowTagMaker/StFlowTagMaker.h"
#include <iostream.h>
#include <stdlib.h>
#include "StMaker.h"
#include "tables/FlowTag.h"
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

  // Histograms
  void makeTagHistograms();
  void makeFlowHistograms();
  Int_t writeHistFile();

  FlowTag_st*   mFlowTag;          //! pointer to the tag table
  StEvent*      mEvent;            //! pointer to DST data
  Float_t mQxSub[nSubs][nHars];    // flow vector x sub-events
  Float_t mQySub[nSubs][nHars];    // flow vector y sub-events
  Float_t mMulSub[nSubs][nHars];   // multiplicity sub-events
  Float_t mSumPtSub[nSubs][nHars]; // Pt sum sub-events
  Float_t mPsiSub[nSubs][nHars];   // event plane angle subevents
  Float_t mQx[nSubs/2][nHars];     // flow vector x
  Float_t mQy[nSubs/2][nHars];     // flow vector y
  Float_t mMul[nSubs/2][nHars];    // multiplicity
  Float_t mSumPt[nSubs/2][nHars];  // Pt sum
  Float_t mQ[nSubs/2][nHars];      // flow vector magnitude
  Float_t m_q[nSubs/2][nHars];     // Q/sqroot(Mul)
  Float_t mPsi[nSubs/2][nHars];    // event plane angle
  Float_t mRes[nSubs/2][nHars];    // event plane resolution
  Float_t mResErr[nSubs/2][nHars]; // event plane resolution error

  ClassDef(StFlowAnalysisMaker, 1) // macro for rootcint
};

#endif
