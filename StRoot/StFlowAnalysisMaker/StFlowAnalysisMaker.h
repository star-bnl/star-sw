#ifndef StFlowAnalysisMaker_HH
#define StFlowAnalysisMaker_HH

///////////////////////////////////////////////////////////////////////////////
//
// $Id: StFlowAnalysisMaker.h,v 1.7 1999/11/05 00:02:03 posk Exp $
//
// Author: Art Poskanzer and Raimond Snellings, LBNL, Aug 1999
// Description:  Maker to analyze Flow using the FlowTags
//
///////////////////////////////////////////////////////////////////////////////
//
// $Log: StFlowAnalysisMaker.h,v $
// Revision 1.7  1999/11/05 00:02:03  posk
// Changed the flow vector, Q, to a TVector2.
//
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
#include "TVector2.h"

class StEvent;

class StFlowAnalysisMaker : public StMaker {

public:

           StFlowAnalysisMaker(const Char_t* name);
  virtual  ~StFlowAnalysisMaker();
  Int_t    Init();
  Int_t    Make();
  Int_t    getTags();
  Double_t getPhiWeight(Float_t mPhi, Int_t eventN, Int_t harN) const;
  TVector2 getQ(Int_t eventN, Int_t harN) const;
  Float_t  get_q(Int_t eventN, Int_t harN) const;
  Float_t  getMeanPt(Int_t eventN, Int_t harN) const;
  void     PrintInfo();
  Int_t    Finish();
  Float_t  getRes(Int_t eventN, Int_t harN) const;
  Float_t  getResErr(Int_t eventN, Int_t harN) const;

protected:

  // C++ way to define constants in the header
  enum {nHars = 4, nSubs = 4};
 
  // structures for histograms
  // for each harminic and each sub-event
  struct histSubHars {
    TH1F     *mHistPsiSubs;
  };

  // for each harminic and each event
  struct histFullHars {
    TH1D     *mHistPhi;
    TH1D     *mHistPhiWgt;
    TH1D     *mHistPhiFlat;
    TH1F     *mHistPhiCorr;
    TH1F     *mHistPsiSubCorr;
    TH1F     *mHistPsiSubCorrDiff;
    TH1F     *mHistPsi;
    TH1F     *mHistMult;
    TH1F     *mHistMeanPt;
    TH1F     *mHist_q;
    TH2D     *mHistYield2D;
    TH2D     *mHistSum_v2D;
    TH2F     *mHist_vObs2D;
    TH2F     *mHist_v2D;
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
    TH1F     *mHistRes;
    TH3F     *mHistEtaPtPhi3D;
    struct histFullHars histFullHar[nHars];
  };
  struct histFulls histFull[nSubs/2]; //!

private:

  void fillTagHistograms();
  void fillFlowHistograms();

  FlowTag_st*   mFlowTag;           //! pointer to the tag table
  StEvent*      mEvent;             //! pointer to DST data

  TVector2 mQSub[nSubs][nHars];     // flow vector sub-events
  TVector2 mQ[nSubs/2][nHars];      // flow vector
  Float_t  mMulSub[nSubs][nHars];   // multiplicity sub-events
  Float_t  mSumPtSub[nSubs][nHars]; // Pt sum sub-events
  Float_t  mPsiSub[nSubs][nHars];   // event plane angle subevents
  Float_t  mMul[nSubs/2][nHars];    // multiplicity
  Float_t  mSumPt[nSubs/2][nHars];  // Pt sum
  Float_t  mQMod[nSubs/2][nHars];   // flow vector magnitude
  Float_t  m_q[nSubs/2][nHars];     // Q/sqroot(Mul)
  Float_t  mPsi[nSubs/2][nHars];    // event plane angle
  Float_t  mRes[nSubs/2][nHars];    // event plane resolution
  Float_t  mResErr[nSubs/2][nHars]; // event plane resolution error

  enum {nPhiBins = 60};
  Double_t mPhiWgt[nSubs/2][nHars][nPhiBins]; // PhiWgt

  ClassDef(StFlowAnalysisMaker, 1) // macro for rootcint
};

inline TVector2 StFlowAnalysisMaker::getQ(Int_t eventN, Int_t harN) const 
  {return mQ[eventN][harN];}
inline Float_t StFlowAnalysisMaker::get_q(Int_t eventN, Int_t harN) const 
  {return m_q[eventN][harN];}
inline Float_t StFlowAnalysisMaker::getMeanPt(Int_t eventN, Int_t harN) const 
  {return (mMul[eventN][harN]) ? mSumPt[eventN][harN]/mMul[eventN][harN] : 0.;}
inline Float_t StFlowAnalysisMaker::getRes(Int_t eventN, Int_t harN) const 
  {return mRes[eventN][harN];}
inline Float_t StFlowAnalysisMaker::getResErr(Int_t eventN, Int_t harN) const 
  {return mResErr[eventN][harN];}

#endif
