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
#include "StEvent.h"
#include "StGlobalTrack.h"
#include "StChain/StChain.h"
#include "TMath.h"
#include "TString.h"
#include "TH1.h"
#include "TH2.h"
#include "TProfile.h"

class StEvent;

class StFlowAnalysisMaker : public StMaker {

public:

  StFlowAnalysisMaker(const Char_t *name="FlowAnalysis");
  virtual ~StFlowAnalysisMaker();
  virtual Int_t Init();
  virtual Int_t Make();
  Int_t   getTags();
  virtual void PrintInfo();
  virtual Int_t Finish();

protected:

  // Histograms
  void makeTagHistograms();
  void makeFlowHistograms();

  // C++ way to define constants in the header
  enum {nHarmonics = 5, nSubEvents = 4};
 
  // structures for histograms
  struct histSubEventHarmonic {
    TH1F *mHistPsiSub;
  };

  struct histFullEventHarmonic {
    TH1F *mHistPsi;
    TH1D *mHistMult;
    TH1F *mHistMeanPt;
    TH1F *mHist_q;
  };

  struct histSubEvent;	
  friend struct histSubEvent;
  struct histSubEvent {
    struct histSubEventHarmonic histSubEventHarmonics[nHarmonics];
  };
  struct histSubEvent histSubEvents[nSubEvents]; //!

  struct histFullEvent;	
  friend struct histFullEvent;
  struct histFullEvent {
    TProfile *mHistCos;
    TH1F *mHistRes;
    struct histFullEventHarmonic histFullEventHarmonics[nHarmonics];
  };
  struct histFullEvent histFullEvents[nSubEvents/2]; //!

private:

  FlowTag_st*   mFlowTag;         //! pointer to the tag table
  StEvent*      mEvent;           //! pointer to DST data
  float QxSub[nSubEvents][nHarmonics];
  float QySub[nSubEvents][nHarmonics];
  float NSub[nSubEvents][nHarmonics];
  float sumPtSub[nSubEvents][nHarmonics];
  float PsiSub[nSubEvents][nHarmonics];
  float Qx[nSubEvents/2][nHarmonics];
  float Qy[nSubEvents/2][nHarmonics];
  float N[nSubEvents/2][nHarmonics];
  float sumPt[nSubEvents/2][nHarmonics];
  float Q[nSubEvents/2][nHarmonics];
  float q[nSubEvents/2][nHarmonics];
  float Psi[nSubEvents/2][nHarmonics];
  float CosDiffSubs[nSubEvents/2][nHarmonics];
  float res[nSubEvents/2][nHarmonics];
  float resErr[nSubEvents/2][nHarmonics];

  ClassDef(StFlowAnalysisMaker, 1)  // macro for rootcint
};

#endif
