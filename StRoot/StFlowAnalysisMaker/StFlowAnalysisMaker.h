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
#include <iostream>
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

// class StFlowTagMaker;

class StFlowAnalysisMaker : public StMaker 
{

public:

  StFlowAnalysisMaker(const Char_t *name="FlowAnalysis");
  virtual ~StFlowAnalysisMaker();
  virtual Int_t Make();
  virtual void PrintInfo();
  virtual Int_t Init();

protected:

  // Histograms
  Int_t makeTagHistograms();
  void  makeFlowHistograms();

  // C++ way to define constants
  enum {nHarmonics = 5, nSubEvents = 4};

  // limits histograms
  enum {nPhiBins = 100, nEtaBins = 100, nPtBins = 100}; 
  enum {nPsiBins = 100, nSumPtBins = 100, nMultBins = 100, n_qBins = 100}; 

  struct histHarmonic {
    TH1F *mHistPsi;
    TH1F *mHistFlowTagSumPt;
    TH1D *mHistFlowTagMult;
    TH1F *mHist_q;
  };

  struct histSubEvent;	
  friend struct histSubEvent;
  struct histSubEvent {
    TProfile *mHistResolution;
    struct histHarmonic histHarmonics[nHarmonics];
  };

  struct histSubEvent histSubEvents[nSubEvents]; //!

private:

  FlowTag_st*   mFlowTag;        //! pointer to the tag table
  StEvent*      mEvent;           //! pointer to DST data
  float QxSub[nSubEvents][nHarmonics];
  float QySub[nSubEvents][nHarmonics];
  float NSub[nSubEvents][nHarmonics];
  float sPtSub[nSubEvents][nHarmonics];
  float psiSub[nSubEvents][nHarmonics];
  float Qx[nSubEvents/2][nHarmonics];
  float Qy[nSubEvents/2][nHarmonics];
  float N[nSubEvents/2][nHarmonics];
  float sPt[nSubEvents/2][nHarmonics];
  float Q[nSubEvents/2][nHarmonics];
  float q[nSubEvents/2][nHarmonics];
  float psi[nSubEvents/2][nHarmonics];
  float res[nSubEvents/2][nHarmonics];

  ClassDef(StFlowAnalysisMaker, 1)  // macro for rootcint
};

#endif









