#ifndef StFlowTagMaker_HH
#define StFlowTagMaker_HH

///////////////////////////////////////////////////////////////////////////////
//
// StFlowTagMaker
//
// Description: 
//  Maker to access and analyze StEvent and fill Tag for flow analysis
//
// Environment:
//  Software developed for the STAR Detector at LBNL
//
// Author List: 
//  Raimond Snellings, LBNL, 6/99
//
// History:
//
///////////////////////////////////////////////////////////////////////////////
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

class StFlowTagMaker : public StMaker 
{

public:

  StFlowTagMaker(const Char_t *name="FlowTag", const Char_t *title="FlowTag");
  virtual ~StFlowTagMaker();
  virtual Int_t Make();
  virtual void PrintInfo();
  virtual Int_t Init();

  FlowTag_st* tag(); // returns pointer to the tag table


protected:

  void  fillFlowTag();
  Int_t phiRapidityWeight(float PhiAngle, float PseudoRapidity, 
			  float Pt, float *weight);
  Int_t eventPlane(long Multiplicity, float *mPseudoRapidity, float *mPhiAngle, 
		   float *mPt, double *mQx, double *mQy, double *mEventPlaneAngle,
		   int OrderParameter=2, int PhiYWeigt=0);
  void  swap(long &a,long &b);
  void  indexx(long n,float arr[], long indx[]);
  Int_t makeHistograms();
  // output Tag info to screen
  void  printTag(ostream& = cout);

  // Histograms to check if code is working

  // C++ way to define constants
  enum {nHarmonics = 5, nSubEvents = 4};

  // limits histograms
  enum {nPhiBins = 100, nEtaBins = 100, nPtBins = 100}; 
  enum {nPsiBins = 100, nSumPtBins = 100, nMultBins = 100, 
	n_qBins = 100}; 

  struct histHarmonic {
    TH1F *mHistPsi;
    TH1F *mHistFlowTagSumPt;
    TH1D *mHistFlowTagMult;
    TH1F *mHist_q;
  };

  struct histSubEvent;
  friend struct histSubEvent;
  struct histSubEvent {
    TH1F *mHistPhi;
    TH1F *mHistPseudoRapidity;
    TH1F *mHistPt;
    TProfile *mHistResolution;
    struct histHarmonic histHarmonics[nHarmonics];
  };

  struct histSubEvent histSubEvents[nSubEvents]; //!

private:

  FlowTag_st*   mFlowTag;        //! the tag table to fill
  StEvent*      mEvent;           //! pointer to DST data


  ClassDef(StFlowTagMaker, 1)  // macro for rootcint
};

#endif









