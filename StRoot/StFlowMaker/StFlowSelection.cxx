////////////////////////////////////////////////////////////////////////////
//
// $Id: StFlowSelection.cxx,v 1.11 2000/09/22 22:03:00 posk Exp $
//
// Author: Art Poskanzer and Raimond Snellings, LBNL, Mar 2000
//
// Description:  Class for making selections
//
////////////////////////////////////////////////////////////////////////////
//
// $Log: StFlowSelection.cxx,v $
// Revision 1.11  2000/09/22 22:03:00  posk
// Clean up.
//
// Revision 1.10  2000/09/16 22:20:32  snelling
// Added selection on P and global DCA and fixed rapidity calulation
//
// Revision 1.9  2000/09/15 22:51:34  posk
// Added pt weighting for event plane calcualtion.
//
// Revision 1.8  2000/09/15 01:20:02  snelling
// Added methods for P and Y and added selection on Y
//
// Revision 1.7  2000/09/13 00:32:27  snelling
// Added selections for particles correlated with reaction plane
//
// Revision 1.6  2000/08/31 18:58:26  posk
// For picoDST, added version number, runID, and multEta for centrality.
// Added centrality cut when reading picoDST.
// Added pt and eta selections for particles corr. wrt event plane.
//
// Revision 1.5  2000/08/12 20:22:21  posk
// Recalculate centrality in read from pico.
//
// Revision 1.4  2000/05/26 21:29:32  posk
// Protected Track data members from overflow.
//
// Revision 1.3  2000/05/11 20:00:38  posk
// Preparation for micro and nano DSTs.
//
// Revision 1.2  2000/03/28 23:21:04  posk
// Allow multiple instances of the AnalysisMaker.
//
// Revision 1.1  2000/03/15 23:28:53  posk
// Added StFlowSelection.
//
////////////////////////////////////////////////////////////////////////////

#include <iostream.h>
#include <stdlib.h>
#include "StFlowSelection.h"
#include "StFlowEvent.h"
#include "StFlowTrack.h"
#define PR(x) cout << "##### FlowSelection: " << (#x) << " = " << (x) << endl;

ClassImp(StFlowSelection)

//-----------------------------------------------------------------------


StFlowSelection::StFlowSelection() : mCentrality(0), mSubevent(-1) {
  // To make selections
  mNumber[0]  = '\0';
  mPid[0]     = '\0';
  mPidPart[0] = '\0';
  mPtPart[0]            = 0.;
  mPtPart[1]            = 0.;
  mPPart[0]             = 0.;
  mPPart[1]             = 0.;
  mEtaPart[0]           = 0.;
  mEtaPart[1]           = 0.;
  mFitPtsPart[0]        = 0;
  mFitPtsPart[1]        = 0;
  mFitOverMaxPtsPart[0] = 0.;
  mFitOverMaxPtsPart[1] = 0.;
  mChiSqPart[0]         = 0.;
  mChiSqPart[1]         = 0.;
  mDcaPart[0]           = 0.;
  mDcaPart[1]           = 0.;
  mDcaGlobalPart[0]     = 0.;
  mDcaGlobalPart[1]     = 0.;
  mYPart[0]             = 0.;
  mYPart[1]             = 0.;
}

//-----------------------------------------------------------------------

StFlowSelection::~StFlowSelection() {
}

//-----------------------------------------------------------------------

Bool_t StFlowSelection::Select(StFlowEvent* pFlowEvent) {
  // Returns kTRUE if the event is selected

  // Centrality
  if (mCentrality) {
    UInt_t cent = pFlowEvent->Centrality();
    if (mCentrality != cent) return kFALSE;
  }

  return kTRUE;
}

//-----------------------------------------------------------------------

Bool_t StFlowSelection::Select(StFlowTrack* pFlowTrack) {
  // Selects particles for event plane determination
  // Returns kTRUE if the track is selected

  // PID
  if (mPid[0] != '\0') {
    const Char_t* pid = pFlowTrack->Pid();
    if (strstr(pid, mPid)==0) return kFALSE;
  }
  
  // Selected for event plane
  if (!pFlowTrack->Select(mHarmonic, mSelection, mSubevent)) return kFALSE;
  
  return kTRUE;
}

//-----------------------------------------------------------------------

Bool_t StFlowSelection::SelectPart(StFlowTrack* pFlowTrack) {
  // Selects particles for correlation with the event plane
  // Returns kTRUE if the track is selected

  // PID
  if (mPidPart[0] != '\0') {
    const Char_t* pid = pFlowTrack->Pid();
    if (strstr(pid, mPidPart)==0) return kFALSE;
  }
  
  // Pt
  float pt = pFlowTrack->Pt();
  if (mPtPart[1] > mPtPart[0] && 
      (pt < mPtPart[0] || pt >= mPtPart[1])) return kFALSE;
  
  // P
  float totalp = pFlowTrack->P();
  if (mPPart[1] > mPPart[0] && 
      (totalp < mPPart[0] || totalp >= mPPart[1])) return kFALSE;
  
  // Eta
  float eta = pFlowTrack->Eta();
  if (mEtaPart[1] > mEtaPart[0] && 
      (eta < mEtaPart[0] || eta >= mEtaPart[1])) return kFALSE;

  // Fit Points
  int fitPts = pFlowTrack->FitPts();
  if (mFitPtsPart[1] > mFitPtsPart[0] && 
      (fitPts < mFitPtsPart[0] || fitPts >= mFitPtsPart[1])) return kFALSE;

  // Fit Points over Max Points
  int maxPts = pFlowTrack->MaxPts();
  float fitOverMaxPts = (float) fitPts / (float) maxPts;
  if (mFitOverMaxPtsPart[1] > mFitOverMaxPtsPart[0] && 
      (fitOverMaxPts < mFitOverMaxPtsPart[0] || 
       fitOverMaxPts >= mFitOverMaxPtsPart[1])) return kFALSE;

  // Chi Squared
  float chiSq = pFlowTrack->Chi2();
  if (mChiSqPart[1] > mChiSqPart[0] && 
      (chiSq < mChiSqPart[0] || 
       chiSq >= mChiSqPart[1])) return kFALSE;

  // DCA
  float dca = pFlowTrack->Dca();
  if (mDcaPart[1] > mDcaPart[0] && 
      (dca < mDcaPart[0] || dca >= mDcaPart[1])) return kFALSE;

  // DCA Global
  float globdca = pFlowTrack->DcaGlobal();
  if (mDcaGlobalPart[1] > mDcaGlobalPart[0] && 
      (globdca < mDcaGlobalPart[0] || 
       globdca >= mDcaGlobalPart[1])) return kFALSE;

  // Rapidity
  float Y = pFlowTrack->Y();
  if (mYPart[1] > mYPart[0] && 
      (Y < mYPart[0] || Y >= mYPart[1])) return kFALSE;

  return kTRUE;
}

//-----------------------------------------------------------------------

void StFlowSelection::PrintList() const {
  
  cout << "#################################################################"
       << endl;
  cout << "# Selection List:" << endl;
  cout << "# Number = " << mNumber << endl;
  cout << "# Centrality = " << mCentrality << endl;
  cout << "# Particles used for the event plane: " << mPid << endl;
  cout << "# Particles correlated with the event plane: " << mPidPart << endl;
  cout << "# Pt for particles correlated with the event plane: " << 
    mPtPart[0] << " to " << mPtPart[1] << " GeV/c" <<endl;
  cout << "# P for particles correlated with the event plane: " << 
    mPPart[0] << " to " << mPPart[1] << " GeV/c" <<endl;
  cout << "# Eta for particles correlated with the event plane: " << 
    mEtaPart[0] << " to " << mEtaPart[1] <<endl;
  cout << "# Y for particles correlated with the event plane: " << 
    mYPart[0] << " to " << mYPart[1] <<endl;
  cout << "# Fit Points for particles correlated with the event plane: " << 
    mFitPtsPart[0] << " to " << mFitPtsPart[1] <<endl;
  cout << "# Fit/Max Points for particles correlated with the event plane: " 
       << mFitOverMaxPtsPart[0] << " to " << mFitOverMaxPtsPart[1] <<endl;
  cout << "# Chi2 for particles correlated with the event plane: " << 
    mChiSqPart[0] << " to " << mChiSqPart[1] <<endl;
  cout << "# Dca for particles correlated with the event plane: " << 
    mDcaPart[0] << " to " << mDcaPart[1] <<endl;
  cout << "# Global Dca for particles correlated with the event plane: " << 
    mDcaGlobalPart[0] << " to " << mDcaGlobalPart[1] <<endl;
  cout << "#################################################################"
       << endl;

}
