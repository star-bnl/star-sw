////////////////////////////////////////////////////////////////////////////
//
// $Id: StFlowCutTrack.cxx,v 1.1 1999/11/11 23:08:51 posk Exp $
//
// Author: Art Poskanzer and Raimond Snellings, LBNL, Oct 1999
//
// Description:  Class for applying track cuts
//
////////////////////////////////////////////////////////////////////////////
//
// $Log: StFlowCutTrack.cxx,v $
// Revision 1.1  1999/11/11 23:08:51  posk
// Rearrangement of files.
//
// Revision 1.1  1999/11/05 00:06:44  posk
// First versions of Flow cut classes.
//
//
////////////////////////////////////////////////////////////////////////////

#include <iostream.h>
#include <stdlib.h>
#include "StFlowCutTrack.hh"
#include "StGlobalTrack.h"
#include "StFlowTrack.hh"
//#include "StFlowMaker.hh"
//#include "StEvent.h"
#include "PhysicalConstants.h"
#include "SystemOfUnits.h"
#include "StThreeVectorD.hh"
#define PR(x) cout << (#x) << " = " << (x) << endl;

//ClassImp(StFlowCutTrack)

//-----------------------------------------------------------------------

StFlowCutTrack::StFlowCutTrack() {
  // To apply track cuts
}

//-----------------------------------------------------------------------

StFlowCutTrack::~StFlowCutTrack() {
}

//-----------------------------------------------------------------------

enum {nEvents = 2, nHars = 4};

Int_t    StFlowCutTrack::mFitPtsCuts[2]     = {10, 200};
Float_t  StFlowCutTrack::mFitOverMaxCuts[2] = {0.6, 1.};
Float_t  StFlowCutTrack::mRLCuts[2]         = {-0.1, 0.1};

Float_t  StFlowCutTrack::mEtaCuts[2][nEvents][nHars] = {{{-1.,-1.,-1.,-1.},
							 {-1.,-1.,-1.,-1.}},
							{{1.,1.,1.,1.},
							 {1.,1.,1.,1.}}};
Float_t  StFlowCutTrack::mPtCuts[2][nEvents][nHars] = {{{0.05,0.05,0.05,0.05},
							{0.05,0.05,0.05,0.05}},
						       {{2.,2.,2.,2.},
							{2.,2.,2.,2.}}};
UInt_t   StFlowCutTrack::mTrackN          = 0;     
UInt_t   StFlowCutTrack::mGoodTrackN      = 0;
UInt_t   StFlowCutTrack::mTrackRN         = 0;     
UInt_t   StFlowCutTrack::mTrackLN         = 0;     
UInt_t   StFlowCutTrack::mRLCutN          = 0;     
UInt_t   StFlowCutTrack::mFitPtsCutN      = 0;
UInt_t   StFlowCutTrack::mFitOverMaxCutN  = 0;
UInt_t   StFlowCutTrack::mEtaCutN         = 0;
UInt_t   StFlowCutTrack::mPtCutN          = 0;

const Double_t bField = 0.5*tesla;

//-----------------------------------------------------------------------

Int_t StFlowCutTrack::CheckTrack(StGlobalTrack* mTrack) {
  // Returns kTRUE if the track survives all the cuts
  mTrackN++;
  
  StTrackFitTraits& fitTraits = mTrack->fitTraits();
  Int_t nFitPoints = fitTraits.numberOfFitPoints();
  if (mFitPtsCuts[1] > mFitPtsCuts[0] && 
      (nFitPoints < mFitPtsCuts[0] || nFitPoints >= mFitPtsCuts[1])) {
    mFitPtsCutN++;
    return kFALSE;
  }
  
  Int_t nMaxPoints = fitTraits.numberOfPossiblePoints();
  //PR(nMaxPoints);
  //Float_t fitOverMax = (nMaxPoints) ? (float)nFitPoints/(float)nMaxPoints : 0.;
  Float_t fitOverMax = (nMaxPoints) ? (float)nFitPoints/(float)nMaxPoints : 0.8;
  if (mFitOverMaxCuts[1] > mFitOverMaxCuts[0] && 
      (fitOverMax < mFitOverMaxCuts[0] || fitOverMax >= mFitOverMaxCuts[1])) {
    mFitOverMaxCutN++;
    return kFALSE;
  }

  // Increment counters for Right - Left symmetry cut
  StThreeVectorD p = mTrack->helix().momentum(bField); 
  if (p.theta() < halfpi || p.theta() >= pi*1.5) {
    mTrackRN++;
  } else {
    mTrackLN++;
  }

  mGoodTrackN++;
  return kTRUE;
}

Int_t StFlowCutTrack::SelectTrack(StFlowTrack* mTrack, Int_t eventN, Int_t harN) {
  // Returns kTRUE if the track meets all the selection criteria
  //StThreeVectorD p = mTrack->helix().momentum(bField); 
  Float_t mEta =  mTrack->Eta();
  if (mEtaCuts[1][eventN][harN] > mEtaCuts[0][eventN][harN] && 
      (mEta < mEtaCuts[0][eventN][harN] || 
       mEta >= mEtaCuts[1][eventN][harN])) {
    mEtaCutN++;
    return kFALSE;
  }

  Float_t mPt  =  mTrack->Pt();
  if (mPtCuts[1][eventN][harN] > mPtCuts[0][eventN][harN] && 
      (mPt < mPtCuts[0][eventN][harN] || mPt >= mPtCuts[1][eventN][harN])) {
    mPtCutN++;
    return kFALSE;
  }
  return kTRUE;
}

//-----------------------------------------------------------------------

Int_t StFlowCutTrack::CheckEvent() {
  // Returns kTRUE if the event survives this Right - Left symmetry cut
  // Call at the end of the event after doing CheckTrack for each track
  // If kFALSE you should delete the last event
  Float_t ratioRL = ((float)mTrackRN - (float)mTrackLN) / 
    (float)(mTrackRN + mTrackLN);
  mTrackRN        = 0;
  mTrackLN        = 0;
  if (mRLCuts[1] > mRLCuts[0] && 
      (ratioRL < mRLCuts[0] || ratioRL >= mRLCuts[1])) {
    mRLCutN++;
    return kFALSE;
  }
  return kTRUE;
}

//-----------------------------------------------------------------------

void StFlowCutTrack::PrintCutList() {
  // Prints the list of cuts
  // Call in Finish
  cout << "#######################################################" << endl;
  for (int k = 0; k < nEvents; k++) {
    for (int j = 0; j < nHars; j++) {
      cout << "# Track Selections List, event= " << k+1 << " harmonic= " 
	   << j+1 << endl;
      cout << "#   Eta cuts= " << mEtaCuts[0][k][j] << ", " 
	   << mEtaCuts[1][1][1] << endl;
      cout << "#   Pt cuts= " << mPtCuts[0][k][j] << ", "
	   << mPtCuts[1][1][1] << endl;
    }
  }
  cout << "# Track Cut List:" << endl;
  cout << "#   FitPts cuts= " << mFitPtsCuts[0] << ", " << mFitPtsCuts[1] 
       << ", " << (float)mFitPtsCutN/(float)mTrackN/perCent << "% cut" << endl;
  cout << "#   FitOverMax cuts= " << mFitOverMaxCuts[0] << ", " 
       << mFitOverMaxCuts[1] << ", " 
       << (float)mFitOverMaxCutN/(float)mTrackN/perCent << "% cut" << endl;
  cout << "#   Right-Left Ratio cuts= " << mRLCuts[0] << ", " << mRLCuts[1] 
       << ", " << (float)mRLCutN/(float)mTrackN/perCent << "% cut" << endl;
  cout << "# Good Tracks = " << (float)mGoodTrackN/(float)mTrackN/perCent
       << "%" << endl;
  cout << "#######################################################" << endl;
}
