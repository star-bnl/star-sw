////////////////////////////////////////////////////////////////////////////
//
// $Id: StFlowCutTrack.cxx,v 1.10 2000/05/11 20:00:32 posk Exp $
//
// Author: Art Poskanzer and Raimond Snellings, LBNL, Oct 1999
//
// Description:  Class for applying track cuts
//
////////////////////////////////////////////////////////////////////////////
//
// $Log: StFlowCutTrack.cxx,v $
// Revision 1.10  2000/05/11 20:00:32  posk
// Preparation for micro and nano DSTs.
//
// Revision 1.9  2000/03/15 23:28:49  posk
// Added StFlowSelection.
//
// Revision 1.8  2000/03/02 23:02:42  posk
// Changed extensions from .hh and .cc to .h and .cxx .
//
// Revision 1.7  2000/02/29 22:00:52  posk
// Made SetPhiWeight inline, changed ImpactPar to Dca, etc.
//
// Revision 1.4  1999/12/15 22:01:23  posk
// Added StFlowConstants.hh
//
// Revision 1.3  1999/11/30 18:52:49  snelling
// First modification for the new StEvent
//
// Revision 1.2  1999/11/24 18:17:11  posk
// Put the methods which act on the data in with the data in StFlowEvent.
//
// Revision 1.1  1999/11/11 23:08:51  posk
// Rearrangement of files.
//
// Revision 1.1  1999/11/05 00:06:44  posk
// First versions of Flow cut classes.
//
//
////////////////////////////////////////////////////////////////////////////

#include <iostream.h>
#include <iomanip.h>
#include <stdlib.h>
#include "StEvent.h"
#include "StEventTypes.h"
#include "StFlowCutTrack.h"
#include "StFlowMaker.h"
#include "PhysicalConstants.h"
#include "SystemOfUnits.h"
#include "StThreeVectorD.hh"
#define PR(x) cout << "##### FlowCutTrack: " << (#x) << " = " << (x) << endl;

ClassImp(StFlowCutTrack)

//-----------------------------------------------------------------------

Int_t   StFlowCutTrack::mFitPtsCuts[2]     = {0, 0};
Float_t StFlowCutTrack::mFitOverMaxCuts[2] = {0.55, 2.};

UInt_t  StFlowCutTrack::mTrackN            = 0;     
UInt_t  StFlowCutTrack::mGoodTrackN        = 0;
UInt_t  StFlowCutTrack::mEtaSymPosN        = 0;     
UInt_t  StFlowCutTrack::mEtaSymNegN        = 0;     
UInt_t  StFlowCutTrack::mFitPtsCutN        = 0;
UInt_t  StFlowCutTrack::mFitOverMaxCutN    = 0;

//-----------------------------------------------------------------------

StFlowCutTrack::StFlowCutTrack() {
  // To apply track cuts
}

//-----------------------------------------------------------------------

StFlowCutTrack::~StFlowCutTrack() {
}

//-----------------------------------------------------------------------

Int_t StFlowCutTrack::CheckTrack(StPrimaryTrack* pTrack) {
  // Returns kTRUE if the track survives all the cuts

  mTrackN++;
  
  // Fit Points
  Int_t nFitPoints = pTrack->fitTraits().numberOfFitPoints();
  if (mFitPtsCuts[1] > mFitPtsCuts[0] && 
      (nFitPoints < mFitPtsCuts[0] || nFitPoints >= mFitPtsCuts[1])) {
    mFitPtsCutN++;
    return kFALSE;
  }

  // Fit points / max points
  Int_t nMaxPoints = pTrack->numberOfPossiblePoints();
  float fitOverMax = (nMaxPoints) ? (float)nFitPoints/(float)nMaxPoints : 0.0;
  if (mFitOverMaxCuts[1] > mFitOverMaxCuts[0] && 
      (fitOverMax < mFitOverMaxCuts[0] || fitOverMax >= mFitOverMaxCuts[1])) {
    mFitOverMaxCutN++;
    return kFALSE;
  }


  // Increment counters for Eta symmetry cut
  StThreeVectorD p = pTrack->geometry()->momentum(); 
  if (p.pseudoRapidity() > 0.) { mEtaSymPosN++; }
  else { mEtaSymNegN++; }
  mGoodTrackN++;
  return kTRUE;
}

//-----------------------------------------------------------------------

void StFlowCutTrack::PrintCutList() {
  // Prints the list of cuts
  // Call in Finish

  cout << "#######################################################" << endl;
  cout << "# Track Cut List:" << endl;
  cout << "#   FitPts cuts= " << mFitPtsCuts[0] << ", " << mFitPtsCuts[1] 
    //      << " :\t " << setprecision(4) << (float)mFitPtsCutN/(float)mTrackN/perCent 
       << " :\t " << (float)mFitPtsCutN/(float)mTrackN/perCent 
       << "% cut" << endl;
  cout << "#   FitOverMax cuts= " << mFitOverMaxCuts[0] << ", " 
    //       << mFitOverMaxCuts[1] << " :\t " << setprecision(4)
       << mFitOverMaxCuts[1] << " :\t " 
       << (float)mFitOverMaxCutN/(float)mTrackN/perCent << "% cut" << endl;
  cout << "# Good Tracks = " << (float)mGoodTrackN/(float)mTrackN/perCent
       << "%" << endl;
  cout << "#######################################################" << endl;

}
