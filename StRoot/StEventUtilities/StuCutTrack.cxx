////////////////////////////////////////////////////////////////////////////
//
// $Id: StuCutTrack.cxx,v 1.3 2003/09/02 17:58:09 perev Exp $
//
// Author: Art Poskanzer, LBNL, Dec 1999
//
// Description:  Class for applying track cuts
//
////////////////////////////////////////////////////////////////////////////
//
// $Log: StuCutTrack.cxx,v $
// Revision 1.3  2003/09/02 17:58:09  perev
// gcc 3.2 updates + WarnOff
//
// Revision 1.2  1999/12/21 16:12:26  posk
// Updates.
//
// Revision 1.1  1999/12/17 00:07:08  posk
// Classes for StEvent cuts.
//
// Revision 1.0  posk
// First versions of StEvent cut classes.
//
////////////////////////////////////////////////////////////////////////////

#include <Stiostream.h>
#include <stdlib.h>
#include "StEvent.h"
#include "StEventTypes.h"
#include "StuCutTrack.hh"
#include "PhysicalConstants.h"
#include "SystemOfUnits.h"
#include "StThreeVectorD.hh"
#define PR(x) cout << "##### CutTrack: " << (#x) << " = " << (x) << endl;

ClassImp(StuCutTrack)

//-----------------------------------------------------------------------

Int_t    StuCutTrack::mFitPtsCuts[2]     = {10, 200};
Float_t  StuCutTrack::mFitOverMaxCuts[2] = {0.6, 1.};

UInt_t   StuCutTrack::mTrackN            = 0;     
UInt_t   StuCutTrack::mGoodTrackN        = 0;
UInt_t   StuCutTrack::mEtaSymPosN        = 0;     
UInt_t   StuCutTrack::mEtaSymNegN        = 0;     
UInt_t   StuCutTrack::mFitPtsCutN        = 0;
UInt_t   StuCutTrack::mFitOverMaxCutN    = 0;

//-----------------------------------------------------------------------

StuCutTrack::StuCutTrack() {
  // To apply track cuts
}

//-----------------------------------------------------------------------

StuCutTrack::~StuCutTrack() {
}

//-----------------------------------------------------------------------

Int_t StuCutTrack::CheckTrack(StPrimaryTrack* pTrack) {
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
  float fitOverMax = (nMaxPoints) ? (float)nFitPoints/(float)nMaxPoints : 0.8;
  // When nMaxPoints is done correctly the 0.8 should be changed to 0.
  if (mFitOverMaxCuts[1] > mFitOverMaxCuts[0] && 
      (fitOverMax < mFitOverMaxCuts[0] || fitOverMax >= mFitOverMaxCuts[1])) {
    mFitOverMaxCutN++;
    return kFALSE;
  }


  // Increment counters for Eta symmetry cut
  double bField = 0.5*tesla;
  // When I learn how to get this from StRun, bField will be done correctly.
  StThreeVectorD p = pTrack->geometry()->helix().momentum(bField); 
  if (p.pseudoRapidity() > 0.) {
    mEtaSymPosN++;
  } else {
    mEtaSymNegN++;
  }
  mGoodTrackN++;
  return kTRUE;
}

//-----------------------------------------------------------------------

void StuCutTrack::PrintCutList() {
  // Prints the list of cuts
  // Call in Finish

  cout << "#######################################################" << endl;
  cout << "# Track Cut List:" << endl;
  cout << "#   FitPts cuts= " << mFitPtsCuts[0] << ", " << mFitPtsCuts[1] 
       << " :\t " << setprecision(4) << (float)mFitPtsCutN/(float)mTrackN/perCent 
       << "% cut" << endl;
  cout << "#   FitOverMax cuts= " << mFitOverMaxCuts[0] << ", " 
       << mFitOverMaxCuts[1] << " :\t " << setprecision(4)
       << (float)mFitOverMaxCutN/(float)mTrackN/perCent << "% cut" << endl;
  cout << "# Good Tracks = " << (float)mGoodTrackN/(float)mTrackN/perCent
       << "%" << endl;
  cout << "#######################################################" << endl;

}
