////////////////////////////////////////////////////////////////////////////
//
// $Id: StFlowCutTrack.cxx,v 1.15 2000/08/10 23:00:20 posk Exp $
//
// Author: Art Poskanzer and Raimond Snellings, LBNL, Oct 1999
//
// Description:  Class for applying track cuts
//
////////////////////////////////////////////////////////////////////////////
//
// $Log: StFlowCutTrack.cxx,v $
// Revision 1.15  2000/08/10 23:00:20  posk
// New centralities. pt and eta cuts.
//
// Revision 1.14  2000/07/20 17:25:50  posk
// Fixed bug in readPico checkEvent.
//
// Revision 1.13  2000/07/12 17:54:35  posk
// Added chi2 and dca cuts. Multiplied EtaSym by sqrt(mult).
// Apply cuts when reading picoevent file.
//
// Revision 1.12  2000/06/30 14:48:31  posk
// Using MessageMgr, changed Eta Symmetry cut.
//
// Revision 1.11  2000/06/01 18:26:34  posk
// Increased precision of Track integer data members.
//
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
#include "StFlowPicoEvent.h"
#include "StEventTypes.h"
#include "StFlowCutTrack.h"
#include "StFlowMaker.h"
#include "PhysicalConstants.h"
#include "SystemOfUnits.h"
#include "StThreeVectorD.hh"
#define PR(x) cout << "##### FlowCutTrack: " << (#x) << " = " << (x) << endl;

ClassImp(StFlowCutTrack)

//-----------------------------------------------------------------------

Int_t   StFlowCutTrack::mFitPtsCuts[2]     = {15, 200};
Float_t StFlowCutTrack::mFitOverMaxCuts[2] = {0.52, 1.};
Float_t StFlowCutTrack::mChiSqCuts[2]      = {0., 0.};
Float_t StFlowCutTrack::mDcaCuts[2]        = {0., 1.};
Float_t StFlowCutTrack::mPtCuts[2]         = {0.1, 2.};
Float_t StFlowCutTrack::mEtaCuts[2]        = {-1.5, 1.5};

UInt_t  StFlowCutTrack::mTrackN            = 0;     
UInt_t  StFlowCutTrack::mGoodTrackN        = 0;
UInt_t  StFlowCutTrack::mEtaSymPosN        = 0;     
UInt_t  StFlowCutTrack::mEtaSymNegN        = 0;     
UInt_t  StFlowCutTrack::mFitPtsCutN        = 0;
UInt_t  StFlowCutTrack::mFitOverMaxCutN    = 0;
UInt_t  StFlowCutTrack::mChiSqCutN         = 0;
UInt_t  StFlowCutTrack::mPtCutN            = 0;
UInt_t  StFlowCutTrack::mEtaCutN           = 0;
UInt_t  StFlowCutTrack::mDcaCutN           = 0;

//-----------------------------------------------------------------------

StFlowCutTrack::StFlowCutTrack() {
  // To apply track cuts
}

//-----------------------------------------------------------------------

StFlowCutTrack::~StFlowCutTrack() {
}

//-----------------------------------------------------------------------

Int_t StFlowCutTrack::CheckTrack(StPrimaryTrack* pTrack) {
  // Returns kTRUE if the StEvent track survives all the cuts

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

  // ChiSq
  float chiSq = (float)(pTrack->fitTraits().chi2());
  if (mChiSqCuts[1] > mChiSqCuts[0] && 
      (chiSq < mChiSqCuts[0] || chiSq >= mChiSqCuts[1])) {
    mChiSqCutN++;
    return kFALSE;
  }

  // dca
  float dca = pTrack->impactParameter();
  if (mDcaCuts[1] > mDcaCuts[0] && 
      (dca < mDcaCuts[0] || dca >= mDcaCuts[1])) {
    mDcaCutN++;
    return kFALSE;
  }

  StThreeVectorD p = pTrack->geometry()->momentum();

  // pt
  float pt = p.perp();
  if (mPtCuts[1] > mPtCuts[0] && 
      (pt < mPtCuts[0] || pt >= mPtCuts[1])) {
    mPtCutN++;
    return kFALSE;
  }

  // eta
  float eta = p.pseudoRapidity();
  if (mEtaCuts[1] > mEtaCuts[0] && 
      (eta < mEtaCuts[0] || eta >= mEtaCuts[1])) {
    mEtaCutN++;
    return kFALSE;
  }

  // Increment counters for Eta symmetry cut
  if (eta > 0.) { mEtaSymPosN++;
  } else { mEtaSymNegN++; }
  mGoodTrackN++;
  return kTRUE;
}

//-----------------------------------------------------------------------

Int_t StFlowCutTrack::CheckTrack(StFlowPicoTrack* pPicoTrack) {
  // Returns kTRUE if the picotrack survives all the cuts

  mTrackN++;
  
  // Fit Points
  Int_t nFitPoints = pPicoTrack->FitPts();
  if (mFitPtsCuts[1] > mFitPtsCuts[0] && 
      (nFitPoints < mFitPtsCuts[0] || nFitPoints >= mFitPtsCuts[1])) {
    mFitPtsCutN++;
    return kFALSE;
  }

  // Fit points / max points
  Int_t nMaxPoints = pPicoTrack->MaxPts();
  float fitOverMax = (nMaxPoints) ? (float)nFitPoints/(float)nMaxPoints : 0.0;
  if (mFitOverMaxCuts[1] > mFitOverMaxCuts[0] && 
      (fitOverMax < mFitOverMaxCuts[0] || fitOverMax >= mFitOverMaxCuts[1])) {
    mFitOverMaxCutN++;
    return kFALSE;
  }

  // ChiSq
  float chiSq = pPicoTrack->Chi2();
  if (mChiSqCuts[1] > mChiSqCuts[0] && 
      (chiSq < mChiSqCuts[0] || chiSq >= mChiSqCuts[1])) {
    mChiSqCutN++;
    return kFALSE;
  }

  // dca
  float dca = pPicoTrack->Dca();
  if (mDcaCuts[1] > mDcaCuts[0] && 
      (dca < mDcaCuts[0] || dca >= mDcaCuts[1])) {
    mDcaCutN++;
    return kFALSE;
  }

  // pt
  float pt = pPicoTrack->Pt();
  if (mPtCuts[1] > mPtCuts[0] && 
      (pt < mPtCuts[0] || pt >= mPtCuts[1])) {
    mPtCutN++;
    return kFALSE;
  }

  // eta
  float eta = pPicoTrack->Eta();
  if (mEtaCuts[1] > mEtaCuts[0] && 
      (eta < mEtaCuts[0] || eta >= mEtaCuts[1])) {
    mEtaCutN++;
    return kFALSE;
  }

  // Increment counters for Eta symmetry cut
  if (eta > 0.) { mEtaSymPosN++;
  } else { mEtaSymNegN++; }
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
       << " :\t " << setprecision(3) << (float)mFitPtsCutN/(float)mTrackN/perCent 
       << "% cut" << endl;
  cout << "#   FitOverMax cuts= " << mFitOverMaxCuts[0] << ", " 
       << mFitOverMaxCuts[1] << " :\t " << setprecision(3)
       << (float)mFitOverMaxCutN/(float)mTrackN/perCent << "% cut" << endl;
  cout << "#   ChiSq cuts= " << mChiSqCuts[0] << ", " 
       << mChiSqCuts[1] << " :\t\t " << setprecision(3)
       << (float)mChiSqCutN/(float)mTrackN/perCent << "% cut" << endl;
  cout << "#   Dca cuts= " << mDcaCuts[0] << ", " 
       << mDcaCuts[1] << " :\t\t " << setprecision(3)
       << (float)mDcaCutN/(float)mTrackN/perCent << "% cut" << endl;
  cout << "#   Pt cuts= " << mPtCuts[0] << ", " 
       << mPtCuts[1] << " :\t\t " << setprecision(3)
       << (float)mPtCutN/(float)mTrackN/perCent << "% cut" << endl;
  cout << "#   Eta cuts= " << mEtaCuts[0] << ", " 
       << mEtaCuts[1] << " :\t " << setprecision(3)
       << (float)mEtaCutN/(float)mTrackN/perCent << "% cut" << endl;
  cout << "# Good Tracks = " << (float)mGoodTrackN/(float)mTrackN/perCent
       << "%" << endl;
  cout << "#######################################################" << endl;

}
