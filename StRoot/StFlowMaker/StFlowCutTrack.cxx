////////////////////////////////////////////////////////////////////////////
//
// $Id: StFlowCutTrack.cxx,v 1.19 2000/12/08 17:03:38 oldi Exp $
//
// Author: Art Poskanzer and Raimond Snellings, LBNL, Oct 1999
//
// Description:  Class for applying track cuts
//
////////////////////////////////////////////////////////////////////////////
//
// $Log: StFlowCutTrack.cxx,v $
// Revision 1.19  2000/12/08 17:03:38  oldi
// Phi weights for both FTPCs included.
//
// Revision 1.18  2000/12/06 15:38:46  oldi
// Including FTPC.
//
// Revision 1.17  2000/10/12 22:46:33  snelling
// Added support for the new pDST's and the probability pid method
//
// Revision 1.16  2000/08/31 18:58:19  posk
// For picoDST, added version number, runID, and multEta for centrality.
// Added centrality cut when reading picoDST.
// Added pt and eta selections for particles corr. wrt event plane.
//
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
#include "StEnumerations.h"
#include "SystemOfUnits.h"
#include "StThreeVectorD.hh"
#define PR(x) cout << "##### FlowCutTrack: " << (#x) << " = " << (x) << endl;

ClassImp(StFlowCutTrack)

//-----------------------------------------------------------------------

<<<<<<< StFlowCutTrack.cxx
Int_t   StFlowCutTrack::mFitPtsTpcCuts[2]  = {15, 200};
Int_t   StFlowCutTrack::mFitPtsFtpcCuts[2] = {5, 11};      // has to be greater than ten!
Float_t StFlowCutTrack::mFitOverMaxCuts[2] = {0.52, 1.1};  // has to be greater than one, otherwise ... :-[
Float_t StFlowCutTrack::mChiSqTpcCuts[2]   = {0., 0.};
Float_t StFlowCutTrack::mChiSqFtpcCuts[2]  = {0., 0.};
Float_t StFlowCutTrack::mDcaTpcCuts[2]     = {0., 1.};
Float_t StFlowCutTrack::mDcaFtpcCuts[2]    = {0., 1.};
Float_t StFlowCutTrack::mPtTpcCuts[2]      = {0.1, 2.};
Float_t StFlowCutTrack::mPtFtpcCuts[2]     = {0.1, 2.};
Float_t StFlowCutTrack::mEtaTpcCuts[2]     = {-1.3, 1.3};
Float_t StFlowCutTrack::mEtaFtpcCuts[4]    = {-4.0, -2.7, 2.7, 4.0};
=======
Int_t   StFlowCutTrack::mFitPtsTpcCuts[2]  = {15, 200};
Int_t   StFlowCutTrack::mFitPtsFtpcCuts[2] = {5, 11};      // has to be greater than ten!
Float_t StFlowCutTrack::mFitOverMaxCuts[2] = {0.52, 1.1};  // has to be greater than one, otherwise ... :-[
Float_t StFlowCutTrack::mChiSqCuts[2]      = {0., 0.};
Float_t StFlowCutTrack::mDcaCuts[2]        = {0., 1.};
Float_t StFlowCutTrack::mPtCuts[2]         = {0.1, 2.};
Float_t StFlowCutTrack::mEtaTpcCuts[2]     = {-1.3, 1.3};
Float_t StFlowCutTrack::mEtaFtpcCuts[2]    = {-4.5, 4.5};
>>>>>>> 1.18

UInt_t  StFlowCutTrack::mTrackN            = 0;     
UInt_t  StFlowCutTrack::mTpcTrackN         = 0;     
UInt_t  StFlowCutTrack::mFtpcTrackN        = 0;     
UInt_t  StFlowCutTrack::mFtpcWestTrackN    = 0;     
UInt_t  StFlowCutTrack::mFtpcEastTrackN    = 0;     
UInt_t  StFlowCutTrack::mGoodTrackN        = 0;
UInt_t  StFlowCutTrack::mGoodTpcTrackN     = 0;
UInt_t  StFlowCutTrack::mGoodFtpcTrackN    = 0;
UInt_t  StFlowCutTrack::mEtaSymPosN        = 0;     
UInt_t  StFlowCutTrack::mEtaSymNegN        = 0;     
UInt_t  StFlowCutTrack::mEtaSymPosTpcN     = 0;     
UInt_t  StFlowCutTrack::mEtaSymNegTpcN     = 0;     
UInt_t  StFlowCutTrack::mEtaSymPosFtpcN    = 0;     
UInt_t  StFlowCutTrack::mEtaSymNegFtpcN    = 0;     
UInt_t  StFlowCutTrack::mFitPtsTpcCutN     = 0;
UInt_t  StFlowCutTrack::mFitPtsFtpcCutN    = 0;
UInt_t  StFlowCutTrack::mFitOverMaxCutN    = 0;
<<<<<<< StFlowCutTrack.cxx
UInt_t  StFlowCutTrack::mFitOverMaxTpcCutN = 0;
UInt_t  StFlowCutTrack::mFitOverMaxFtpcCutN= 0;
UInt_t  StFlowCutTrack::mChiSqTpcCutN      = 0;
UInt_t  StFlowCutTrack::mChiSqFtpcCutN     = 0;
UInt_t  StFlowCutTrack::mDcaTpcCutN        = 0;
UInt_t  StFlowCutTrack::mDcaFtpcCutN       = 0;
UInt_t  StFlowCutTrack::mPtTpcCutN         = 0;
UInt_t  StFlowCutTrack::mPtFtpcCutN        = 0;
UInt_t  StFlowCutTrack::mEtaTpcCutN        = 0;
UInt_t  StFlowCutTrack::mEtaFtpcCutN       = 0;
=======
UInt_t  StFlowCutTrack::mChiSqCutN         = 0;
UInt_t  StFlowCutTrack::mPtCutN            = 0;
UInt_t  StFlowCutTrack::mEtaTpcCutN        = 0;
UInt_t  StFlowCutTrack::mEtaFtpcCutN       = 0;
UInt_t  StFlowCutTrack::mDcaCutN           = 0;
>>>>>>> 1.18

//-----------------------------------------------------------------------

StFlowCutTrack::StFlowCutTrack() {
  // To apply track cuts
}

//-----------------------------------------------------------------------

StFlowCutTrack::~StFlowCutTrack() {
}

//-----------------------------------------------------------------------

Int_t StFlowCutTrack::DetId(Float_t eta) {
    // Returns the detector Id depending on the pseudorapidity eta.

    if (TMath::Abs(eta) < 2.) {
	return kTpcId;
    }

    else if (TMath::Abs(eta) < 4.5) {
	return eta > 0. ? kFtpcWestId : kFtpcEastId;
    } 

    return -1;
}

//-----------------------------------------------------------------------

Int_t StFlowCutTrack::CheckTrack(StPrimaryTrack* pTrack) {
  // Returns kTRUE if the StEvent track survives all the cuts

<<<<<<< StFlowCutTrack.cxx
  StThreeVectorD p = pTrack->geometry()->momentum();

  float eta = p.pseudoRapidity();
  float dca = pTrack->impactParameter();
  float pt = p.perp();
  float chiSq = (float)(pTrack->fitTraits().chi2());
  Int_t nFitPoints = pTrack->fitTraits().numberOfFitPoints();
  Int_t nMaxPoints = pTrack->numberOfPossiblePoints();
  float fitOverMax = (nMaxPoints) ? (float)nFitPoints/(float)nMaxPoints : 0.0;

  if (DetId(eta) == kTpcId) {
      mTpcTrackN++;
=======
  StThreeVectorD p = pTrack->geometry()->momentum();
  float eta = p.pseudoRapidity();

  if (DetId(eta) == kTpcId) {
      mTpcTrackN++;
>>>>>>> 1.18
  }

  else if (DetId(eta) == kFtpcEastId) {
      mFtpcTrackN++;
      mFtpcEastTrackN++;
  }

<<<<<<< StFlowCutTrack.cxx
  else if (DetId(eta) == kFtpcWestId) {
      mFtpcTrackN++;
      mFtpcWestTrackN++;
  }

  mTrackN++;
  
  if (DetId(eta) == kFtpcEastId || DetId(eta) == kFtpcWestId) { // Ftpc track
      
    // dca
    if (mDcaFtpcCuts[1] > mDcaFtpcCuts[0] && 
	(dca < mDcaFtpcCuts[0] || dca >= mDcaFtpcCuts[1])) {
      mDcaFtpcCutN++;
      return kFALSE;
    }
    
    // pt
    if (mPtFtpcCuts[1] > mPtFtpcCuts[0] && 
	(pt < mPtFtpcCuts[0] || pt >= mPtFtpcCuts[1])) {
      mPtFtpcCutN++;
      return kFALSE;
    }
    
    // ChiSq
    if (mChiSqFtpcCuts[1] > mChiSqFtpcCuts[0] && 
	(chiSq < mChiSqFtpcCuts[0] || chiSq >= mChiSqFtpcCuts[1])) {
      mChiSqFtpcCutN++;
      return kFALSE;
    }
    
    // Fit Points
    if (mFitPtsFtpcCuts[1] > mFitPtsFtpcCuts[0] && 
	(nFitPoints < mFitPtsFtpcCuts[0] || nFitPoints >= mFitPtsFtpcCuts[1])) {
      mFitPtsFtpcCutN++;
      return kFALSE;
    }
    
    // Fit points / max points
    if (mFitOverMaxCuts[1] > mFitOverMaxCuts[0] && 
	(fitOverMax < mFitOverMaxCuts[0] || fitOverMax >= mFitOverMaxCuts[1])) {
      mFitOverMaxFtpcCutN++;
      mFitOverMaxCutN++;
      return kFALSE;
    }
    
    // eta
    if ((eta < 0 && mEtaFtpcCuts[1] > mEtaFtpcCuts[0] &&
	 (eta < mEtaFtpcCuts[0] || eta >= mEtaFtpcCuts[1])) ||
	(eta > 0 && mEtaFtpcCuts[3] > mEtaFtpcCuts[2] &&
	 (eta < mEtaFtpcCuts[2] || eta >= mEtaFtpcCuts[3]))) {
      mEtaFtpcCutN++;
      return kFALSE;
    }
    
    // Increment counters for Eta symmetry cut
    if (eta > 0.) {
      mEtaSymPosFtpcN++;
      mEtaSymPosN++;
    } 
    
    else {
      mEtaSymNegFtpcN++; 
      mEtaSymNegN++; 
    }
    
    mGoodFtpcTrackN++;
=======
  else if (DetId(eta) == kFtpcWestId) {
      mFtpcTrackN++;
      mFtpcWestTrackN++;
  }

  mTrackN++;
  
  // ChiSq
  float chiSq = (float)(pTrack->fitTraits().chi2());
  if (mChiSqCuts[1] > mChiSqCuts[0] && 
      (chiSq < mChiSqCuts[0] || chiSq >= mChiSqCuts[1])) {
    mChiSqCutN++;
    return kFALSE;
>>>>>>> 1.18
  }

  else { // (DetId(eta) == kTpcId) Tpc track or otherwise!!!

<<<<<<< StFlowCutTrack.cxx
    // dca
    if (mDcaTpcCuts[1] > mDcaTpcCuts[0] && 
	(dca < mDcaTpcCuts[0] || dca >= mDcaTpcCuts[1])) {
      mDcaTpcCutN++;
      return kFALSE;
    }
    
    // pt
    if (mPtTpcCuts[1] > mPtTpcCuts[0] && 
	(pt < mPtTpcCuts[0] || pt >= mPtTpcCuts[1])) {
      mPtTpcCutN++;
      return kFALSE;
    }
    
    // ChiSq
    if (mChiSqTpcCuts[1] > mChiSqTpcCuts[0] && 
	(chiSq < mChiSqTpcCuts[0] || chiSq >= mChiSqTpcCuts[1])) {
      mChiSqTpcCutN++;
      return kFALSE;
    }
    
    // Fit Points
    if (mFitPtsTpcCuts[1] > mFitPtsTpcCuts[0] && 
	(nFitPoints < mFitPtsTpcCuts[0] || nFitPoints >= mFitPtsTpcCuts[1])) {
      mFitPtsTpcCutN++;
      return kFALSE;
    }
    
    // Fit points / max points
    if (mFitOverMaxCuts[1] > mFitOverMaxCuts[0] && 
	(fitOverMax < mFitOverMaxCuts[0] || fitOverMax >= mFitOverMaxCuts[1])) {
      mFitOverMaxTpcCutN++;
      mFitOverMaxCutN++;
      return kFALSE;
    }
    
    // eta
    if (mEtaTpcCuts[1] > mEtaTpcCuts[0] && 
	(eta < mEtaTpcCuts[0] || eta >= mEtaTpcCuts[1])) {
      mEtaTpcCutN++;
      return kFALSE;
    }
    
    // Increment counters for Eta symmetry cut
    if (eta > 0.) { 
      mEtaSymPosTpcN++;
      mEtaSymPosN++;
    } 
    
    else { 
      mEtaSymNegTpcN++; 
      mEtaSymNegN++; 
    }
    
    mGoodTpcTrackN++;
=======
  // pt
  float pt = p.perp();
  if (mPtCuts[1] > mPtCuts[0] && 
      (pt < mPtCuts[0] || pt >= mPtCuts[1])) {
    mPtCutN++;
    return kFALSE;
  }

  if (DetId(eta) == kTpcId) { // Tpc track

      // Fit Points
      Int_t nFitPoints = pTrack->fitTraits().numberOfFitPoints();
      if (mFitPtsTpcCuts[1] > mFitPtsTpcCuts[0] && 
	  (nFitPoints < mFitPtsTpcCuts[0] || nFitPoints >= mFitPtsTpcCuts[1])) {
	  mFitPtsTpcCutN++;
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

      // eta
      if (mEtaTpcCuts[1] > mEtaTpcCuts[0] && 
	  (eta < mEtaTpcCuts[0] || eta >= mEtaTpcCuts[1])) {
	  mEtaTpcCutN++;
	  return kFALSE;
      }

      // Increment counters for Eta symmetry cut
      if (eta > 0.) { 
	  mEtaSymPosTpcN++;
	  mEtaSymPosN++;
      } 
      
      else { 
	  mEtaSymNegTpcN++; 
	  mEtaSymNegN++; 
      }
      
      mGoodTpcTrackN++;
  }

  else if (DetId(eta) == kFtpcEastId || DetId(eta) == kFtpcWestId) { // Ftpc track
      
      // Fit Points
      Int_t nFitPoints = pTrack->fitTraits().numberOfFitPoints();
      if (mFitPtsFtpcCuts[1] > mFitPtsFtpcCuts[0] && 
	  (nFitPoints < mFitPtsFtpcCuts[0] || nFitPoints >= mFitPtsFtpcCuts[1])) {
	  mFitPtsFtpcCutN++;
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

      // eta
      if (mEtaFtpcCuts[1] > mEtaFtpcCuts[0] && 
	  (eta < mEtaFtpcCuts[0] || eta >= mEtaFtpcCuts[1])) {
	  mEtaFtpcCutN++;
	  return kFALSE;
      }

      // Increment counters for Eta symmetry cut
      if (eta > 0.) {
	  mEtaSymPosFtpcN++;
	  mEtaSymPosN++;
      } 

      else {
	  mEtaSymNegFtpcN++; 
	  mEtaSymNegN++; 
      }

      mGoodFtpcTrackN++;
>>>>>>> 1.18
  }
<<<<<<< StFlowCutTrack.cxx
  
=======

  else return kFALSE;

>>>>>>> 1.18
  mGoodTrackN++;
  return kTRUE;
}

//-----------------------------------------------------------------------

Int_t StFlowCutTrack::CheckTrack(StGlobalTrack* gTrack) {
  // Returns kTRUE if the StEvent track survives all the cuts

<<<<<<< StFlowCutTrack.cxx
  StThreeVectorD g = gTrack->geometry()->momentum();

  float eta = g.pseudoRapidity();
  float dca = gTrack->impactParameter();
  float pt = g.perp();
  float chiSq = (float)(gTrack->fitTraits().chi2());
  Int_t nFitPoints = gTrack->fitTraits().numberOfFitPoints();
  Int_t nMaxPoints = gTrack->numberOfPossiblePoints();
  float fitOverMax = (nMaxPoints) ? (float)nFitPoints/(float)nMaxPoints : 0.0;    

  if (DetId(eta) == kTpcId) {
      mTpcTrackN++;
=======
  StThreeVectorD g = gTrack->geometry()->momentum();
  float eta = g.pseudoRapidity();

  if (DetId(eta) == kTpcId) {
      mTpcTrackN++;
>>>>>>> 1.18
  }

  else if (DetId(eta) == kFtpcEastId) {
      mFtpcTrackN++;
      mFtpcEastTrackN++;
  }

<<<<<<< StFlowCutTrack.cxx
  else if (DetId(eta) == kFtpcWestId) {
      mFtpcTrackN++;
      mFtpcWestTrackN++;
  }

  mTrackN++;
    
  if (DetId(eta) == kFtpcEastId || DetId(eta) == kFtpcWestId) { // Ftpc track

    // dca
    if (mDcaFtpcCuts[1] > mDcaFtpcCuts[0] && 
	(dca < mDcaFtpcCuts[0] || dca >= mDcaFtpcCuts[1])) {
      mDcaFtpcCutN++;
      return kFALSE;
    }

    // pt
    if (mPtFtpcCuts[1] > mPtFtpcCuts[0] && 
	(pt < mPtFtpcCuts[0] || pt >= mPtFtpcCuts[1])) {
      mPtFtpcCutN++;
      return kFALSE;
    }

    // ChiSq
    if (mChiSqFtpcCuts[1] > mChiSqFtpcCuts[0] && 
	(chiSq < mChiSqFtpcCuts[0] || chiSq >= mChiSqFtpcCuts[1])) {
      mChiSqFtpcCutN++;
      return kFALSE;
    }
=======
  else if (DetId(eta) == kFtpcWestId) {
      mFtpcTrackN++;
      mFtpcWestTrackN++;
  }

  mTrackN++;
    
  // ChiSq
  float chiSq = (float)(gTrack->fitTraits().chi2());
  if (mChiSqCuts[1] > mChiSqCuts[0] && 
      (chiSq < mChiSqCuts[0] || chiSq >= mChiSqCuts[1])) {
    mChiSqCutN++;
    return kFALSE;
  }
>>>>>>> 1.18

    // Fit Points
    if (mFitPtsFtpcCuts[1] > mFitPtsFtpcCuts[0] && 
	(nFitPoints < mFitPtsFtpcCuts[0] || nFitPoints >= mFitPtsFtpcCuts[1])) {
      mFitPtsFtpcCutN++;
      return kFALSE;
    }
      
    // Fit points / max points
    if (mFitOverMaxCuts[1] > mFitOverMaxCuts[0] && 
	(fitOverMax < mFitOverMaxCuts[0] || fitOverMax >= mFitOverMaxCuts[1])) {
      mFitOverMaxFtpcCutN++;
      mFitOverMaxCutN++;
      return kFALSE;
    }

<<<<<<< StFlowCutTrack.cxx
    // eta
    if ((eta < 0 && mEtaFtpcCuts[1] > mEtaFtpcCuts[0] &&
	 (eta < mEtaFtpcCuts[0] || eta >= mEtaFtpcCuts[1])) ||
	(eta > 0 && mEtaFtpcCuts[3] > mEtaFtpcCuts[2] &&
	 (eta < mEtaFtpcCuts[2] || eta >= mEtaFtpcCuts[3]))) {
      mEtaFtpcCutN++;
      return kFALSE;
    }
      
    // Increment counters for Eta symmetry cut
    if (eta > 0.) {
      mEtaSymPosFtpcN++;
      mEtaSymPosN++;
    } 
     
    else {
      mEtaSymNegFtpcN++;
      mEtaSymNegN++;
    }
      
    mGoodFtpcTrackN++;
  }  

  else { // (DetId(eta) == kTpcId) Tpc tracks or otherwise!!!
      
    // dca
    if (mDcaTpcCuts[1] > mDcaTpcCuts[0] && 
	(dca < mDcaTpcCuts[0] || dca >= mDcaTpcCuts[1])) {
      mDcaTpcCutN++;
      return kFALSE;
    }

    // pt
    if (mPtTpcCuts[1] > mPtTpcCuts[0] && 
	(pt < mPtTpcCuts[0] || pt >= mPtTpcCuts[1])) {
      mPtTpcCutN++;
      return kFALSE;
    }

    // ChiSq
    if (mChiSqTpcCuts[1] > mChiSqTpcCuts[0] && 
	(chiSq < mChiSqTpcCuts[0] || chiSq >= mChiSqTpcCuts[1])) {
      mChiSqTpcCutN++;
      return kFALSE;
    }

    // Fit Points
    if (mFitPtsTpcCuts[1] > mFitPtsTpcCuts[0] && 
	(nFitPoints < mFitPtsTpcCuts[0] || nFitPoints >= mFitPtsTpcCuts[1])) {
      mFitPtsTpcCutN++;
      return kFALSE;
    }
      
    // Fit points / max points
    if (mFitOverMaxCuts[1] > mFitOverMaxCuts[0] && 
	(fitOverMax < mFitOverMaxCuts[0] || fitOverMax >= mFitOverMaxCuts[1])) {
      mFitOverMaxTpcCutN++;
      mFitOverMaxCutN++;
      return kFALSE;
    }
      
    // eta
    if (mEtaTpcCuts[1] > mEtaTpcCuts[0] && 
	(eta < mEtaTpcCuts[0] || eta >= mEtaTpcCuts[1])) {
      mEtaTpcCutN++;
      return kFALSE;
    }
      
    // Increment counters for Eta symmetry cut
    if (eta > 0.) {
      mEtaSymPosTpcN++;
      mEtaSymPosN++;
    } 

    else {
      mEtaSymNegFtpcN++; 
      mEtaSymNegN++; 
    }
      
    mGoodTpcTrackN++;
  }
=======
  // pt
  float pt = g.perp();
  if (mPtCuts[1] > mPtCuts[0] && 
      (pt < mPtCuts[0] || pt >= mPtCuts[1])) {
    mPtCutN++;
    return kFALSE;
  }

  if (DetId(eta) == kTpcId) { // Tpc tracks
      
      // Fit Points
      Int_t nFitPoints = gTrack->fitTraits().numberOfFitPoints();
      if (mFitPtsTpcCuts[1] > mFitPtsTpcCuts[0] && 
	  (nFitPoints < mFitPtsTpcCuts[0] || nFitPoints >= mFitPtsTpcCuts[1])) {
	  mFitPtsTpcCutN++;
	  return kFALSE;
      }
      
      // Fit points / max points
      Int_t nMaxPoints = gTrack->numberOfPossiblePoints();
      float fitOverMax = (nMaxPoints) ? (float)nFitPoints/(float)nMaxPoints : 0.0;
      if (mFitOverMaxCuts[1] > mFitOverMaxCuts[0] && 
	  (fitOverMax < mFitOverMaxCuts[0] || fitOverMax >= mFitOverMaxCuts[1])) {
	  mFitOverMaxCutN++;
	  return kFALSE;
      }
      
      // eta
      if (mEtaTpcCuts[1] > mEtaTpcCuts[0] && 
	  (eta < mEtaTpcCuts[0] || eta >= mEtaTpcCuts[1])) {
	  mEtaTpcCutN++;
	  return kFALSE;
      }
      
      // Increment counters for Eta symmetry cut
      if (eta > 0.) {
	  mEtaSymPosTpcN++;
	  mEtaSymPosN++;
      } 

      else {
	  mEtaSymNegFtpcN++; 
	  mEtaSymNegN++; 
      }
      
      mGoodTpcTrackN++;
  }

  else if (DetId(eta) == kFtpcEastId || DetId(eta) == kFtpcWestId) { // Ftpc track

      // Fit Points
      Int_t nFitPoints = gTrack->fitTraits().numberOfFitPoints();
      if (mFitPtsFtpcCuts[1] > mFitPtsFtpcCuts[0] && 
	  (nFitPoints < mFitPtsFtpcCuts[0] || nFitPoints >= mFitPtsFtpcCuts[1])) {
	  mFitPtsFtpcCutN++;
	  return kFALSE;
      }
      
      // Fit points / max points
      Int_t nMaxPoints = gTrack->numberOfPossiblePoints();
      float fitOverMax = (nMaxPoints) ? (float)nFitPoints/(float)nMaxPoints : 0.0;
      if (mFitOverMaxCuts[1] > mFitOverMaxCuts[0] && 
	  (fitOverMax < mFitOverMaxCuts[0] || fitOverMax >= mFitOverMaxCuts[1])) {
  	  mFitOverMaxCutN++;
	  return kFALSE;
      }

      // eta
      if (mEtaFtpcCuts[1] > mEtaFtpcCuts[0] && 
	  (eta < mEtaFtpcCuts[0] || eta >= mEtaFtpcCuts[1])) {
	  mEtaFtpcCutN++;
	  return kFALSE;
      }
      
      // Increment counters for Eta symmetry cut
      if (eta > 0.) {
	  mEtaSymPosFtpcN++;
	  mEtaSymPosN++;
      } 
     
      else {
	  mEtaSymNegFtpcN++;
	  mEtaSymNegN++;
      }
      
      mGoodFtpcTrackN++;
  }  

  else return kFALSE;
>>>>>>> 1.18

  mGoodTrackN++;
  return kTRUE;
}

//-----------------------------------------------------------------------

Int_t StFlowCutTrack::CheckTrack(StFlowPicoTrack* pPicoTrack) {
  // Returns kTRUE if the picotrack survives all the cuts

<<<<<<< StFlowCutTrack.cxx
  float eta = pPicoTrack->Eta();
  float dca = pPicoTrack->Dca();
  float pt = pPicoTrack->Pt();
  float chiSq = pPicoTrack->Chi2();
  Int_t nFitPoints = pPicoTrack->FitPts();
  Int_t nMaxPoints = pPicoTrack->MaxPts();
  float fitOverMax = (nMaxPoints) ? (float)nFitPoints/(float)nMaxPoints : 0.0;

  if (DetId(eta) == kTpcId) {
      mTpcTrackN++;
=======
  float eta = pPicoTrack->Eta();

  if (DetId(eta) == kTpcId) {
      mTpcTrackN++;
>>>>>>> 1.18
  }

  else if (DetId(eta) == kFtpcEastId) {
      mFtpcTrackN++;
      mFtpcEastTrackN++;
  }

  else if (DetId(eta) == kFtpcWestId) {
      mFtpcTrackN++;
      mFtpcWestTrackN++;
  }

<<<<<<< StFlowCutTrack.cxx
  mTrackN++;
      
  if (DetId(eta) == kFtpcEastId || DetId(eta) == kFtpcWestId) { // Ftpc track
      
    // dca
    if (mDcaFtpcCuts[1] > mDcaFtpcCuts[0] && 
	(dca < mDcaFtpcCuts[0] || dca >= mDcaFtpcCuts[1])) {
      mDcaFtpcCutN++;
      return kFALSE;
    }

    // pt
    if (mPtFtpcCuts[1] > mPtFtpcCuts[0] && 
	(pt < mPtFtpcCuts[0] || pt >= mPtFtpcCuts[1])) {
      mPtFtpcCutN++;
      return kFALSE;
    }

    // ChiSq
    if (mChiSqFtpcCuts[1] > mChiSqFtpcCuts[0] && 
	(chiSq < mChiSqFtpcCuts[0] || chiSq >= mChiSqFtpcCuts[1])) {
      mChiSqFtpcCutN++;
      return kFALSE;
    }
      
    // Fit Points
    if (mFitPtsFtpcCuts[1] > mFitPtsFtpcCuts[0] && 
	(nFitPoints < mFitPtsFtpcCuts[0] || nFitPoints >= mFitPtsFtpcCuts[1])) {
      mFitPtsFtpcCutN++;
      return kFALSE;
    }
      
    // Fit points / max points
    if (mFitOverMaxCuts[1] > mFitOverMaxCuts[0] && 
	(fitOverMax < mFitOverMaxCuts[0] || fitOverMax >= mFitOverMaxCuts[1])) {
      mFitOverMaxFtpcCutN++;
      mFitOverMaxCutN++;
      return kFALSE;
    }
      
    // eta
    if ((eta < 0 && mEtaFtpcCuts[1] > mEtaFtpcCuts[0] &&
	 (eta < mEtaFtpcCuts[0] || eta >= mEtaFtpcCuts[1])) ||
	(eta > 0 && mEtaFtpcCuts[3] > mEtaFtpcCuts[2] &&
	 (eta < mEtaFtpcCuts[2] || eta >= mEtaFtpcCuts[3]))) {
      mEtaFtpcCutN++;
      return kFALSE;
    }
      
    // Increment counters for Eta symmetry cut
    if (eta > 0.) { 
      mEtaSymPosFtpcN++;
      mEtaSymPosN++;
    } 
      
    else { 
      mEtaSymNegFtpcN++;
      mEtaSymNegN++;
    }
      
    mGoodFtpcTrackN++;
=======
  mTrackN++;
      
  // ChiSq
  float chiSq = pPicoTrack->Chi2();
  if (mChiSqCuts[1] > mChiSqCuts[0] && 
      (chiSq < mChiSqCuts[0] || chiSq >= mChiSqCuts[1])) {
    mChiSqCutN++;
    return kFALSE;
>>>>>>> 1.18
  }
  
  else { // (DetId(eta) == kTpcId)   Tpc track or otherwise!!!
      
    // dca
    if (mDcaTpcCuts[1] > mDcaTpcCuts[0] && 
	(dca < mDcaTpcCuts[0] || dca >= mDcaTpcCuts[1])) {
      mDcaTpcCutN++;
      return kFALSE;
    }

    // pt
    if (mPtTpcCuts[1] > mPtTpcCuts[0] && 
	(pt < mPtTpcCuts[0] || pt >= mPtTpcCuts[1])) {
      mPtTpcCutN++;
      return kFALSE;
    }

    // ChiSq
    if (mChiSqTpcCuts[1] > mChiSqTpcCuts[0] && 
	(chiSq < mChiSqTpcCuts[0] || chiSq >= mChiSqTpcCuts[1])) {
      mChiSqTpcCutN++;
      return kFALSE;
    }

<<<<<<< StFlowCutTrack.cxx
    // Fit Points
    if (mFitPtsTpcCuts[1] > mFitPtsTpcCuts[0] && 
	(nFitPoints < mFitPtsTpcCuts[0] || nFitPoints >= mFitPtsTpcCuts[1])) {
      mFitPtsTpcCutN++;
      return kFALSE;
    }
      
    // Fit points / max points
    if (mFitOverMaxCuts[1] > mFitOverMaxCuts[0] && 
	(fitOverMax < mFitOverMaxCuts[0] || fitOverMax >= mFitOverMaxCuts[1])) {
      mFitOverMaxTpcCutN++;
      mFitOverMaxCutN++;
      return kFALSE;
    }
      
    // eta
    if (mEtaTpcCuts[1] > mEtaTpcCuts[0] && 
	(eta < mEtaTpcCuts[0] || eta >= mEtaTpcCuts[1])) {
      mEtaTpcCutN++;
      return kFALSE;
    }
      
    // Increment counters for Eta symmetry cut
    if (eta > 0.) { 
      mEtaSymPosTpcN++;
      mEtaSymPosN++;
    } 
      
    else { 
      mEtaSymNegTpcN++;
      mEtaSymNegN++;
    }
      
    mGoodTpcTrackN++;
=======
  // pt
  float pt = pPicoTrack->Pt();
  if (mPtCuts[1] > mPtCuts[0] && 
      (pt < mPtCuts[0] || pt >= mPtCuts[1])) {
    mPtCutN++;
    return kFALSE;
  }

  if (DetId(eta) == kTpcId) { // Tpc track
      
      // Fit Points
      Int_t nFitPoints = pPicoTrack->FitPts();
      if (mFitPtsTpcCuts[1] > mFitPtsTpcCuts[0] && 
	  (nFitPoints < mFitPtsTpcCuts[0] || nFitPoints >= mFitPtsTpcCuts[1])) {
	  mFitPtsTpcCutN++;
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
      
      // eta
      if (mEtaTpcCuts[1] > mEtaTpcCuts[0] && 
	  (eta < mEtaTpcCuts[0] || eta >= mEtaTpcCuts[1])) {
	  mEtaTpcCutN++;
	  return kFALSE;
      }
      
      // Increment counters for Eta symmetry cut
      if (eta > 0.) { 
	  mEtaSymPosTpcN++;
	  mEtaSymPosN++;
      } 
      
      else { 
	  mEtaSymNegTpcN++;
	  mEtaSymNegN++;
      }
      
      mGoodTpcTrackN++;
  }

  else if (DetId(eta) == kFtpcEastId || DetId(eta) == kFtpcWestId) { // Ftpc track
      
      // Fit Points
      Int_t nFitPoints = pPicoTrack->FitPts();
      if (mFitPtsFtpcCuts[1] > mFitPtsFtpcCuts[0] && 
	  (nFitPoints < mFitPtsFtpcCuts[0] || nFitPoints >= mFitPtsFtpcCuts[1])) {
	  mFitPtsFtpcCutN++;
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
      
      // eta
      if (mEtaFtpcCuts[1] > mEtaFtpcCuts[0] && 
	  (eta < mEtaFtpcCuts[0] || eta >= mEtaFtpcCuts[1])) {
	  mEtaFtpcCutN++;
	  return kFALSE;
      }
      
      // Increment counters for Eta symmetry cut
      if (eta > 0.) { 
	  mEtaSymPosFtpcN++;
	  mEtaSymPosN++;
      } 
      
      else { 
	  mEtaSymNegFtpcN++;
	  mEtaSymNegN++;
      }
      
      mGoodFtpcTrackN++;
>>>>>>> 1.18
  }

<<<<<<< StFlowCutTrack.cxx
=======
  else return kFALSE;

>>>>>>> 1.18
  mGoodTrackN++;
  return kTRUE;
}

//-----------------------------------------------------------------------

void StFlowCutTrack::PrintCutList() {
  // Prints the list of cuts
  // Call in Finish

  cout << "#######################################################" << endl;
  cout << "# Track Cut List:" << endl;
<<<<<<< StFlowCutTrack.cxx
  cout << "#   FitPts (Tpc) cuts= " << mFitPtsTpcCuts[0] << ", " << mFitPtsTpcCuts[1] 
       << " :\t " << setprecision(3) << (float)mFitPtsTpcCutN/(float)mTrackN/perCent 
       << "%\t (" << setprecision(3) << (float)mFitPtsTpcCutN/(float)mTpcTrackN/perCent << "% Tpc) cut" << endl;
  cout << "#   FitPts (Ftpc) cuts= " << mFitPtsFtpcCuts[0] << ", " << mFitPtsFtpcCuts[1] 
       << " :\t\t " << setprecision(3) << (float)mFitPtsFtpcCutN/(float)mTrackN/perCent 
       << "%\t (" << setprecision(3) << (float)mFitPtsFtpcCutN/(float)mFtpcTrackN/perCent << "% Ftpc) cut" << endl;
  cout << "#   FitOverMax cuts= " << mFitOverMaxCuts[0] << ", " << mFitOverMaxCuts[1]
       << " :\t " << setprecision(3) << (float)mFitOverMaxCutN/(float)mTrackN/perCent
       << "%\t (" << setprecision(3) << (float)mFitOverMaxTpcCutN/(float)mTpcTrackN/perCent << "% Tpc, "
       << setprecision(3) << (float)mFitOverMaxFtpcCutN/(float)mFtpcTrackN/perCent << "% Ftpc) cut" << endl;
  cout << "#   ChiSq (Tpc) cuts= " << mChiSqTpcCuts[0] << ", " << mChiSqTpcCuts[1]
       << " :\t\t " << setprecision(3) << (float)mChiSqTpcCutN/(float)mTrackN/perCent
       << "%\t (" << setprecision(3) << (float)mChiSqTpcCutN/(float)mTpcTrackN/perCent  <<" % Tpc) cut" << endl;
  cout << "#   ChiSq (Ftpc) cuts= " << mChiSqFtpcCuts[0] << ", " << mChiSqFtpcCuts[1]
       << " :\t\t " << setprecision(3) << (float)mChiSqFtpcCutN/(float)mTrackN/perCent
       << "%\t (" << setprecision(3) << (float)mChiSqFtpcCutN/(float)mFtpcTrackN/perCent  <<" % Ftpc) cut" << endl;
  cout << "#   Dca (Tpc) cuts= " << mDcaTpcCuts[0] << ", " << mDcaTpcCuts[1]
       << " :\t\t " << setprecision(3) << (float)mDcaTpcCutN/(float)mTrackN/perCent
       << "%\t (" << setprecision(3) << (float)mDcaTpcCutN/(float)mTpcTrackN/perCent  <<" % Tpc) cut" << endl;
  cout << "#   Dca (Ftpc) cuts= " << mDcaFtpcCuts[0] << ", " << mDcaFtpcCuts[1]
       << " :\t\t " << setprecision(3) << (float)mDcaFtpcCutN/(float)mTrackN/perCent
       << "%\t (" << setprecision(3) << (float)mDcaFtpcCutN/(float)mFtpcTrackN/perCent  <<" % Ftpc) cut" << endl;
  cout << "#   Pt (Tpc) cuts= " << mPtTpcCuts[0] << ", " << mPtTpcCuts[1]
       << " :\t\t " << setprecision(3) << (float)mPtTpcCutN/(float)mTrackN/perCent
       << "%\t (" << setprecision(3) << (float)mPtTpcCutN/(float)mTpcTrackN/perCent  <<" % Tpc) cut" << endl;
  cout << "#   Pt (Ftpc) cuts= " << mPtFtpcCuts[0] << ", " << mPtFtpcCuts[1]
       << " :\t\t " << setprecision(3) << (float)mPtFtpcCutN/(float)mTrackN/perCent
       << "%\t (" << setprecision(3) << (float)mPtFtpcCutN/(float)mFtpcTrackN/perCent  <<" % Ftpc) cut" << endl;
  cout << "#   Eta (Tpc) cuts= " << mEtaTpcCuts[0] << ", " << mEtaTpcCuts[1]
       << " :\t\t " << setprecision(3) << (float)mEtaTpcCutN/(float)mTrackN/perCent
       << "%\t (" << setprecision(3) << (float)mEtaTpcCutN/(float)mTpcTrackN/perCent << "% Tpc) cut" << endl;
  cout << "#   Eta (Ftpc) cuts= " << mEtaFtpcCuts[0] << ", " << mEtaFtpcCuts[1]
       << "; " << mEtaFtpcCuts[2] << ", " << mEtaFtpcCuts[3] 
       << " :\t " << setprecision(3) << (float)mEtaFtpcCutN/(float)mTrackN/perCent
       << "%\t (" << setprecision(3) << (float)mEtaFtpcCutN/(float)mFtpcTrackN/perCent << "% Ftpc) cut" << endl;
  cout << "# Good Tpc Tracks =\t " << (float)mGoodTpcTrackN/(float)mTpcTrackN/perCent
       << "%" << endl;
  cout << "# Good Ftpc Tracks =\t " << (float)mGoodFtpcTrackN/(float)mFtpcTrackN/perCent
       << "%" << endl;
  cout << "# Good Tracks =\t\t " << (float)mGoodTrackN/(float)mTrackN/perCent
=======
  cout << "#   FitPts (Tpc) cuts= " << mFitPtsTpcCuts[0] << ", " << mFitPtsTpcCuts[1] 
       << " : " << setprecision(3) << (float)mFitPtsTpcCutN/(float)mTrackN/perCent 
       << "% (" << setprecision(3) << (float)mFitPtsTpcCutN/(float)mTpcTrackN/perCent << "% Tpc) cut" << endl;
  cout << "#   FitPts (Ftpc) cuts= " << mFitPtsFtpcCuts[0] << ", " << mFitPtsFtpcCuts[1] 
       << " :\t " << setprecision(3) << (float)mFitPtsFtpcCutN/(float)mTrackN/perCent 
       << "% (" << setprecision(3) << (float)mFitPtsFtpcCutN/(float)mFtpcTrackN/perCent << "% Ftpc) cut" << endl;
  cout << "#   FitOverMax cuts= " << mFitOverMaxCuts[0] << ", " 
       << mFitOverMaxCuts[1] << " : " << setprecision(3)
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
  cout << "#   Eta (Tpc) cuts= " << mEtaTpcCuts[0] << ", " 
       << mEtaTpcCuts[1] << " :\t " << setprecision(3)
       << (float)mEtaTpcCutN/(float)mTrackN/perCent << "% (" 
       << setprecision(3) << (float)mEtaTpcCutN/(float)mTpcTrackN/perCent << "% Tpc) cut" << endl;
  cout << "#   Eta (Ftpc) cuts= " << mEtaFtpcCuts[0] << ", " 
       << mEtaFtpcCuts[1] << " : " << setprecision(3)
       << (float)mEtaFtpcCutN/(float)mTrackN/perCent << "% (" 
       << setprecision(3) << (float)mEtaFtpcCutN/(float)mFtpcTrackN/perCent << "% Ftpc) cut" << endl;
  cout << "# Good Tpc Tracks = " << (float)mGoodTpcTrackN/(float)mTpcTrackN/perCent
       << "%" << endl;
  cout << "# Good Ftpc Tracks = " << (float)mGoodFtpcTrackN/(float)mFtpcTrackN/perCent
       << "%" << endl;
  cout << "# Good Tracks = " << (float)mGoodTrackN/(float)mTrackN/perCent
>>>>>>> 1.18
       << "%" << endl;
  cout << "#######################################################" << endl;

}
