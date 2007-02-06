////////////////////////////////////////////////////////////////////////////
//
// $Id: StFlowCutTrack.cxx,v 1.43 2007/02/06 18:57:49 posk Exp $
//
// Author: Art Poskanzer and Raimond Snellings, LBNL, Oct 1999
//          FTPC added by Markus Oldenburg, MPI, Dec 2000
//          MuDst enabled by Kirill Filimonov, LBNL, Jun 2002
//
// Description:  Class for applying track cuts
//
////////////////////////////////////////////////////////////////////////////

#include <Stiostream.h>
#include <stdlib.h>
#include "StEvent.h"
#include "StTrackTopologyMap.h"
#include "StFlowPicoEvent.h"
#include "StEventTypes.h"
#include "StFlowCutTrack.h"
#include "StFlowMaker.h"
#include "PhysicalConstants.h"
#include "StEnumerations.h"
#include "SystemOfUnits.h"
#include "StThreeVectorD.hh"
#include "StMuDSTMaker/COMMON/StMuTrack.h"
#define PR(x) cout << "##### FlowCutTrack: " << (#x) << " = " << (x) << endl;

ClassImp(StFlowCutTrack)

//-----------------------------------------------------------------------

Bool_t  StFlowCutTrack::mTpcTrackCut          = kFALSE;
Int_t   StFlowCutTrack::mFitPtsTpcCuts[2]     = {15, 50};
Float_t StFlowCutTrack::mFitOverMaxCuts[2]    = {0.52, 1.05}; // greater than one!
Float_t StFlowCutTrack::mChiSqTpcCuts[2]      = {0., 0.};
Float_t StFlowCutTrack::mPtTpcCuts[2]         = {0.15, 12.};
Float_t StFlowCutTrack::mEtaTpcCuts[2]        = {-1.3, 1.3};
Int_t   StFlowCutTrack::mChgTpcCuts[2]        = {0, 0};

Bool_t  StFlowCutTrack::mFtpcTrackCut         = kFALSE;
Int_t   StFlowCutTrack::mFitPtsFtpcCuts[2]    = {5, 11};     // FitPts DO NOT include the vertex ANYMORE (i.e. the range is from 5 to 10 FTPC points). Everything < 5 and >= 11 is cut.
Float_t StFlowCutTrack::mChiSqFtpcCuts[2]     = {0., 0.};
Float_t StFlowCutTrack::mDcaFtpcCuts[2]       = {0., 0.};
Float_t StFlowCutTrack::mDcaGlobalFtpcCuts[2] = {0., 2.};
Float_t StFlowCutTrack::mPtFtpcCuts[2]        = {0.15, 6.};
Float_t StFlowCutTrack::mEtaFtpcCuts[4]       = {-4.0, -2.5, 2.5, 4.0};
Int_t   StFlowCutTrack::mChgFtpcCuts[2]       = {0, 0};

UInt_t  StFlowCutTrack::mTrackN             = 0;     
UInt_t  StFlowCutTrack::mTpcTrackN          = 0;     
UInt_t  StFlowCutTrack::mFtpcTrackN         = 0;     
UInt_t  StFlowCutTrack::mFtpcWestTrackN     = 0;     
UInt_t  StFlowCutTrack::mFtpcEastTrackN     = 0;     
UInt_t  StFlowCutTrack::mGoodTrackN         = 0;
UInt_t  StFlowCutTrack::mGoodTpcTrackN      = 0;
UInt_t  StFlowCutTrack::mGoodFtpcTrackN     = 0;
UInt_t  StFlowCutTrack::mEtaSymPosTpcN      = 0;     
UInt_t  StFlowCutTrack::mEtaSymNegTpcN      = 0;     
UInt_t  StFlowCutTrack::mEtaSymPosFtpcN     = 0;     
UInt_t  StFlowCutTrack::mEtaSymNegFtpcN     = 0;     
UInt_t  StFlowCutTrack::mFitPtsTpcCutN      = 0;
UInt_t  StFlowCutTrack::mFitPtsFtpcCutN     = 0;
UInt_t  StFlowCutTrack::mFitOverMaxCutN     = 0;
UInt_t  StFlowCutTrack::mFitOverMaxTpcCutN  = 0;
UInt_t  StFlowCutTrack::mFitOverMaxFtpcCutN = 0;
UInt_t  StFlowCutTrack::mChiSqTpcCutN       = 0;
UInt_t  StFlowCutTrack::mChiSqFtpcCutN      = 0;
UInt_t  StFlowCutTrack::mDcaFtpcCutN        = 0;
UInt_t  StFlowCutTrack::mDcaGlobalFtpcCutN  = 0;
UInt_t  StFlowCutTrack::mPtTpcCutN          = 0;
UInt_t  StFlowCutTrack::mPtFtpcCutN         = 0;
UInt_t  StFlowCutTrack::mEtaTpcCutN         = 0;
UInt_t  StFlowCutTrack::mChgTpcCutN         = 0;
UInt_t  StFlowCutTrack::mEtaFtpcCutN        = 0;
UInt_t  StFlowCutTrack::mTpcTrackCutN       = 0;
UInt_t  StFlowCutTrack::mFtpcTrackCutN      = 0;
UInt_t  StFlowCutTrack::mChgFtpcCutN        = 0;


//-----------------------------------------------------------------------

StFlowCutTrack::StFlowCutTrack() {
  // To apply track cuts
}

//-----------------------------------------------------------------------

StFlowCutTrack::~StFlowCutTrack() {
}

//-----------------------------------------------------------------------

Int_t StFlowCutTrack::CheckTrack(StTrack* pTrack) {
  // Returns kTRUE if the StEvent track survives all the cuts

  StThreeVectorD p = pTrack->geometry()->momentum();

  float charge = pTrack->geometry()->charge();
  float eta = p.pseudoRapidity();
  float dca = pTrack->impactParameter();
  float dcaGlobal = pTrack->node()->track(global)->impactParameter();  
  float pt = p.perp();
  float chiSq = (float)(pTrack->fitTraits().chi2());
  StTrackTopologyMap map = pTrack->topologyMap();
  Int_t nFitPoints = pTrack->fitTraits().numberOfFitPoints() -
    pTrack->fitTraits().numberOfFitPoints(kSvtId) -
    pTrack->fitTraits().numberOfFitPoints(kSsdId) - 1; // remove additional vertex point
  Int_t nMaxPoints = pTrack->numberOfPossiblePoints() - 
    pTrack->numberOfPossiblePoints(kSvtId) - 
    pTrack->numberOfPossiblePoints(kSsdId) - 1; // remove additional vertex point
  float fitOverMax = (nMaxPoints) ? (float)nFitPoints/(float)nMaxPoints : 0.0;

  if (map.hasHitInDetector(kTpcId) || (map.data(0) == 0 && map.data(1) == 0)) { 
    // Tpc track, or no topologyMap
      mTpcTrackN++;
  } else if (map.trackFtpcEast()) {
      mFtpcTrackN++;
      mFtpcEastTrackN++;
  } else if (map.trackFtpcWest()) {
      mFtpcTrackN++;
      mFtpcWestTrackN++;
  }

  mTrackN++;
  
  if (map.hasHitInDetector(kTpcId) || (map.data(0) == 0 && map.data(1) == 0)) { 
    // Tpc track, or no topologyMap

    // tpc all cut
    if (mTpcTrackCut) {
      mTpcTrackCut++;
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
    
    // charge
    if (mChgTpcCuts[1] > mChgTpcCuts[0] && 
	(charge < mChgTpcCuts[0] || charge > mChgTpcCuts[1])) {
      mChgTpcCutN++;
      return kFALSE;
    }

    // Increment counters for Eta symmetry cut
    if (eta > 0.) { 
      mEtaSymPosTpcN++;
    } else { 
      mEtaSymNegTpcN++; 
    }
    
    mGoodTpcTrackN++;
  } 

  else if (map.trackFtpcEast() || map.trackFtpcWest()) {
    // Ftpc track
     
    // Ftpc all cut
    if (mFtpcTrackCut) {
      mFtpcTrackCutN++;
      return kFALSE;
    }

    // dca
    if (mDcaFtpcCuts[1] > mDcaFtpcCuts[0] && 
	(dca < mDcaFtpcCuts[0] || dca >= mDcaFtpcCuts[1])) {
      mDcaFtpcCutN++;
      return kFALSE;
    }
    
    // dca global
    if (mDcaGlobalFtpcCuts[1] > mDcaGlobalFtpcCuts[0] && 
	(dcaGlobal < mDcaGlobalFtpcCuts[0] || dcaGlobal >= mDcaGlobalFtpcCuts[1])) {
      mDcaGlobalFtpcCutN++;
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

    // charge
    if (mChgFtpcCuts[1] > mChgFtpcCuts[0] && 
	(charge < mChgFtpcCuts[0] || charge > mChgFtpcCuts[1])) {
      mChgFtpcCutN++;
      return kFALSE;
    }
    
    // Increment counters for Eta symmetry cut
    if (eta > 0.) {
      mEtaSymPosFtpcN++;
    } else {
      mEtaSymNegFtpcN++; 
    }
    
    mGoodFtpcTrackN++;
  } else { // neither Tpc nor Ftpc track
    return kFALSE;
  }

  mGoodTrackN++;
  return kTRUE;
}

//-----------------------------------------------------------------------

Int_t StFlowCutTrack::CheckTrack(StFlowPicoTrack* pPicoTrack) {
  // Returns kTRUE if the picotrack survives all the cuts

  float charge =  (float) (pPicoTrack->Charge());
  float eta = pPicoTrack->Eta();
  float dca = pPicoTrack->Dca();
  float dcaGlobal = pPicoTrack->DcaGlobal();
  float pt = pPicoTrack->Pt();
  float chiSq = pPicoTrack->Chi2();
  Int_t nFitPoints = pPicoTrack->FitPts() - 1; // remove additional vertex point
  Int_t nMaxPoints = pPicoTrack->MaxPts();
  float fitOverMax = (nMaxPoints) ? (float)nFitPoints/(float)nMaxPoints : 0.0;
  StTrackTopologyMap map(pPicoTrack->TopologyMap0(), pPicoTrack->TopologyMap1());

  if (map.hasHitInDetector(kTpcId) || (map.data(0) == 0 && map.data(1) == 0)) {
    // Tpc track, or no topologyMap
      mTpcTrackN++;
  } else if (map.trackFtpcEast()) {
      mFtpcTrackN++;
      mFtpcEastTrackN++;
  } else if (map.trackFtpcWest()) {
      mFtpcTrackN++;
      mFtpcWestTrackN++;
  }

  mTrackN++;
      
  if (map.hasHitInDetector(kTpcId) || (map.data(0) == 0 && map.data(1) == 0)) {
    // Tpc track, or no topologyMap      
  
    // Tpc all cut
    if (mTpcTrackCut) {
      mTpcTrackCutN++;
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

    // charge
    if (mChgTpcCuts[1] > mChgTpcCuts[0] && 
	(charge < mChgTpcCuts[0] || charge > mChgTpcCuts[1])) {
      mChgTpcCutN++;
      return kFALSE;
    }
      
    // Increment counters for Eta symmetry cut
    if (eta > 0.) { 
      mEtaSymPosTpcN++;
    } else { 
      mEtaSymNegTpcN++;
    }
      
    mGoodTpcTrackN++;
  } 

  else if (map.trackFtpcEast() || map.trackFtpcWest()) { 
    // Ftpc track
     
    // Ftpc all cut
    if (mFtpcTrackCut) {
      mFtpcTrackCutN++;
      return kFALSE;
    }

    // dca
    if (mDcaFtpcCuts[1] > mDcaFtpcCuts[0] && 
	(dca < mDcaFtpcCuts[0] || dca >= mDcaFtpcCuts[1])) {
      mDcaFtpcCutN++;
      return kFALSE;
    }

    // dca global
    if (mDcaGlobalFtpcCuts[1] > mDcaGlobalFtpcCuts[0] && 
	(dcaGlobal < mDcaGlobalFtpcCuts[0] || dcaGlobal >= mDcaGlobalFtpcCuts[1])) {
      mDcaGlobalFtpcCutN++;
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
    
    // charge
    if (mChgFtpcCuts[1] > mChgFtpcCuts[0] && 
	(charge < mChgFtpcCuts[0] || charge > mChgFtpcCuts[1])) {
      mChgFtpcCutN++;
      return kFALSE;
    }
    
    // Increment counters for Eta symmetry cut
    if (eta > 0.) { 
      mEtaSymPosFtpcN++;
    } else { 
      mEtaSymNegFtpcN++;
    }
      
    mGoodFtpcTrackN++;
  } else { // neither Tpc nor Ftpc track
    return kFALSE;
  }

  mGoodTrackN++;
  return kTRUE;
}

//-----------------------------------------------------------------------

Int_t StFlowCutTrack::CheckTrack(StMuTrack* pMuTrack) {
  // Returns kTRUE if the micotrack survives all the cuts

  float charge =  (float) (pMuTrack->charge());
  float eta = pMuTrack->eta();
  float dca = pMuTrack->dca().mag();
  float dcaGlobal = pMuTrack->dcaGlobal().mag();
  float pt = pMuTrack->pt();
  float chiSq = pMuTrack->chi2xy(); 
  StTrackTopologyMap map(pMuTrack->topologyMap());
  Int_t nFitPoints = pMuTrack->nHitsFit() -
    pMuTrack->nHitsFit(kSvtId) -
    pMuTrack->nHitsFit(kSsdId) - 1; // remove additional vertex point
  Int_t nMaxPoints = pMuTrack->nHitsPoss() -
    pMuTrack->nHitsPoss(kSvtId) -
    pMuTrack->nHitsPoss(kSsdId) - 1; // remove additional vertex point
  float fitOverMax = (nMaxPoints) ? (float)nFitPoints/(float)nMaxPoints : 0.0;

  if (map.hasHitInDetector(kTpcId) || (map.data(0) == 0 && map.data(1) == 0)) { 
    // Tpc track, or no topologyMap
      mTpcTrackN++;
  } else if (map.trackFtpcEast()) {
      mFtpcTrackN++;
      mFtpcEastTrackN++;
  } else if (map.trackFtpcWest()) {
      mFtpcTrackN++;
      mFtpcWestTrackN++;
  }

  mTrackN++;
      
  if (map.hasHitInDetector(kTpcId) || (map.data(0) == 0 && map.data(1) == 0)) { 
    // Tpc track, or no topologyMap
      
    // Tpc all cut
    if (mTpcTrackCut) {
      mTpcTrackCutN++;
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

    // charge
    if (mChgTpcCuts[1] > mChgTpcCuts[0] && 
        (charge < mChgTpcCuts[0] || charge > mChgTpcCuts[1])) {
      mChgTpcCutN++;
      return kFALSE;
    }
      
    // Increment counters for Eta symmetry cut
    if (eta > 0.) { 
      mEtaSymPosTpcN++;
    } else { 
      mEtaSymNegTpcN++;
    }
      
    mGoodTpcTrackN++;
  } 

  else if (map.trackFtpcEast() || map.trackFtpcWest()) {
    // Ftpc track
      
    // Ftpc all cut
    if (mFtpcTrackCut) {
      mFtpcTrackCutN++;
      return kFALSE;
    }

    // dca
    if (mDcaFtpcCuts[1] > mDcaFtpcCuts[0] && 
        (dca < mDcaFtpcCuts[0] || dca >= mDcaFtpcCuts[1])) {
      mDcaFtpcCutN++;
      return kFALSE;
    }

    // dca global
    if (mDcaGlobalFtpcCuts[1] > mDcaGlobalFtpcCuts[0] && 
        (dcaGlobal < mDcaGlobalFtpcCuts[0] || dcaGlobal >= mDcaGlobalFtpcCuts[1])) {
      mDcaGlobalFtpcCutN++;
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
      
    // charge
    if (mChgFtpcCuts[1] > mChgFtpcCuts[0] && 
	(charge < mChgFtpcCuts[0] || charge > mChgFtpcCuts[1])) {
      mChgFtpcCutN++;
      return kFALSE;
    }

    // Increment counters for Eta symmetry cut
    if (eta > 0.) { 
      mEtaSymPosFtpcN++;
    } else { 
      mEtaSymNegFtpcN++;
    }
      
    mGoodFtpcTrackN++;
  } else { // neither Tpc nor Ftpc track
    return kFALSE;
  }

  mGoodTrackN++;
  return kTRUE;
}

//-----------------------------------------------------------------------

void StFlowCutTrack::PrintCutList() {
  // Prints the list of cuts
  // Call in Finish

  cout << "#######################################################" << endl;
  cout << "# Track Cut List:" << endl;

  if (mTpcTrackCut) {
    cout << "# Include Tpc tracks= FALSE :\t\t ";
  } 
  else {
    cout << "# Include Tpc tracks= TRUE :\t\t ";
  }
  cout << setprecision(3) << (float)mTpcTrackCutN/(float)mTrackN/perCent << "%\t (" 
       << setprecision(3) << (float)mTpcTrackCutN/(float)mTpcTrackN/perCent << "% Tpc) cut" << endl;
  cout << "#   FitPts (Tpc) cuts= " << mFitPtsTpcCuts[0] << ", " << mFitPtsTpcCuts[1] 
       << " :\t\t " << setprecision(3) << (float)mFitPtsTpcCutN/(float)mTrackN/perCent 
       << "%\t (" << setprecision(3) << (float)mFitPtsTpcCutN/(float)mTpcTrackN/perCent << "% Tpc) cut" << endl;
  cout << "#   FitOverMax cuts= " << mFitOverMaxCuts[0] << ", " << mFitOverMaxCuts[1]
       << " :\t " << setprecision(3) << (float)mFitOverMaxCutN/(float)mTrackN/perCent
       << "%\t (" << setprecision(3) << (float)mFitOverMaxTpcCutN/(float)mTpcTrackN/perCent << "% Tpc, "
       << setprecision(3) << ((mFtpcTrackN == 0)?0.:(float)mFitOverMaxFtpcCutN/(float)mFtpcTrackN/perCent) 
       << "% Ftpc) cut" << endl;
  cout << "#   ChiSq (Tpc) cuts= " << mChiSqTpcCuts[0] << ", " << mChiSqTpcCuts[1]
       << " :\t\t " << setprecision(3) << (float)mChiSqTpcCutN/(float)mTrackN/perCent
       << "%\t (" << setprecision(3) << (float)mChiSqTpcCutN/(float)mTpcTrackN/perCent  <<"% Tpc) cut" << endl;
  cout << "#   Pt (Tpc) cuts= " << mPtTpcCuts[0] << ", " << mPtTpcCuts[1]
       << " :\t\t " << setprecision(3) << (float)mPtTpcCutN/(float)mTrackN/perCent
       << "%\t (" << setprecision(3) << (float)mPtTpcCutN/(float)mTpcTrackN/perCent  <<"% Tpc) cut" << endl;
  cout << "#   Eta (Tpc) cuts= " << mEtaTpcCuts[0] << ", " << mEtaTpcCuts[1]
       << " :\t\t " << setprecision(3) << (float)mEtaTpcCutN/(float)mTrackN/perCent
       << "%\t (" << setprecision(3) << (float)mEtaTpcCutN/(float)mTpcTrackN/perCent << "% Tpc) cut" << endl;
  cout << "#   Chg (Tpc) cuts= " << mChgTpcCuts[0] << ", " << mChgTpcCuts[1]
       << " :\t\t " << setprecision(3) << (float)mChgTpcCutN/(float)mTrackN/perCent
       << "%\t (" << setprecision(3) << (float)mChgTpcCutN/(float)mTpcTrackN/perCent << "% Tpc) cut" << endl;

  if (mFtpcTrackCut) {
    cout << "# Include Ftpc tracks= FALSE :\t\t ";
  } 
  else {
    cout << "# Include Ftpc tracks= TRUE :\t\t ";
  }
  cout << setprecision(3) << (float)mFtpcTrackCutN/(float)mTrackN/perCent << "%\t (" 
       << setprecision(3) << (float)mFtpcTrackCutN/(float)mFtpcTrackN/perCent << "% Ftpc) cut" << endl;
  cout << "#   FitPts (Ftpc) cuts= " << mFitPtsFtpcCuts[0] << ", " << mFitPtsFtpcCuts[1] 
       << " :\t\t " << setprecision(3) << (float)mFitPtsFtpcCutN/(float)mTrackN/perCent 
       << "%\t (" << setprecision(3) << ((mFtpcTrackN == 0)?0.:(float)mFitPtsFtpcCutN/(float)mFtpcTrackN/perCent) 
       << "% Ftpc) cut" << endl;
  cout << "#   ChiSq (Ftpc) cuts= " << mChiSqFtpcCuts[0] << ", " << mChiSqFtpcCuts[1]
       << " :\t\t " << setprecision(3) << (float)mChiSqFtpcCutN/(float)mTrackN/perCent
       << "%\t (" << setprecision(3) << ((mFtpcTrackN == 0)?0.:(float)mChiSqFtpcCutN/(float)mFtpcTrackN/perCent)  
       << "% Ftpc) cut" << endl;
  cout << "#   Dca (Ftpc) cuts= " << mDcaFtpcCuts[0] << ", " << mDcaFtpcCuts[1]
       << " :\t\t " << setprecision(3) << (float)mDcaFtpcCutN/(float)mTrackN/perCent
       << "%\t (" << setprecision(3) << ((mFtpcTrackN == 0)?0.:(float)mDcaFtpcCutN/(float)mFtpcTrackN/perCent)  
       <<"% Ftpc) cut" << endl;
  cout << "#   Dca global (Ftpc) cuts= " << mDcaGlobalFtpcCuts[0] << ", " << mDcaGlobalFtpcCuts[1]
       << " :\t " << setprecision(3) << (float)mDcaGlobalFtpcCutN/(float)mTrackN/perCent
       << "%\t (" << setprecision(3) << ((mFtpcTrackN == 0)?0.:(float)mDcaGlobalFtpcCutN/(float)mFtpcTrackN/perCent)  
       <<"% Ftpc) cut" << endl;
  cout << "#   Pt (Ftpc) cuts= " << mPtFtpcCuts[0] << ", " << mPtFtpcCuts[1]
       << " :\t\t " << setprecision(3) << (float)mPtFtpcCutN/(float)mTrackN/perCent
       << "%\t (" << setprecision(3) << ((mFtpcTrackN == 0)?0.:(float)mPtFtpcCutN/(float)mFtpcTrackN/perCent)  
       << "% Ftpc) cut" << endl;
  cout << "#   Eta (Ftpc) cuts= " << mEtaFtpcCuts[0] << ", " << mEtaFtpcCuts[1]
       << "; " << mEtaFtpcCuts[2] << ", " << mEtaFtpcCuts[3] 
       << " :\t " << setprecision(3) << (float)mEtaFtpcCutN/(float)mTrackN/perCent
       << "%\t (" << setprecision(3) << ((mFtpcTrackN == 0)?0.:(float)mEtaFtpcCutN/(float)mFtpcTrackN/perCent) 
       << "% Ftpc) cut" << endl;
  cout << "#   Chg (Ftpc) cuts= " << mChgFtpcCuts[0] << ", " << mChgFtpcCuts[1]
       << " :\t\t " << setprecision(3) << (float)mChgFtpcCutN/(float)mTrackN/perCent
       << "%\t (" << setprecision(3) << (float)mChgFtpcCutN/(float)mFtpcTrackN/perCent << "% Ftpc) cut" << endl;
  cout << "# Good Tpc Tracks =\t " << (float)mGoodTpcTrackN/(float)mTpcTrackN/perCent
       << "%" << endl;
  cout << "# Good Ftpc Tracks =\t " << ((mFtpcTrackN == 0)?0.:(float)mGoodFtpcTrackN/(float)mFtpcTrackN/perCent)
       << "%" << endl;
  cout << "# Good Tracks =\t\t " << (float)mGoodTrackN/(float)mTrackN/perCent
       << "%" << endl;
  cout << "#######################################################" << endl;

}

////////////////////////////////////////////////////////////////////////////
//
// $Log: StFlowCutTrack.cxx,v $
// Revision 1.43  2007/02/06 18:57:49  posk
// In Lee Yang Zeros method, introduced recentering of Q vector.
// Reactivated eta symmetry cut.
//
// Revision 1.42  2004/12/09 23:43:34  posk
// Minor changes in code formatting.
//
// Revision 1.41  2004/08/18 00:19:17  oldi
// Several changes were necessary to comply with latest changes of MuDsts and StEvent:
//
// nHits, nFitPoints, nMaxPoints
// -----------------------------
// From now on
//  - the fit points used in StFlowMaker are the fit points within the TPC xor FTPC (vertex excluded).
//  - the max. possible points used in StFlowMAker are the max. possible points within the TPC xor FTPC (vertex excluded).
//  - the number of points (nHits; not used for analyses so far) are the total number of points on a track, i. e.
//    TPC + SVT + SSD + FTPCeast + FTPCwest [reading from HBT event gives a warning, but it seems like nobody uses it anyhow].
// - The fit/max plot (used to be (fit-1)/max) was updated accordingly.
// - The default cuts for fit points were changed (only for the FTPC, since TPC doesn't set default cuts).
// - All these changes are backward compatible, as long as you change your cuts for the fit points by 1 (the vertex used to
//   be included and is not included anymore). In other words, your results won't depend on old or new MuDst, StEvent,
//   PicoDsts as long as you use the new flow software (together with the latest MuDst and StEvent software version).
// - For backward compatibility reasons the number of fit points which is written out to the flowpicoevent.root file
//   includes the vertex. It is subtracted internally while reading back the pico files. This is completely hidden from the
//   user.
//
// zFirstPoint
// -----------
// The positions of the first point of tracks which have points in the TPC can lie outside of the TPC (the tracks can start in
// the SVT or SSD now). In this case, the first point of the track is obtained by extrapolating the track helix to the inner
// radius of the TPC.
//
// Revision 1.40  2003/09/02 17:58:11  perev
// gcc 3.2 updates + WarnOff
//
// Revision 1.39  2003/06/11 00:51:25  oldi
// Default cut for FitPtsFtpc changed to correct values (long tracks were cut before).
//
// Revision 1.38  2003/02/25 19:28:39  posk
// Changed a few unimportant default cuts.
//
// Revision 1.37  2003/01/14 14:14:09  oldi
// Possibility to exclude TPC tracks completely (= FTPC only).
//
// Revision 1.36  2003/01/10 16:42:01  oldi
// Several changes to comply with FTPC tracks:
// - Switch to include/exclude FTPC tracks introduced.
//   The same switch changes the range of the eta histograms.
// - Eta symmetry plots for FTPC tracks added and separated from TPC plots.
// - PhiWgts and related histograms for FTPC tracks split in FarEast, East,
//   West, FarWest (depending on vertex.z()).
// - Psi_Diff plots for 2 different selections and the first 2 harmonics added.
// - Cut to exclude mu-events with no primary vertex introduced.
//   (This is possible for UPC events and FTPC tracks.)
// - Global DCA cut for FTPC tracks added.
// - Global DCA cuts for event plane selection separated for TPC and FTPC tracks.
// - Charge cut for FTPC tracks added.
//
// Revision 1.35  2002/06/12 22:36:42  posk
// FitOverMax points cut/selection is now done on (FitPts - 1)/MaxPts.
//
// Revision 1.34  2002/06/10 22:50:58  posk
// pt and eta weighting now default.
// DcaGlobalPart default now 0 to 1 cm.
// Event cut order changed.
//
// Revision 1.33  2002/06/07 22:18:39  kirill
// Introduced MuDst reader
//
// Revision 1.32  2002/04/05 12:10:57  oldi
// Default values for FTPC eta cuts changed. FTPC is excluded now.
//
// Revision 1.31  2002/03/12 02:33:19  posk
// Now makes pico files in SL02c.
//
// Revision 1.30  2002/02/13 22:29:12  posk
// Pt Weight now also weights Phi Weights. Added Eta Weight, default=FALSE.
//
// Revision 1.29  2002/01/31 21:43:14  aihong
// add SetChgTpc()
//
// Revision 1.28  2001/11/13 22:43:50  posk
// Documentation updated.
//
// Revision 1.27  2001/11/09 21:10:27  posk
// Switched from CERNLIB to TMath. Little q is now normalized.
//
// Revision 1.26  2001/08/22 19:23:28  oldi
// Fix to avoid 'nan' in text output if no FTPC tracks found.
//
// Revision 1.25  2001/07/27 01:26:04  snelling
// Added and changed variables for picoEvent. Changed trackCut class to StTrack
//
// Revision 1.24  2001/07/24 22:29:06  snelling
// First attempt to get a standard root pico file again, added variables
//
// Revision 1.23  2001/05/22 20:17:17  posk
// Now can do pseudorapidity subevents.
//
// Revision 1.22  2000/12/12 20:22:05  posk
// Put log comments at end of files.
// Deleted persistent StFlowEvent (old micro DST).
//
// Revision 1.21  2000/12/10 02:01:13  oldi
// A new member (StTrackTopologyMap mTopology) was added to StFlowPicoTrack.
// The evaluation of either a track originates from the FTPC or not is
// unambiguous now. The evaluation itself is easily extendible for other
// detectors (e.g. SVT+TPC). Old flowpicoevent.root files are treated as if
// they contain TPC tracks only (backward compatibility).
//
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
// Revision 1.13  2000/07/12 17:54:35  posk
// Added chi2 and dca cuts. Multiplied EtaSym by ::sqrt(mult).
// Apply cuts when reading picoevent file.
//
// Revision 1.12  2000/06/30 14:48:31  posk
// Using MessageMgr, changed Eta Symmetry cut.
//
// Revision 1.11  2000/06/01 18:26:34  posk
// Increased precision of Track integer data members.
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
// Revision 1.1  1999/11/05 00:06:44  posk
// First versions of Flow cut classes.
//
////////////////////////////////////////////////////////////////////////////
