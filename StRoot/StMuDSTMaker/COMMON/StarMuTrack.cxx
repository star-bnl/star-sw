/***************************************************************************
 *
 * $Id: StarMuTrack.cxx,v 1.1 2002/03/05 15:41:10 jeromel Exp $
 *
 * Author: Frank Laue, BNL, laue@bnl.gov
 ***************************************************************************/

#include "StarMuTrack.h"
#include "StarMuException.hh"
#include "StEvent/StEventTypes.h"
#include "StEvent/StTrackGeometry.h"
#include "StEvent/StPrimaryVertex.h"
#include "StarClassLibrary/SystemOfUnits.h"
#include "StEvent/StTpcDedxPidAlgorithm.h"
#include "StarClassLibrary/StThreeVectorD.hh"
#include "StarClassLibrary/StPhysicalHelixD.hh"
#include "StarClassLibrary/StParticleTypes.hh"
  
#include "StEventUtilities/StuProbabilityPidAlgorithm.h"

StuProbabilityPidAlgorithm* StarMuTrack::mProbabilityPidAlgorithm=0;
double StarMuTrack::mProbabilityPidCentrality=0;

//----------------------------------------------------------------------------
//----------------------------------------------------------------------------
//----------------------------------------------------------------------------
StarMuTrack::StarMuTrack(const StEvent* event, const StTrack* track, int index2Global, int index2RichSpectra, bool l3) : mIndex2Global(index2Global), mIndex2RichSpectra(index2RichSpectra) {

  mId = track->key();
  mType = track->type();
  mFlag = track->flag();

  // while getting the bestGuess, the pidAlgorithm (StTpcDedxPidAlgorithm) is set up.
  // pointers to track and pidTraits are set 
  // So, even though BestGuess will generate a "variable not used" warning, DO NOT DELETE THE NEXT LINE
  static StTpcDedxPidAlgorithm PidAlgorithm;
  static StElectron* Electron = StElectron::instance();
  static StPionPlus* Pion = StPionPlus::instance();
  static StKaonPlus* Kaon = StKaonPlus::instance();
  static StProton* Proton = StProton::instance();
  track->pidTraits(PidAlgorithm);
  mNSigmaElectron = PidAlgorithm.numberOfSigma(Electron);
  mNSigmaPion = PidAlgorithm.numberOfSigma(Pion);
  mNSigmaKaon = PidAlgorithm.numberOfSigma(Kaon);
  mNSigmaProton = PidAlgorithm.numberOfSigma(Proton);
  if ( PidAlgorithm.traits() && !l3) 
    mdEdx = PidAlgorithm.traits()->mean();

  // the following just point to particle definitions in StEvent
  if ( track->detectorInfo() ) {
    mFirstPoint = track->detectorInfo()->firstPoint();
    mLastPoint = track->detectorInfo()->lastPoint();
    mNHits = track->detectorInfo()->numberOfPoints(kTpcId);
    mNHitsPoss = track->numberOfPossiblePoints(kTpcId);
    mNHitsDedx = track->fitTraits().numberOfFitPoints(kTpcId);
  }

  
  mChiSqXY = track->fitTraits().chi2(0);
  mChiSqZ = track->fitTraits().chi2(1);

  mTopologyMap = track->topologyMap();

  if ( track->geometry() ) {
    mHelix = StarMuHelix(track->geometry()->helix(),event->runInfo()->magneticField());
    if (event->primaryVertex()) {
      mP = momentumAtPrimaryVertex(event,track);
      mPt = mP.perp();
      mPhi = mP.phi();
      mEta = mP.pseudoRapidity();
      mDCA = dca(event, track);
      mDCAGlobal = dca(event, track->node()->track(global));

      if (!l3) { // L3TRACKS seem to break pid    
	int charge = track->geometry()->charge();
	//	StParticleDefinition* pc = (*mProbabilityPidAlgorithm)( *track, track->pidTraits() );
	mProbabilityPidAlgorithm->processPIDAsFunction(mProbabilityPidCentrality, mDCA.mag(), charge, mP.mag()/charge, mEta, mNHitsDedx, mdEdx);
	mPidProbElectron= (charge>0) ? mProbabilityPidAlgorithm->beingPositronProb() : mProbabilityPidAlgorithm->beingElectronProb() ;
	mPidProbPion= (charge>0) ? mProbabilityPidAlgorithm->beingPionPlusProb() : mProbabilityPidAlgorithm->beingPionMinusProb();
	mPidProbKaon= (charge>0) ? mProbabilityPidAlgorithm->beingKaonPlusProb() : mProbabilityPidAlgorithm->beingKaonMinusProb(); 
	mPidProbProton=  (charge>0) ? mProbabilityPidAlgorithm->beingProtonProb() : mProbabilityPidAlgorithm->beingAntiProtonProb(); 
      }
    }
  }

  if ( track->outerGeometry() ) 
    mOuterHelix = StarMuHelix(track->outerGeometry()->helix(),event->runInfo()->magneticField());
};



StThreeVectorD StarMuTrack::dca(const StEvent* event, const StTrack* track) {
  double pathlength = track->geometry()->helix().pathLength( event->primaryVertex()->position() );
  return track->geometry()->helix().at(pathlength)-event->primaryVertex()->position();
}

StThreeVectorD StarMuTrack::momentumAtPrimaryVertex(const StEvent* event, const StTrack* track) {
  double pathlength = track->geometry()->helix().pathLength( event->primaryVertex()->position() );
  return track->geometry()->helix().momentumAt(pathlength,event->runInfo()->magneticField()*kilogauss);
}



ClassImp(StarMuTrack)


/***************************************************************************
 *
 * $Log: StarMuTrack.cxx,v $
 * Revision 1.1  2002/03/05 15:41:10  jeromel
 * First version of Frank's Commone MicroDST.
 *
 *
 **************************************************************************/
