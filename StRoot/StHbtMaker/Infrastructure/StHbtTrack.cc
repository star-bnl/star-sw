/***************************************************************************
 *
 * $Id: StHbtTrack.cc,v 1.3 2001/05/25 23:23:59 lisa Exp $
 *
 * Author: Frank Laue, Ohio State, laue@mps.ohio-state.edu
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 * Implementation of methods
 *
 ***************************************************************************
 * $Log: StHbtTrack.cc,v $
 * Revision 1.3  2001/05/25 23:23:59  lisa
 * Added in StHbtKink stuff
 *
 *
 ****************************************************************************/

#include "StHbtMaker/Infrastructure/StHbtTrack.hh" 

StHbtTrack::StHbtTrack(const StHbtTrack& t) { // copy constructor
  mCharge = t.mCharge;
  mNHits = t.mNHits;
  mNHitsPoss = t.mNHitsPoss; 
  mNSigmaElectron = t.mNSigmaElectron;
  mNSigmaPion = t.mNSigmaPion;
  mNSigmaKaon = t.mNSigmaKaon;
  mNSigmaProton = t.mNSigmaProton;
  mdEdx = t.mdEdx;
  mDCAxy = t.mDCAxy;
  mDCAz = t.mDCAz; 
  mChiSqXY = t.mChiSqXY;
  mChiSqZ = t.mChiSqZ;
  mP = t.mP;
  mPt = t.mPt;
  mHelix = t.mHelix;
  mMap[0] = t.mMap[0];
  mMap[1] = t.mMap[1];
  mTrackId = t.mTrackId;
};
//___________________________________________________
#ifdef __ROOT__
#include "StEventTypes.h"
#include "StEvent/StTpcDedxPidAlgorithm.h"
#include "StEventUtilities/StuRefMult.hh"
#include "StarClassLibrary/SystemOfUnits.h"   // has "tesla" in it
// #include "StEvent.h"
// #include "StEventTypes.h"
// #include "StEventUtilities/StuRefMult.hh"
// #include "StEventSummary.h"
// #include "StGlobalTrack.h"
// #include "StTrackNode.h"
// #include "StContainers.h"
// #include "StPrimaryVertex.h"
// #include "StVertex.h"
// #include "StMeasuredPoint.h"
// #include "StDedxPidTraits.h"
// #include "StTrackPidTraits.h"
// #include "StTrackGeometry.h"
// #include "StTrackDetectorInfo.h"
// #include "StParticleTypes.hh"
// #include "StTpcDedxPidAlgorithm.h"
// #include "StHit.h"
// #include "StEventInfo.h"
// #include "SystemOfUnits.h"   // has "tesla" in it
StHbtTrack::StHbtTrack(const StTrack* ST, StHbtThreeVector PrimaryVertex)
{

  StTpcDedxPidAlgorithm* PidAlgorithm = new StTpcDedxPidAlgorithm();
  if (!PidAlgorithm) cout << " StHbtTrack::StHbtTrack(StTrack*) - Whoa!! No PidAlgorithm!! " << endl;
  // while getting the bestGuess, the pidAlgorithm (StTpcDedxPidAlgorithm) is set up.
  // pointers to track and pidTraits are set 
  // So, even though BestGuess will generate a "variable not used" warning, DO NOT DELETE THE NEXT LINE
  StParticleDefinition* BestGuess = (StParticleDefinition*)ST->pidTraits(*PidAlgorithm);

  // the following just point to particle definitions in StEvent
  StElectron* Electron = StElectron::instance();
  StPionPlus* Pion = StPionPlus::instance();
  StKaonPlus* Kaon = StKaonPlus::instance();
  StProton* Proton = StProton::instance();



  // OK let's go...
  mHiddenInfo = 0;
  mCharge = ST->geometry()->charge();
  mNHits = ST->detectorInfo()->numberOfPoints(kTpcId);
  mNHitsPoss = ST->numberOfPossiblePoints(kTpcId);
  mNSigmaElectron = PidAlgorithm->numberOfSigma(Electron);
  mNSigmaPion = PidAlgorithm->numberOfSigma(Pion);
  mNSigmaKaon = PidAlgorithm->numberOfSigma(Kaon);
  mNSigmaProton = PidAlgorithm->numberOfSigma(Proton);
  mdEdx = PidAlgorithm->traits()->mean();

  mP = ST->geometry()->momentum();
  mPt = mP.perp();

  double pathlength = ST->geometry()->helix().pathLength(PrimaryVertex);
  StHbtThreeVector  DCAxyz = ST->geometry()->helix().at(pathlength)-PrimaryVertex;
  mDCAxy = DCAxyz.perp();
  mDCAz = DCAxyz.z();


  mChiSqXY = ST->fitTraits().chi2(0);
  mChiSqZ = ST->fitTraits().chi2(1);
  mHelix = ST->geometry()->helix();
  mMap[0] = ST->topologyMap().data(0);
  mMap[1] = ST->topologyMap().data(1);
  mTrackId = ST->key();
}
#endif  // __ROOT__
