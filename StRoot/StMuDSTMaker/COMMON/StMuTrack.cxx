/***************************************************************************
 *
 * $Id: StMuTrack.cxx,v 1.15 2004/08/11 00:51:54 mvl Exp $
 *
 * Author: Frank Laue, BNL, laue@bnl.gov
 ***************************************************************************/

#include "StMuTrack.h"
#include "StMuDebug.h"
#include "StMuException.hh"
#include "StEvent/StEventTypes.h"
#include "StEvent/StTrackGeometry.h"
#include "StEvent/StPrimaryVertex.h"
#include "StarClassLibrary/SystemOfUnits.h"
#include "StEvent/StTpcDedxPidAlgorithm.h"
#include "StarClassLibrary/StThreeVectorD.hh"
#include "StarClassLibrary/StPhysicalHelixD.hh"
#include "StarClassLibrary/StParticleTypes.hh"
#include "StEventUtilities/StuProbabilityPidAlgorithm.h"

StuProbabilityPidAlgorithm* StMuTrack::mProbabilityPidAlgorithm=0;
double StMuTrack::mProbabilityPidCentrality=0;
 
//----------------------------------------------------------------------------
//----------------------------------------------------------------------------
//----------------------------------------------------------------------------
StMuTrack::StMuTrack(const StEvent* event, const StTrack* track, int index2Global, int index2RichSpectra, bool l3) : mId(0), mType(0), mFlag(0), mIndex2Global(index2Global), mIndex2RichSpectra(index2RichSpectra), mNHits(0), mNHitsPoss(0), mNHitsDedx(0),mNHitsFit(0), mPidProbElectron(0), mPidProbPion(0),mPidProbKaon(0),mPidProbProton(0), /* mNSigmaElectron(__NOVALUE__), mNSigmaPion(__NOVALUE__), mNSigmaKaon(__NOVALUE__), mNSigmaProton(__NOVALUE__) ,*/ mdEdx(0.) {

  const StTrack* globalTrack = track->node()->track(global);

  mId = track->key();
  mType = track->type();
  mFlag = track->flag();
  mTopologyMap = track->topologyMap();

  // while getting the bestGuess, the pidAlgorithm (StTpcDedxPidAlgorithm) is set up.
  // pointers to track and pidTraits are set 
  // So, even though BestGuess will generate a "variable not used" warning, DO NOT DELETE THE NEXT LINES
  static StTpcDedxPidAlgorithm PidAlgorithm;
  static StElectron* Electron = StElectron::instance();
  static StPionPlus* Pion = StPionPlus::instance();
  static StKaonPlus* Kaon = StKaonPlus::instance();
  static StProton* Proton = StProton::instance();
  const StParticleDefinition* pd = track->pidTraits(PidAlgorithm);
  if (pd) {
    mNSigmaElectron = pack2Int( fabsMin(PidAlgorithm.numberOfSigma(Electron),__SIGMA_SCALE__), __SIGMA_SCALE__ );
    mNSigmaPion =     pack2Int( fabsMin(PidAlgorithm.numberOfSigma(Pion),__SIGMA_SCALE__),     __SIGMA_SCALE__ );
    mNSigmaKaon =     pack2Int( fabsMin(PidAlgorithm.numberOfSigma(Kaon),__SIGMA_SCALE__),     __SIGMA_SCALE__ );
    mNSigmaProton =   pack2Int( fabsMin(PidAlgorithm.numberOfSigma(Proton),__SIGMA_SCALE__),   __SIGMA_SCALE__ );
  } else {
    // BEWARE : Data members initialized to __NOVALUE__ was an hazardous
    // bunsiness as both NOVALUE and SIGMA_SCALE would have to be changed 
    // simultaneously (error prone). The above is equivalent while safer ...
    mNSigmaElectron = int(__NOVALUE__*__SIGMA_SCALE__); // pack2Int( fabsMin(__NOVALUE__*1.0,__SIGMA_SCALE__), __SIGMA_SCALE__ );
    mNSigmaPion =     int(__NOVALUE__*__SIGMA_SCALE__); // pack2Int( fabsMin(__NOVALUE__*1.0,__SIGMA_SCALE__), __SIGMA_SCALE__ );
    mNSigmaKaon =     int(__NOVALUE__*__SIGMA_SCALE__); // pack2Int( fabsMin(__NOVALUE__*1.0,__SIGMA_SCALE__), __SIGMA_SCALE__ );
    mNSigmaProton =   int(__NOVALUE__*__SIGMA_SCALE__); // pack2Int( fabsMin(__NOVALUE__*1.0,__SIGMA_SCALE__), __SIGMA_SCALE__ );
  }

  // if we have pid traits
  if ( PidAlgorithm.traits() ) {
    mdEdx = PidAlgorithm.traits()->mean();
    mNHitsDedx = PidAlgorithm.traits()->numberOfPoints();
  }

  // the following just point to particle definitions in StEvent
  if ( track->detectorInfo() ) {
    mFirstPoint = track->detectorInfo()->firstPoint();
    mLastPoint = track->detectorInfo()->lastPoint();
    mNHits = track->detectorInfo()->numberOfPoints();
  } 

  mNHitsPoss = track->numberOfPossiblePoints();
  int tpc_hits;
  // This only handles new data. 
  // Need to think of backward compatibiltiy mode (old StEvent)
  // Also what about initialisation for odl MuDst files?
  mNHitsPossTpc=0;
  if (track->numberOfPossiblePoints(kTpcId)==track->numberOfPossiblePoints(kFtpcEastId)) {
    // backward compatibility mode, figure out which TPC points are in
    if (track->topologyMap().hasHitInDetector(kTpcId))
      mNHitsPossTpc=track->numberOfPossiblePoints(kTpcId);
    else if (track->topologyMap().hasHitInDetector(kFtpcEastId))
      mNHitsPossTpc=(1 << 6) + track->numberOfPossiblePoints(kFtpcEastId);
    else if (track->topologyMap().hasHitInDetector(kFtpcWestId))
      mNHitsPossTpc=(2 << 6) + track->numberOfPossiblePoints(kFtpcWestId);
  } else {
    // new Ittf chain distinguishes the three TPCs properly
    if ( (tpc_hits=track->numberOfPossiblePoints(kTpcId)) )
      mNHitsPossTpc=tpc_hits; 
    else if ( (tpc_hits=track->numberOfPossiblePoints(kFtpcEastId)) )
      mNHitsPossTpc=(1 << 6) + tpc_hits;
    else if ( (tpc_hits=track->numberOfPossiblePoints(kFtpcWestId)) ) 
      mNHitsPossTpc=(2 << 6) + tpc_hits;
  }

  mNHitsPossInner=track->numberOfPossiblePoints(kSvtId) & 0x7;
  mNHitsPossInner|=(track->numberOfPossiblePoints(kSsdId) & 0x3) << 3;

  mNHitsFit = track->fitTraits().numberOfFitPoints();
  mNHitsFitTpc=0;
  if (track->numberOfPossiblePoints(kTpcId)==track->numberOfPossiblePoints(kFtpcEastId)) {
    // backward compatibility mode, figure out which TPC points are in
    if (track->topologyMap().hasHitInDetector(kTpcId))
      mNHitsFitTpc=track->fitTraits().numberOfFitPoints(kTpcId);
    else if (track->topologyMap().hasHitInDetector(kFtpcEastId))
      mNHitsFitTpc=(1 << 6) + track->fitTraits().numberOfFitPoints(kFtpcEastId);
    else if (track->topologyMap().hasHitInDetector(kFtpcWestId))
      mNHitsFitTpc=(2 << 6) + track->fitTraits().numberOfFitPoints(kFtpcWestId);
  } else {
    // new Ittf chain distinguishes the three TPCs properly
    if ( (tpc_hits=track->fitTraits().numberOfFitPoints(kTpcId)) ) 
      mNHitsFitTpc=tpc_hits; 
    else if ( (tpc_hits=track->fitTraits().numberOfFitPoints(kFtpcEastId)) ) 
      mNHitsFitTpc=(1 << 6) + tpc_hits;
    else if ( (tpc_hits=track->fitTraits().numberOfFitPoints(kFtpcWestId)) ) 
      mNHitsFitTpc=(2 << 6) + tpc_hits;
  }
  mNHitsFitInner=track->fitTraits().numberOfFitPoints(kSvtId) & 0x7;
  mNHitsFitInner|=(track->fitTraits().numberOfFitPoints(kSsdId) & 0x3) << 3;

  mChiSqXY = track->fitTraits().chi2(0);
  mChiSqZ = track->fitTraits().chi2(1);

  if ( track->geometry() ) {
    mHelix = StMuHelix(track->geometry()->helix(),event->runInfo()->magneticField());
    if (event->primaryVertex()) {
      mP = momentumAtPrimaryVertex(event,track);
      mPt = mP.perp();
      mPhi = mP.phi();
      mEta = mP.pseudoRapidity();
      mDCA = dca(event, track);
      if ( globalTrack ) mDCAGlobal = dca(event, globalTrack);

      if (!l3) { // L3TRACKS seem to break pid    
	int charge = track->geometry()->charge();
	//	StParticleDefinition* pc = (*mProbabilityPidAlgorithm)( *track, track->pidTraits() );
	mProbabilityPidAlgorithm->processPIDAsFunction(mProbabilityPidCentrality, mDCA.mag(), charge, mP.mag()/charge, mEta, mNHitsDedx, mdEdx);
	mPidProbElectron= pack2UnsignedShort( (charge>0) ? mProbabilityPidAlgorithm->beingPositronProb() : mProbabilityPidAlgorithm->beingElectronProb(), __PROB_SCALE__) ;
	mPidProbPion=     pack2UnsignedShort( (charge>0) ? mProbabilityPidAlgorithm->beingPionPlusProb() : mProbabilityPidAlgorithm->beingPionMinusProb(), __PROB_SCALE__);
	mPidProbKaon=     pack2UnsignedShort( (charge>0) ? mProbabilityPidAlgorithm->beingKaonPlusProb() : mProbabilityPidAlgorithm->beingKaonMinusProb(), __PROB_SCALE__); 
	mPidProbProton=   pack2UnsignedShort( (charge>0) ? mProbabilityPidAlgorithm->beingProtonProb()   : mProbabilityPidAlgorithm->beingAntiProtonProb(), __PROB_SCALE__); 
      }
    }
  }

  fillMuProbPidTraits(event,track);

  if ( track->outerGeometry() ) 
    mOuterHelix = StMuHelix(track->outerGeometry()->helix(),event->runInfo()->magneticField());
}

unsigned short StMuTrack::nHitsPoss(StDetectorId det) const {

  // Backward compatibility for old files
  if (mNHitsPossTpc==255) {
    if (det==kTpcId || det==kFtpcEastId || det==kFtpcWestId)
      return mTopologyMap.hasHitInDetector(det)*mNHitsPoss;
    else 
      return 0;
  }

  // New situation: decode point counts
  switch (det) {
  case kTpcId:
    return ((mNHitsPossTpc & 0xC0)==0)*mNHitsPoss;
    break;
  case kFtpcEastId:
    return ((mNHitsPossTpc & 0xC0)==0x40)*(mNHitsPoss & 0x3F);
    break;
  case kFtpcWestId:
    return ((mNHitsPossTpc & 0xC0)==0x80)*(mNHitsPoss & 0x3F);
    break;
  case kSvtId:
    return (mNHitsPossInner & 0x7);
    break;
  case kSsdId:
    return ((mNHitsPossInner & 0x18) >> 3);
    break;
  default:
    return 0;
  }

}

unsigned short StMuTrack::nHitsFit(StDetectorId det) const {
  // Backward compatibility for old files
  if (mNHitsFitTpc==255) {
    if (det==kTpcId || det==kFtpcEastId || det==kFtpcWestId)
      return mTopologyMap.hasHitInDetector(det)*mNHitsFit;
    else 
      return 0;
  }

  // New situation: decode point counts
  switch (det) {
  case kTpcId:
    return ((mNHitsFitTpc & 0xC0)==0)*mNHitsFit;
    break;
  case kFtpcEastId:
    return ((mNHitsFitTpc & 0xC0)==0x40)*(mNHitsFit & 0x3F);
    break;
  case kFtpcWestId:
    return ((mNHitsFitTpc & 0xC0)==0x80)*(mNHitsFit & 0x3F);
    break;
  case kSvtId:
    return (mNHitsFitInner & 0x7);
    break;
  case kSsdId:
    return ((mNHitsFitInner & 0x18) >> 3);
    break;
  default:
    return 0;
  }
}

StThreeVectorD StMuTrack::dca(const StEvent* event, const StTrack* track) {
  double pathlength = track->geometry()->helix().pathLength( event->primaryVertex()->position() );
  return track->geometry()->helix().at(pathlength)-event->primaryVertex()->position();
}

StThreeVectorD StMuTrack::momentumAtPrimaryVertex(const StEvent* event, const StTrack* track) {
  double pathlength = track->geometry()->helix().pathLength( event->primaryVertex()->position() );
  return track->geometry()->helix().momentumAt(pathlength,event->runInfo()->magneticField()*kilogauss);
}

StPhysicalHelixD StMuTrack::helix() const 
{
  return StPhysicalHelixD(mHelix.p(),mHelix.origin(), mHelix.b()*kilogauss, mHelix.q());}  
StPhysicalHelixD StMuTrack::outerHelix() const {return StPhysicalHelixD(mOuterHelix.p(),mOuterHelix.origin(), mOuterHelix.b()*kilogauss, mOuterHelix.q());}  

double StMuTrack::length() const { 
  return fabs( helix().pathLength(StThreeVectorD(mLastPoint)) ); }
double StMuTrack::lengthMeasured() const { 
  return fabs( helix().pathLength(StThreeVectorD(mLastPoint)) - helix().pathLength(StThreeVectorD(mFirstPoint)) ); }

int StMuTrack::bad() const 
{
   if (mFlag <= 0  )            return 10;
   if (mHelix.bad()) 		return 20;
   if (mOuterHelix.bad())	return 30;
   return 0;
}
#include "StEvent/StProbPidTraits.h"
void StMuTrack::fillMuProbPidTraits(const StEvent* e, const StTrack* t) {
  // get vector of traits; 
  StPtrVecTrackPidTraits traits = t->pidTraits(kTpcId);
  // get the StDedxPidTraits
  StDedxPidTraits* dedxPidTraits =0;
  unsigned int size = traits.size();

  if (StMuDebug::level()>=3) {
      cout << " dedxPidTraits->method() ";
  }
  for (unsigned int i = 0; i < size; i++) {
      if ( !(dedxPidTraits=dynamic_cast<StDedxPidTraits*>(traits[i])) ) continue;
      if (StMuDebug::level()>=3) {
	  cout << " " << dedxPidTraits->method();
      }
      if (dedxPidTraits->method() == kTruncatedMeanIdentifier)  {
	  mProbPidTraits.setdEdxTruncated( dedxPidTraits->mean() ); 
	  mProbPidTraits.setdEdxErrorTruncated( dedxPidTraits->errorOnMean() ); 
      }
      if (dedxPidTraits->method() == kLikelihoodFitIdentifier)  {
	  mProbPidTraits.setdEdxFit( dedxPidTraits->mean() ); 
	  mProbPidTraits.setdEdxErrorFit( dedxPidTraits->errorOnMean() ); 
	  mProbPidTraits.setdEdxTrackLength( dedxPidTraits->length() ); 
      }
  }
  if (StMuDebug::level()>=3) {
      cout << endl;
  }

  // get the StProbPidTraits 
  StProbPidTraits* probPidTraits =0;
  size = traits.size();
  for (unsigned int i = 0; i < size; i++) {
    if ( (probPidTraits=dynamic_cast<StProbPidTraits*>(traits[i])) ) {
      for (int i=0; i<mProbPidTraits.numberOfParticles(); i++) 	mProbPidTraits.setProbability(i,probPidTraits->GetProbability(i));
      mProbPidTraits.setNdf(probPidTraits->GetNDF());
    }
  } 
  
}

ClassImp(StMuTrack)


/***************************************************************************
 *
 * $Log: StMuTrack.cxx,v $
 * Revision 1.15  2004/08/11 00:51:54  mvl
 * Added topologyMap check for nHitsFit(StDetectorId) and nHitsPoss(StDetectorId) to better handle existing MuDst
 *
 * Revision 1.14  2004/08/10 22:38:30  mvl
 * Extended support for fitspoints in Svt and Tpc to work for old files + fixed some types in previous version.
 *
 * Revision 1.13  2004/08/07 02:44:05  mvl
 * Added support for fitted and possible points in different detectors, for ITTF
 *
 * Revision 1.12  2004/04/14 14:21:53  jeromel
 * Not sure why I made it - but better to preserve the sign
 *
 * Revision 1.11  2004/04/14 03:27:30  jeromel
 * Change init of mNSigma
 *
 * Revision 1.10  2004/04/08 16:21:57  subhasis
 * Initialization ,as suggested by Yuri
 *
 * Revision 1.9  2003/11/07 15:23:26  laue
 * added error on dEdx measurements to the StMuProbPidTraits
 *
 * Revision 1.8  2003/10/30 20:08:13  perev
 * Check of quality added
 *
 * Revision 1.7  2003/10/28 18:57:56  perev
 * BadData protection added
 *
 * Revision 1.6  2003/02/21 14:32:47  laue
 * Yuri's updates to the PID probabilities. dE/dx track length in TPC added
 *
 * Revision 1.5  2002/11/22 18:08:53  laue
 * Bug in logic fixed. Checking for null pointers was wrong.
 *
 * Revision 1.4  2002/11/18 14:29:31  laue
 * update for Yuri's new StProbPidTraits
 *
 * Revision 1.3  2002/09/19 21:54:01  laue
 * fix bug in length() method
 *
 * Revision 1.2  2002/08/20 19:55:49  laue
 * Doxygen comments added
 *
 * Revision 1.1  2002/03/08 17:04:18  laue
 * initial revision
 *
 *
 **************************************************************************/
