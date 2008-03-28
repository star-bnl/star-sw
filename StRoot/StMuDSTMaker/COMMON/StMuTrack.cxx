/***************************************************************************
 *
 * $Id: StMuTrack.cxx,v 1.26 2006/07/27 18:55:41 fisyak Exp $
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
#include "StMuDSTMaker/COMMON/StMuPrimaryVertex.h"

StuProbabilityPidAlgorithm* StMuTrack::mProbabilityPidAlgorithm=0;
double StMuTrack::mProbabilityPidCentrality=0;
 
//----------------------------------------------------------------------------
//----------------------------------------------------------------------------
//----------------------------------------------------------------------------
StMuTrack::StMuTrack(const StEvent* event, const StTrack* track, const StVertex *vertex, int index2Global, int index2RichSpectra, bool l3, TObjArray *vtxList) : mId(0), mType(0), mFlag(0), mIndex2Global(index2Global), mIndex2RichSpectra(index2RichSpectra), mNHits(0), mNHitsPoss(0), mNHitsDedx(0),mNHitsFit(0), mPidProbElectron(0), mPidProbPion(0),mPidProbKaon(0),mPidProbProton(0), /* mNSigmaElectron(__NOVALUE__), mNSigmaPion(__NOVALUE__), mNSigmaKaon(__NOVALUE__), mNSigmaProton(__NOVALUE__) ,*/ mdEdx(0.), mPt(0.), mEta(0.), mPhi(0.) {

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

  if ( track->geometry() && track->geometry()->charge()) {
    mHelix = StMuHelix(track->geometry()->helix(),event->runInfo()->magneticField());
    //if (event->primaryVertex()) {
    if (vertex) {
      mP = momentumAtPrimaryVertex(event,track,vertex);
      mPt = mP.perp();
      mPhi = mP.phi();
      mEta = mP.pseudoRapidity();
      Int_t vtx_id=vtxList->IndexOf(vertex);
      if (vtx_id >= 0) {
        mVertexIndex=vtx_id;
      }
      else {
        gMessMgr->Warning() << "Track does not point to a primary vertex" << endm;
        mVertexIndex = -1;
        mDCA = StThreeVectorF(-999,-999,-999);
        mDCAGlobal = StThreeVectorF(-999,-999,-999);
      }
      mDCA = dca(track, vertex);
      if ( globalTrack ) mDCAGlobal = dca(globalTrack, vertex);

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
    else {  // vertex == 0
      mVertexIndex = -1;
      mDCA = StThreeVectorF(-999,-999,-999);
      mDCAGlobal = StThreeVectorF(-999,-999,-999);
    }
  }

  fillMuProbPidTraits(event,track);

  if ( track->outerGeometry() ) 
    mOuterHelix = StMuHelix(track->outerGeometry()->helix(),event->runInfo()->magneticField());
}

unsigned short StMuTrack::nHitsPoss() const {
  // Add 1 for primary tracks in old files
  if (mNHitsPossTpc==255 && type()==primary)
    return mNHitsPoss+1;
 
  return mNHitsPoss;
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
    return ((mNHitsPossTpc & 0xC0)==0)*mNHitsPossTpc;
    break;
  case kFtpcEastId:
    return ((mNHitsPossTpc & 0xC0)==0x40)*(mNHitsPossTpc & 0x3F);
    break;
  case kFtpcWestId:
    return ((mNHitsPossTpc & 0xC0)==0x80)*(mNHitsPossTpc & 0x3F);
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
    return ((mNHitsFitTpc & 0xC0)==0)*mNHitsFitTpc;
    break;
  case kFtpcEastId:
    return ((mNHitsFitTpc & 0xC0)==0x40)*(mNHitsFitTpc & 0x3F);
    break;
  case kFtpcWestId:
    return ((mNHitsFitTpc & 0xC0)==0x80)*(mNHitsFitTpc & 0x3F);
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

StThreeVectorF StMuTrack::dca(Int_t vtx_id) const {
  if (vtx_id == -1)
    vtx_id = StMuDst::currentVertexIndex();
  if (vtx_id==mVertexIndex)
    return mDCA;
  else if (mVertexIndex == -1)  // should not happen
    return StThreeVectorF(-999,-999,-999);
    return dca(((StMuPrimaryVertex*)StMuDst::array(muPrimaryVertex)->UncheckedAt(vtx_id))->position());
}

StThreeVectorF StMuTrack::dcaGlobal(Int_t vtx_id) const {
  if (vtx_id == -1)
    vtx_id = StMuDst::currentVertexIndex();
  if (vtx_id==mVertexIndex)
    return mDCAGlobal;
  else if (mVertexIndex == -1)  // should not happen
    return StThreeVectorF(-999,-999,-999);

  if (globalTrack())
    return globalTrack()->dca(((StMuPrimaryVertex*)StMuDst::array(muPrimaryVertex)->UncheckedAt(vtx_id))->position());
  else 
    return StThreeVectorF(-999,-999,-999);
}

StThreeVectorF StMuTrack::dca(const StThreeVectorF pos) const {
  StPhysicalHelixD helix(helix());
  double pathlength = helix.pathLength(pos, false); // do not scan periods
  return helix.at(pathlength)-pos;
}

StThreeVectorD StMuTrack::dca(const StTrack* track, const StVertex *vertex) {
  double pathlength = track->geometry()->helix().pathLength( vertex->position(), false ); // do not scan periods
  return track->geometry()->helix().at(pathlength)-vertex->position();
}

StThreeVectorD StMuTrack::momentumAtPrimaryVertex(const StEvent* event, const StTrack* track, const StVertex *vertex) {
  double pathlength = track->geometry()->helix().pathLength( vertex->position() );
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
 * Revision 1.26  2006/07/27 18:55:41  fisyak
 * Remove DCA hack used in SSD+SVT test production (P06id)
 *
 * Revision 1.24  2005/08/19 19:46:06  mvl
 * Further updates for multiple vertices. The main changes are:
 * 1) StMudst::primaryTracks() now returns a list (TObjArray*) of tracks
 *    belonging to the 'current' primary vertex. The index number of the
 *    'current' vertex can be set using StMuDst::setCurrentVertex().
 *    This also affects StMuDst::primaryTracks(int i) and
 *    StMuDst::numberOfprimaryTracks().
 * 2) refMult is now stored for all vertices, in StMuPrimaryVertex. The
 *    obvious way to access these numbers is from the StMuprimaryVertex structures,
 *    but for ebakcward compatibility a function is provided in StMuEvent as well
 *    (this is the only function taht works for existing MuDst)
 *
 * As an aside, I've also changes the internals of StMuDst::createStEvent and
 * StMuDst::fixTrackIndices() to be able to deal with a larger range of index numbers for tracks as generated by Ittf.
 *
 * BIG FAT WARNING: StMudst2StEventMaker and StMuDstFilterMaker
 * do not fully support the multiple vertex functionality yet.
 *
 * Revision 1.23  2005/07/20 04:08:47  mvl
 * Now also changed dca calculation in the other function (StMuTrack::dca(StThreeVectorF))
 * instead of only the helper function for StTrack, StVertex.
 *
 * Revision 1.22  2005/07/20 03:48:06  mvl
 * Changed dca calculation: no longer scan period of helix to get DCA.
 * This mainly affects tracks with large dcas in events with multiple vertices.
 *
 * Revision 1.21  2005/07/15 21:45:08  mvl
 * Added support for multiple primary vertices (StMuPrimaryVertex). Track Dcas are now calculated with repect to the first vertex in the list (highest rank), but another vertex number can be specified. Tarcks also store the index of the vertex they belong to (StMuTrack::vertexIndex())
 *
 * Revision 1.20  2005/03/17 21:55:00  mvl
 * Added StMuMomentumShiftMaker for applying a magnetic field scaling to the reconstructed MuDst. This class accesses StMuTrack, StMuEvent and StMuHelix and some Strangeness MuDst data members as 'friend'
 *
 * Revision 1.19  2005/02/05 01:08:12  perev
 * zero charge skip
 *
 * Revision 1.18  2004/12/15 21:35:06  mvl
 * Added initialisaion of eta, phi and pt. Affects events without main vertex.
 *
 * Revision 1.17  2004/08/14 00:53:41  mvl
 * Added 1 to possibel points for primary tracks, like in StEvent
 *
 * Revision 1.16  2004/08/12 01:34:25  mvl
 * Fixed typo when extracting number of fitted, possible points per tpc
 *
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
