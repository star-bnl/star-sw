/***************************************************************************
 *
 * $Id: StMuTrack.cxx,v 1.55 2019/03/20 19:36:11 jdb Exp $
 *
 * Author: Frank Laue, BNL, laue@bnl.gov
 ***************************************************************************/

#include "StMuTrack.h"
#include "StMuDebug.h"
#include "StMuException.hh"
#include "StEvent/StEventTypes.h"
#include "StEvent/StTrackGeometry.h"
#include "StEvent/StPrimaryVertex.h"
#include "StEvent/StDcaGeometry.h"
///dongx
#include "StEvent/StBTofHit.h"
#include "StEvent/StBTofPidTraits.h"
#include "StEvent/StETofHit.h"
#include "StEvent/StETofPidTraits.h"
#include "StEvent/StMtdHit.h"
#include "StEvent/StMtdPidTraits.h"
#include "StarClassLibrary/SystemOfUnits.h"
#include "StEvent/StTpcDedxPidAlgorithm.h"
#include "StarClassLibrary/StThreeVectorD.hh"
#include "StarClassLibrary/StPhysicalHelixD.hh"
#include "StarClassLibrary/StParticleTypes.hh"
#include "StEventUtilities/StuProbabilityPidAlgorithm.h"
#include "StMuDSTMaker/COMMON/StMuPrimaryVertex.h"
#include "StEmcUtil/projection/StEmcPosition.h"
#include "StEmcUtil/geometry/StEmcGeom.h"
#include "StBichsel/Bichsel.h"
#include "StBichsel/StdEdxModel.h"
#include "THelixTrack.h"
#include "TMath.h"
#include "TString.h"
namespace {
	const StThreeVectorF gDummy(-999,-999,-999);
}

StuProbabilityPidAlgorithm* StMuTrack::mProbabilityPidAlgorithm=0;
double StMuTrack::mProbabilityPidCentrality=0;
 
//----------------------------------------------------------------------------
//----------------------------------------------------------------------------
//----------------------------------------------------------------------------
StMuTrack::StMuTrack(const StEvent* event, const StTrack* track, const StVertex *vertex, Int_t index2Global, Int_t index2RichSpectra, Bool_t l3, TObjArray *vtxList) : 
  mId(0), mType(0), mFlag(0), mFlagExtension(0), mIndex2Global(index2Global), mIndex2RichSpectra(index2RichSpectra), mNHits(0), mNHitsPoss(0), mNHitsDedx(0),mNHitsFit(0), 
  mPidProbElectron(0), mPidProbPion(0),mPidProbKaon(0),mPidProbProton(0), 
  /* mNSigmaElectron(__NOVALUE__), mNSigmaPion(__NOVALUE__), mNSigmaKaon(__NOVALUE__), mNSigmaProton(__NOVALUE__) ,*/ 
  mdEdx(0.), mPt(0.), mEta(0.), mPhi(0.), mIndex2Cov(-1), mIdTruth(0), mQuality(0), mIdParentVx(0)
{
  const StGlobalTrack* globalTrack = dynamic_cast<const StGlobalTrack*>(track->node()->track(global));

  mId = track->key();
  mType = track->type();
  mFlag = track->flag();
  mFlagExtension = track->flagExtension();
  mTopologyMap = track->topologyMap();
  mIdTruth = track->idTruth();
  mQuality = track->qaTruth();
  mIdParentVx = track->idParentVx();
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

  Int_t tpc_hits;
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
  if (! mNHitsPossInner) mNHitsPossInner=track->numberOfPossiblePoints(kIstId) & 0x7;
  mNHitsPossInner|=(track->numberOfPossiblePoints(kSsdId) & 0x3) << 3;
  mNHitsPossInner|=(track->numberOfPossiblePoints(kPxlId) & 0x7) << 5;
  
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
  if (! mNHitsFitInner) mNHitsFitInner=track->fitTraits().numberOfFitPoints(kIstId) & 0x7;
  mNHitsFitInner|=(track->fitTraits().numberOfFitPoints(kSsdId) & 0x3) << 3;
  mNHitsFitInner|=(track->fitTraits().numberOfFitPoints(kPxlId) & 0x7) << 5;

  mChiSqXY = track->fitTraits().chi2(0);
  mChiSqZ = track->fitTraits().chi2(1);

  if ( track->geometry() && track->geometry()->charge()) {
    mHelix = StMuHelix(track->geometry()->helix(),event->runInfo()->magneticField());
    //if (event->primaryVertex()) {
    if (vertex) {
      mP = momentumAtPrimaryVertex(event,track,vertex);
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
      if (globalTrack) {
        if (globalTrack->dcaGeometry()) {
          const StDcaGeometry *dcaGeometry = globalTrack->dcaGeometry();
          const StThreeVectorF &pvert = vertex->position();
          Double_t        vtx[3] = {pvert[0],pvert[1],pvert[2]};
          THelixTrack     thelix =  dcaGeometry->thelix();
          thelix.Move(thelix.Path(vtx));
          const Double_t *pos = thelix.Pos();
          const Double_t *mom = thelix.Dir();
          mDCAGlobal = StThreeVectorF(pos[0],pos[1],pos[2]) - vertex->position();
          if (track->type() == global) {
            mDCA = mDCAGlobal;
            mP   = StThreeVectorF(mom[0],mom[1],mom[2]);
            mP  *= dcaGeometry->momentum().mag();
          }
        }
        else {  // No StDcaGeometry; use helix
          mDCAGlobal = dca(globalTrack, vertex);
        }
      }
      else {  // No global track available
        mDCAGlobal = StThreeVectorF(-999,-999,-999);
      }

      mPt = mP.perp();
      mPhi = mP.phi();
      mEta = mP.pseudoRapidity();

      if (!l3) { // L3TRACKS seem to break pid    
	Int_t charge = track->geometry()->charge();
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
      if (globalTrack) {
        if (globalTrack->dcaGeometry()) {
          const StDcaGeometry *dcaGeometry = globalTrack->dcaGeometry();
          THelixTrack     thelix =  dcaGeometry->thelix();
          const Double_t *mom = thelix.Dir();
          if (track->type() == global) {
            mP   = StThreeVectorF(mom[0],mom[1],mom[2]);
            mP  *= dcaGeometry->momentum().mag();
          }
        }
	  }
	  mPt = mP.perp();
	  mPhi = mP.phi();
	  mEta = mP.pseudoRapidity();
	}
  }
  fillMuProbPidTraits(event,track);
 /// dongx
  mIndex2BTofHit = -1;   // filled later
  fillMuBTofPidTraits(track);

  mIndex2ETofHit = -1;    
  fillMuETofPidTraits(track);

  mIndex2MtdHit = -1;    
  fillMuMtdPidTraits(track);

  if ( track->outerGeometry() ) 
    mOuterHelix = StMuHelix(track->outerGeometry()->helix(),event->runInfo()->magneticField());
}

UShort_t StMuTrack::nHitsPoss() const {
  // Add 1 for primary tracks in old files
  if (mNHitsPossTpc==255 && type()==primary)
    return mNHitsPoss+1;
 
  return mNHitsPoss;
}

UShort_t StMuTrack::nHitsPoss(StDetectorId det) const {

  // Backward compatibility for old files
  if (mNHitsPossTpc==255) {
    if (det==kFtpcEastId || det==kFtpcWestId)
      return mTopologyMap.hasHitInDetector(det)*mNHitsPoss;
    else if (det==kTpcId)
      return (mTopologyMap.hasHitInDetector(det) || mTopologyMap.hasHitInDetector(kiTpcId) ? mNHitsPoss : 0);
    else
      return 0;
  }

  // New situation: decode point counts
  switch (det) {
  case kTpcId:
    return mNHitsPossTpc;
    // previously we had a mask on the 7 & 8th bit to distinguish FTPC hits packed into the last two bits. This is should not be needed since no track will have TPC and FTPC hits.
    // removed to make incusion of iTPC hits work, since it can be larger than 63
    // changed from : return ((mNHitsPossTpc & 0xC0)==0)*mNHitsPossTpc;
    break;
  case kFtpcEastId:
    return ((mNHitsPossTpc & 0xC0)==0x40)*(mNHitsPossTpc & 0x3F);
    break;
  case kFtpcWestId:
    return ((mNHitsPossTpc & 0xC0)==0x80)*(mNHitsPossTpc & 0x3F);
    break;
  case kSvtId:
  case kIstId:
    return (mNHitsPossInner & 0x7);
    break;
  case kSsdId:
    return ((mNHitsPossInner >> 3) & 0x3);
    break;
  case kPxlId:
    return ((mNHitsPossInner >> 5) & 0x7);
    break;
  default:
    return 0;
  }

}

UShort_t StMuTrack::nHitsFit(StDetectorId det) const {
  // Backward compatibility for old files
  if (mNHitsFitTpc==255) {
    if (det==kFtpcEastId || det==kFtpcWestId)
      return mTopologyMap.hasHitInDetector(det)*mNHitsFit;
    else if (det==kTpcId)
      return (mTopologyMap.hasHitInDetector(det) || mTopologyMap.hasHitInDetector(kiTpcId) ? mNHitsFit : 0);
    else
      return 0;
  }

  // New situation: decode point counts
  switch (det) {
  case kTpcId:
    return mNHitsFitTpc;
     // previously we had a mask on the 7 & 8th bit to distinguish FTPC hits packed into the last two bits. This is should not be needed since no track will have TPC and FTPC hits.
    // removed to make incusion of iTPC hits work, since it can be larger than 63
    // changed from : return ((mNHitsFitTpc & 0xC0)==0)*mNHitsFitTpc;
    break;
  case kFtpcEastId:
    return ((mNHitsFitTpc & 0xC0)==0x40)*(mNHitsFitTpc & 0x3F);
    break;
  case kFtpcWestId:
    return ((mNHitsFitTpc & 0xC0)==0x80)*(mNHitsFitTpc & 0x3F);
    break;
  case kSvtId:
  case kIstId:
    return (mNHitsFitInner & 0x7);
    break;
  case kSsdId:
    return ((mNHitsFitInner >> 3) & 0x3);
    break;
  case kPxlId:
    return ((mNHitsFitInner >> 5) & 0x7);
    break;
  default:
    return 0;
  }
}

Float_t StMuTrack::dcaD(Int_t vtx_id) const {
  if ((vtx_id == -1 && mVertexIndex == StMuDst::currentVertexIndex()) ||
       vtx_id == mVertexIndex) {
    StThreeVectorF dir;
    if (mType == global)
      dir = mP;
    else if (globalTrack())
      dir = globalTrack()->p();
    else
      return -999; 
    if (dir.mag() == 0)
      return -999;
    dir /= dir.mag();  
    Float_t cosl = dir.perp();
    return -dir[1]/cosl * mDCAGlobal[0] + dir[0]/cosl * mDCAGlobal[1]; 
  }
  else return -999;
}

Float_t StMuTrack::dcaZ(Int_t vtx_id) const {
  if ((vtx_id == -1 && mVertexIndex == StMuDst::currentVertexIndex()) ||
       vtx_id == mVertexIndex) {
    return mDCAGlobal.z();
  }
  else return -999;
}

StThreeVectorF StMuTrack::dca(Int_t vtx_id) const {
  if (vtx_id == -1)
    vtx_id = StMuDst::currentVertexIndex();
  if (vtx_id==mVertexIndex)
    return mDCA;
  else if (mVertexIndex == -1)  // should not happen
	return gDummy;
    if(((StMuPrimaryVertex*)StMuDst::array(muPrimaryVertex)->UncheckedAt(vtx_id))) 
		return dca(((StMuPrimaryVertex*)StMuDst::array(muPrimaryVertex)->UncheckedAt(vtx_id))->position());
	else return gDummy;
}

StThreeVectorF StMuTrack::dcaGlobal(Int_t vtx_id) const {
  if (vtx_id == -1)
    vtx_id = StMuDst::currentVertexIndex();
  if (vtx_id==mVertexIndex) {
    return mDCAGlobal;
  }
  else if (mVertexIndex == -1)  { // should not happen
    return gDummy;
  }

	const StMuTrack *gTrack = (mType == global) ? this : globalTrack();

	if (gTrack && ((StMuPrimaryVertex*)StMuDst::array(muPrimaryVertex)->UncheckedAt(vtx_id))){
		return gTrack->dca(((StMuPrimaryVertex*)StMuDst::array(muPrimaryVertex)->UncheckedAt(vtx_id))->position());
	}
	else {
    return gDummy;
  }
}

StThreeVectorF StMuTrack::dca(const StThreeVectorF &pos) const {
  const StPhysicalHelixD &hlx = helix();
  double pathlength = hlx.pathLength(pos, false); // do not scan periods
  return hlx.at(pathlength)-pos;
}

StThreeVectorD StMuTrack::dca(const StTrack* track, const StVertex *vertex) const{
  double pathlength = track->geometry()->helix().pathLength( vertex->position(), false ); // do not scan periods
  return track->geometry()->helix().at(pathlength)-vertex->position();
}

StThreeVectorD StMuTrack::momentumAtPrimaryVertex(const StEvent* event, const StTrack* track, const StVertex *vertex) const{
  double pathlength = track->geometry()->helix().pathLength( vertex->position() );
  return track->geometry()->helix().momentumAt(pathlength,event->runInfo()->magneticField()*kilogauss);
}

StPhysicalHelixD StMuTrack::helix() const 
{
  return StPhysicalHelixD(mHelix.p(),mHelix.origin(), mHelix.b()*kilogauss, mHelix.q());
}
  
StPhysicalHelixD StMuTrack::outerHelix() const {
  return StPhysicalHelixD(mOuterHelix.p(),mOuterHelix.origin(), mOuterHelix.b()*kilogauss, mOuterHelix.q());
}  

double StMuTrack::length() const { 
  return fabs( helix().pathLength(StThreeVectorD(mLastPoint)) ); }
double StMuTrack::lengthMeasured() const { 
  return fabs( helix().pathLength(StThreeVectorD(mLastPoint)) - helix().pathLength(StThreeVectorD(mFirstPoint)) ); }

Int_t StMuTrack::bad() const 
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
  UInt_t size = traits.size();

  if (StMuDebug::level()>=3) {
      cout << " dedxPidTraits->method() ";
  }
  for (UInt_t i = 0; i < size; i++) {
      if ( !(dedxPidTraits=dynamic_cast<StDedxPidTraits*>(traits[i])) ) continue;
      if (StMuDebug::level()>=3) {
	  cout << " " << dedxPidTraits->method();
      }
      if (dedxPidTraits->method() == kTruncatedMeanIdentifier)  {
	  mProbPidTraits.setdEdxTruncated( dedxPidTraits->mean() ); 
	  mProbPidTraits.setdEdxErrorTruncated( dedxPidTraits->errorOnMean() ); 
	  mProbPidTraits.setLog2dX( dedxPidTraits->log2dX() );
      }
      if (dedxPidTraits->method() == kLikelihoodFitIdentifier)  {
	  mProbPidTraits.setdEdxFit( dedxPidTraits->mean() ); 
	  mProbPidTraits.setdEdxErrorFit( dedxPidTraits->errorOnMean() ); 
	  mProbPidTraits.setdEdxTrackLength( dedxPidTraits->length() ); 
	  mProbPidTraits.setLog2dX( dedxPidTraits->log2dX() );
      }
      if (dedxPidTraits->method() == kOtherMethodIdentifier)  {
         mProbPidTraits.setdNdxFit( dedxPidTraits->mean() ); 
         mProbPidTraits.setdNdxErrorFit( dedxPidTraits->errorOnMean() ); 
         mProbPidTraits.setdEdxTrackLength( dedxPidTraits->length() ); 
         mProbPidTraits.setLog2dX( dedxPidTraits->log2dX() );
      }
  }
  if (StMuDebug::level()>=3) {
      cout << endl;
  }

  // get the StProbPidTraits 
  StProbPidTraits* probPidTraits =0;
  size = traits.size();
  for (UInt_t i = 0; i < size; i++) {
    if ( (probPidTraits=dynamic_cast<StProbPidTraits*>(traits[i])) ) {
      for (Int_t i=0; i<mProbPidTraits.numberOfParticles(); i++) 	mProbPidTraits.setProbability(i,probPidTraits->GetProbability(i));
      mProbPidTraits.setNdf(probPidTraits->GetNDF());
    }
  } 
  
}

void StMuTrack::fillMuBTofPidTraits(const StTrack* t) {
  StBTofPidTraits* btofPidTraits = 0;
  StPtrVecTrackPidTraits traits = t->pidTraits(kTofId);
  UInt_t size = traits.size();
  for (UInt_t i = 0; i < size; i++) {
    if ( (btofPidTraits=dynamic_cast<StBTofPidTraits*>(traits[i])) ) {   
      mBTofPidTraits.setBTofPidTraits(btofPidTraits);
    }    
  }
}

void StMuTrack::fillMuETofPidTraits( const StTrack* t ) {
  StETofPidTraits* etofPidTraits = nullptr;

  // set etof pid traits per default to 0 ...
  mETofPidTraits.setETofPidTraits( etofPidTraits );

  StPtrVecTrackPidTraits traits = t->pidTraits( kETofId );
  UInt_t size = traits.size();
  for( UInt_t i = 0; i < size; i++ ) {
    if( ( etofPidTraits = dynamic_cast< StETofPidTraits* >( traits[ i ] ) ) ) {   
      mETofPidTraits.setETofPidTraits( etofPidTraits );
    }    
  }
}


void StMuTrack::fillMuMtdPidTraits(const StTrack* t) {
  StMtdPidTraits* mtdPidTraits = 0;
  StPtrVecTrackPidTraits traits = t->pidTraits(kMtdId);
  UInt_t size = traits.size();
  for (UInt_t i = 0; i < size; i++) {
    if ( (mtdPidTraits=dynamic_cast<StMtdPidTraits*>(traits[i])) ) {   
      mMtdPidTraits.setMtdPidTraits(mtdPidTraits);
    }    
  }
}

#if 0
void StMuTrack::Print(Option_t *option) const {
  //
  // Print out some of the essential track info. 
  // Note: more is stored in the track; may expand printout
  // or specify options some time
  //


  if (mType == global)
    cout << "Global ";
  else if (mType == primary)
    cout << "Primary ";
  else    
    cout << "Other type ";
  cout << "track, id " << mId << ", flag " << mFlag << " (>0 is OK)" << endl;

  if (mVertexIndex != 0) 
    cout << "Not assigned to primary vertex ( vertex Index " << mVertexIndex << " )" << endl;

  cout << "momentum " << mP << endl;
  cout << "eta  " << mEta << ", phi " << mPhi << ", pt " << mPt << endl;
  cout << "DCA  " << mDCA << endl;
  cout << "\t radial " << dcaD()  << ", z " << dcaZ() << endl;
  cout << "global DCA " << mDCAGlobal << endl;
  cout << "Total hits: " << nHits() << ", fitted " << nHitsFit()
       << "\t ( TPC "
       << nHitsFit(kTpcId) << ", FTPC "
       << nHitsFit(kFtpcEastId) + nHitsFit(kFtpcWestId) << ", SVT "  
       << nHitsFit(kSvtId) << ", SSD "
       << nHitsFit(kSsdId) << " ) " << endl;

  cout << "Possible points: " << nHitsPoss() << " \t( TPC "
       << nHitsPoss(kTpcId) << ", FTPC "
       << nHitsPoss(kFtpcEastId) + nHitsPoss(kFtpcWestId) << ", SVT "
       << nHitsPoss(kSvtId) << ", SSD "
       << nHitsPoss(kSsdId) << " ) " << endl;

  cout << "\t first point " << mFirstPoint << endl;
  cout << "\t last point  " << mLastPoint << endl;
  //cout << "\t inner helix origin " << pr_track->helix().origin() << endl; 

}
#endif
ostream&  operator<<(ostream& os, const StMuTrack& v) {
  if      (v.type() == global)  os << "Gl ";
  else if (v.type() == primary) os << "Pr ";
  else                          os << "   ";
  os << Form("id:%5i fl:%5i vx:%3i p:%8.3f %8.3f %8.3f",v.id(),v.flag(),v.vertexIndex(), v.p().x(), v.p().y(), v.p().z());
  os << Form(" q:%2i eta:%6.3f phi:%6.3f pT: %6.3f",v.charge(),v.eta(),v.phi(),v.pt());
  os << Form(" DCA [%d]:%6.3f %6.3f %6.3f", v.index2Cov(), v.dca().x(),v.dca().y(),v.dca().z());
  os << Form(" Total hits:%2i fitted:%2i poss:%2i",v.nHits(),v.nHitsFit(),v.nHitsPoss());
  os << Form(" Points F: %6.3f %6.3f %6.3f L: %6.3f %6.3f %6.3f", 
	     v.firstPoint().x(),v.firstPoint().y(),v.firstPoint().z(),
	     v.lastPoint().x(),v.lastPoint().y(),v.lastPoint().z());
  os << Form(" idT %4i qa %2i",v.idTruth(), v.qaTruth());
  os << Form(" idParentVx %d", v.idParentVx());
  return os;
}

void StMuTrack::Print(Option_t *option) const {cout << *this << endl;}

const StMuTrack* StMuTrack::primaryTrack() const {

	if(mType==1) return this;
	const StMuTrack *prim = 0;

//	StMuDst *tMuDst = 
	
	//For old MuDsts where there was one vertex per event
	if(mIndex2Global==-1){
		Int_t nVer = StMuDst::numberOfPrimaryVertices();
		StMuDst::fixTrackIndicesG(nVer);
	}
	if(mIndex2Global < 0 ) return prim;
	
	if(StMuDst::numberOfPrimaryVertices()==0) return StMuDst::primaryTracks(mIndex2Global);
	Int_t curVer =  StMuDst::currentVertexIndex();
	StMuDst::setVertexIndex(mVertexIndex);
	prim =  StMuDst::primaryTracks(mIndex2Global);
	StMuDst::setVertexIndex(curVer);
	return prim;
}

Int_t StMuTrack::vertexIndex() const {

	//For old MuDsts where there was one vertex per event
	if (StMuDst::numberOfPrimaryVertices()==0){
		if(!(fabs(StMuDst::event()->primaryVertexPosition().x()) < 1.e-5 && fabs(StMuDst::event()->primaryVertexPosition().y()) < 1.e-5 && fabs(StMuDst::event()->primaryVertexPosition().z()) < 1.e-5)){
			if(primaryTrack()!=0) return 0;
		}
		else return -1;
	}
	if(this->type()==1) return mVertexIndex;
	if(primaryTrack()!=0) {
		Int_t index = primaryTrack()->vertexIndex();
		return index;
	}
	else return -1;
}

TArrayI StMuTrack::getTower(Bool_t useExitRadius,Int_t det) const{ //1=BTOW, 3=BSMDE, 4=BSMDP... Returns TVector tower. tower[0] is module, tower[1] is eta, tower[2] is sub, and tower[3] is id
	
//	StMuTrack* track = this;
	TArrayI tower(4);
	tower[0] = -10;
	tower[1] = -10;
	tower[2] = -10;		
	tower[3] = -10;		

	StThreeVectorD momentum,position;
	Double_t radius;
	
	StEmcGeom* mEmcGeom = StEmcGeom::instance("bemc");
	StEmcGeom* mSmdEGeom= StEmcGeom::instance("bsmde");
	StEmcGeom* mSmdPGeom= StEmcGeom::instance("bsmdp");
	
	if(det==1) radius = mEmcGeom->Radius();
	if(det==2) radius = mEmcGeom->Radius();
	if(det==3) radius = mSmdEGeom->Radius();
	if(det==4) radius = mSmdPGeom->Radius();
		
	StEventSummary& evtSummary = StMuDst::event()->eventSummary();
	Double_t mField = evtSummary.magneticField()/10;
		
	//add 30 cm to radius to find out if track left same tower
	if(useExitRadius) radius += 30.0;
	
	StEmcPosition mEmcPosition;
	Bool_t goodProjection;
	if(this) goodProjection = mEmcPosition.trackOnEmc(&position,&momentum,this,mField,radius);
	else return tower;
	if(goodProjection){
		Int_t m,e,s,id=0;
		Float_t eta=position.pseudoRapidity();
		Float_t phi=position.phi();
		if(det==1){
		  mEmcGeom->getBin(phi,eta,m,e,s);
//		  s = abs(s);
		  if(mEmcGeom->getId(m,e,s,id)==0){
		    tower[0] = m;
		    tower[1] = e;
			tower[2] = s;		
			tower[3] = id;		
		  }
		}
		else if(det==3){
		  Int_t check=mSmdEGeom->getBin(phi,eta,m,e,s);
		  if(!check){
		    s = abs(s);
		    if(mSmdEGeom->getId(m,e,s,id)==0){
		    tower[0] = m;
		    tower[1] = e;
			tower[2] = s;		    
			tower[3] = id;		}
		  }
		}
		else if(det==4){
		  Int_t check=mSmdPGeom->getBin(phi,eta,m,e,s);
		  s = abs(s);
		  if(!check){
		    if(mSmdPGeom->getId(m,e,s,id)==0){
		    tower[0] = m;
		    tower[1] = e;
			tower[2] = s;
			tower[3] = s;   }
		  }
		}
	}
	return tower;
}

double StMuTrack::energyBEMC() const { //Return energy of negative 100 GeV is no BEMC hit is matched...

	double hitEnergy;
	TArrayI tower = getTower();
	UInt_t iMod = tower[0];
    UInt_t iEta = tower[1];
	Int_t iSub = tower[2];
	if(iMod < 1 ||iMod > 120) return -100.0;

	if (StMuDst::emcCollection()) {
		StEmcDetector	*bemcDet = StMuDst::emcCollection()->detector(kBarrelEmcTowerId);
		StEmcModule	*mod = bemcDet->module(iMod);
		StSPtrVecEmcRawHit&  hits = mod->hits();
		for(UInt_t i=0; i<hits.size();i++){
			if(hits[i]){
				if((hits[i]->eta() == iEta) && (hits[i]->sub() == iSub)) {
					hitEnergy = hits[i]->energy();
					if(hitEnergy > 0) return hitEnergy;
					else return -100.0;
				}
			}
		}
	}
	return -100.0;
}

Bool_t StMuTrack::matchBEMC() const {
	double mEmcThres = 0.15;
	if (energyBEMC() > mEmcThres) return true;
	return false;
}
//________________________________________________________________________________
Double_t StMuTrack::dEdxPull(Double_t mass, Bool_t fit, Int_t charge) const {
  Double_t z = -999.;
  if (probPidTraits().dEdxTrackLength() > 0 ) {
    const StMuHelix &mh = muHelix();
    Double_t momentum  = mh.p().mag();
    Double_t log2dX = probPidTraits().log2dX();
    if (log2dX <= 0) log2dX = 1;
    Double_t dedx_measured, dedx_expected, dedx_resolution = -1;
    if (! fit) { // I70
      dedx_measured = probPidTraits().dEdxTruncated();
      dedx_expected = 1.e-6*charge*charge*Bichsel::Instance()->GetI70M(TMath::Log10(momentum*TMath::Abs(charge)/mass)); 
      dedx_resolution = probPidTraits().dEdxErrorTruncated();
    } else if ( fit == 1) {     // Ifit
      dedx_measured = probPidTraits().dEdxFit();
      dedx_expected = 1.e-6*charge*charge*TMath::Exp(Bichsel::Instance()->GetMostProbableZ(TMath::Log10(momentum*TMath::Abs(charge)/mass)));
      dedx_resolution = probPidTraits().dEdxErrorFit();
    } else {     // dNdx
      dedx_measured = probPidTraits().dNdxFit();
      dedx_expected = StdEdxModel::instance()->dNdx(momentum*TMath::Abs(charge)/mass,charge);
      dedx_resolution = probPidTraits().dNdxErrorFit();
    }
    if (dedx_resolution > 0)
      z = TMath::Log(dedx_measured/dedx_expected)/dedx_resolution;
  }
  return z;
}
//________________________________________________________________________________

ClassImp(StMuTrack)

/***************************************************************************
 *
 * $Log: StMuTrack.cxx,v $
 * Revision 1.55  2019/03/20 19:36:11  jdb
 * Update to StMuTrack for nHitsFit and nHitsPoss to handle the iTPC hits correctly
 *
 * Revision 1.54  2019/02/21 13:32:54  jdb
 * Inclusion of ETOF MuDst code. This code adds support for the full set of ETOF data which includes EtofDigi, EtofHit, EtofHeader. The code essentially copies similar structures from StEvent and additionally rebuilds the maps between Digis and Hits. Accessor methods are added based on the pattern from BTOF to provide access to data at various levels. The code for accessing the PID traits provided by ETOF is also provided
 *
 * Revision 1.53  2017/04/17 20:41:07  smirnovd
 * StMuTrack: Add additional info to Print()
 *
 * Revision 1.52  2017/04/17 19:19:43  smirnovd
 * [Cosmetic] Whitespace adjustments
 *
 * Revision 1.51  2016/09/18 23:00:49  fisyak
 * Add dNdx
 *
 * Revision 1.50  2014/01/15 22:00:04  fisyak
 * Add method to calculate dE/dx pulls for I70 and Ifit
 *
 * Revision 1.3  2014/01/15 21:11:08  fisyak
 * Add dE/dx pulls calculations
 *
 * Revision 1.2  2013/08/19 15:03:16  fisyak
 * Add calculation dE/dx pulls
 *
 * Revision 1.1.1.1  2013/07/23 14:14:48  fisyak
 *
 *
 * Revision 1.48  2012/05/07 14:47:06  fisyak
 * Add handles for track to fast detector matching
 *
 * Revision 1.47  2011/10/17 00:19:14  fisyak
 * Active handing of IdTruth
 *
 * Revision 1.46  2011/04/08 01:25:51  fisyak
 * Add branches for MC track and vertex information, add IdTruth to  tracks and vertices, reserve a possiblity to remove Strange MuDst
 *
 * Revision 1.45  2010/06/02 18:53:13  tone421
 * Fixed a bug. Previously, only events where a vertex was found had global tracks with pt and eta filled properly...
 *
 * Revision 1.44  2010/04/27 20:47:17  tone421
 * Added extra functions for BEMC matching. See this post for more details:
 *
 * http://www.star.bnl.gov/HyperNews-star/get/starsofi/7816.html
 *
 * Revision 1.43  2010/03/08 19:06:51  tone421
 * Two things. Global tracks how are filled with an index to primary at birth. Added StMuDst::fixTrackIndicesG(), which is used for matching the primary track indices to global tracks. Previously, this was quite slow -  see this post:
 *
 * http://www.star.bnl.gov/HyperNews-star/protected/get/starsoft/8092/1/1/1.html
 *
 * for more details.
 *
 * Revision 1.42  2009/12/01 21:56:35  tone421
 * Implemented changes as per http://www.star.bnl.gov/rt2/Ticket/Display.html?id=1734
 *
 * Revision 1.38  2009/02/20 16:37:44  tone421
 * *** empty log message ***
 *
 * Revision 1.36  2009/01/07 20:50:59  tone421
 * Added more pointer protection in dcaGlobal(int) and dca(int)
 *
 * Revision 1.35  2008/05/01 20:53:59  mvl
 * Changed/fixed handling of DCA for global tracks without StDcaGeometry: now calculate DCA based on helix (previous behaviour was to default to (0,0,0); only affected globals without primaries).
 * Changed globalDca() function to also work for globals that are attached to a different primary vertex. Used to return -999, now use helix to calculate.
 *
 * Revision 1.34  2008/03/19 14:51:04  fisyak
 * Add two clone arrays for global and primary track covariance matrices, remove mSigmaDcaD and mSigmaDcaZ
 *
 * Revision 1.33  2007/10/18 03:44:23  mvl
 * Added Ist and Pixel hits to mNPossInner and mNFitInner
 *
 * Revision 1.32  2007/09/18 02:29:58  mvl
 * Added basic printing functionality. For convenience and to assist data consistency checks
 *
 * Revision 1.31  2007/07/12 19:46:20  fisyak
 * Add includes for ROOT 5.16
 *
 * Revision 1.30  2007/04/20 06:23:46  mvl
 * Removed Q-vector flag (will implement utility class)
 *
 * Revision 1.28  2007/01/29 18:34:44  mvl
 * Updates to use StDcaGeometry for global DCA and momentum.
 * Added helper functions for radial and Z component: dcaD and dcaZ.
 * Uncertainties on those are stored in sigmaDcaD and sigmaDcaZ.
 * dcaD and dcaZ only work for the primary vertex to which the track belongs (avoid long extrapolations).
 *
 * Revision 1.27  2007/01/05 20:19:44  jeromel
 * helix(helix()) cannot be interpreted by gcc 3.4.6
 *
 * Revision 1.26  2006/07/27 18:55:41  fisyak
 * Remove DCA hack used in SSD+SVT test production (P06id)
 *
 * Revision 1.24  2005/08/19 19:46:06  mvl
 * Further updates for multiple vertices. The main changes are:
 * 1) StMudst::primaryTracks() now returns a list (TObjArray*) of tracks
 *    belonging to the 'current' primary vertex. The index number of the
 *    'current' vertex can be set using StMuDst::setCurrentVertex().
 *    This also affects StMuDst::primaryTracks(Int_t i) and
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
