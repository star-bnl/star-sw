/***********************************************************************
 *
 * $Id: StKinkMuDst.cc,v 3.2 2000/08/10 01:16:24 genevb Exp $
 *
 * Author: Wensheng Deng, Kent State University, 29-Mar-2000
 *
 ***********************************************************************
 *
 * Description: Kink micro dst class
 *
 ***********************************************************************
 *
 * $Log: StKinkMuDst.cc,v $
 * Revision 3.2  2000/08/10 01:16:24  genevb
 * Added number of dedx points
 *
 * Revision 3.1  2000/08/09 18:56:18  wdeng
 * Get parent track lengths from primary tracks.
 *
 * Revision 3.0  2000/07/14 12:56:48  genevb
 * Revision 3 has event multiplicities and dedx information for vertex tracks
 *
 * Revision 2.0  2000/06/02 22:11:54  genevb
 * New version of Strangeness micro DST package
 *
 * Revision 1.1  2000/03/30 00:18:08  genevb
 * Introduction of StKinkMuDst
 *
 *
 ***********************************************************************/
#include "StKinkMuDst.hh"
#include "StKinkVertex.h"
#include "StTrack.h"
#include "StTrackNode.h"
#include "StTrackFitTraits.h"
#include "StDedxPidTraits.h"

#include <stdlib.h>
#include "phys_constants.h"

ClassImp(StKinkMuDst)

StKinkMuDst::StKinkMuDst()
{}

StKinkMuDst::StKinkMuDst(StKinkVertex* kinkVertex)
{
  mParentGeantId   = kinkVertex->geantIdParent();
  mDaughterGeantId = kinkVertex->geantIdDaughter();
  mDcaParentDaughter = kinkVertex->dcaParentDaughter();
  mDcaDaughterPrimaryVertex = kinkVertex->dcaDaughterPrimaryVertex();
  mDcaParentPrimaryVertex   = kinkVertex->dcaParentPrimaryVertex();
  mHitDistanceParentDaughter = kinkVertex->hitDistanceParentDaughter();
  mHitDistanceParentVertex = kinkVertex->hitDistanceParentVertex();
  mDecayAngle = kinkVertex->decayAngle();
  mParentMomentumX = kinkVertex->parentMomentum().x();
  mParentMomentumY = kinkVertex->parentMomentum().y();
  mParentMomentumZ = kinkVertex->parentMomentum().z();
  mDaughterMomentumX = kinkVertex->daughterMomentum().x();
  mDaughterMomentumY = kinkVertex->daughterMomentum().y();
  mDaughterMomentumZ = kinkVertex->daughterMomentum().z();
  mPositionX = kinkVertex->position().x();
  mPositionY = kinkVertex->position().y();
  mPositionZ = kinkVertex->position().z();
  mChi2Kink = kinkVertex->chiSquared();
  mClKink = kinkVertex->probChiSquared();
  
  StTrack* trk = kinkVertex->parent();
  mChi2Parent = trk->fitTraits().chi2(0);
  mClParent = trk->fitTraits().chi2(1);
  // For now, get the truncated mean dE/dX from the TPC
  StPtrVecTrackPidTraits pidParent = trk->pidTraits(kTpcId);
  for (UInt_t i=0; i<pidParent.size(); i++) {
    StDedxPidTraits* pid = (StDedxPidTraits*) pidParent[i];
    if (pid->method() == kTruncatedMeanId) {
      mDedxParent = pid->mean();
      mNumDedxParent = pid->numberOfPoints();
      break;
    }
  }

  trk = kinkVertex->daughter();
  mChi2Daughter = trk->fitTraits().chi2(0);
  mClDaughter = trk->fitTraits().chi2(1);
  // For now, get the truncated mean dE/dX from the TPC
  StPtrVecTrackPidTraits pidDaughter = trk->pidTraits(kTpcId);
  for (UInt_t i=0; i<pidDaughter.size(); i++) {
    StDedxPidTraits* pid = (StDedxPidTraits*) pidDaughter[i];
    if (pid->method() == kTruncatedMeanId) {
      mDedxDaughter = pid->mean();
      mNumDedxDaughter = pid->numberOfPoints();
      break;
    }
  }

  findMinDeltaEnergy(kinkVertex);
  findDecayLength(kinkVertex);
  // Pay attention to the calling order
  findTransverseMomentum();
  findTransverseMassKaon();
  findTransverseMassPion();
  findRapidityKaon();
  findRapidityPion();
}

StKinkMuDst::~StKinkMuDst() {/* noop */}

void
StKinkMuDst::findMinDeltaEnergy(StKinkVertex* kinkVertex) 
{
  mMinDeltaEnergy = kinkVertex->dE(0);
  if( mMinDeltaEnergy > kinkVertex->dE(1) ) 
    mMinDeltaEnergy = kinkVertex->dE(1);
  if( mMinDeltaEnergy > kinkVertex->dE(2) )
    mMinDeltaEnergy = kinkVertex->dE(2);
}

void
StKinkMuDst::findDecayLength(StKinkVertex* kinkVertex)
{
  StTrack* parentPrimaryTrack = 
    kinkVertex->parent()->node()->track(primary);
  if( parentPrimaryTrack ) {
    mDecayLength = parentPrimaryTrack->length();
  } else {
    mDecayLength = 999.;
  }
}

void  
StKinkMuDst::findTransverseMomentum()
{
  mTransverseMomentum = 
    sqrt( mParentMomentumX * mParentMomentumX
	+ mParentMomentumY * mParentMomentumY );
}

void
StKinkMuDst::findTransverseMassKaon()
{
  mTransverseMassKaon = 
    sqrt(  M_KAON_PLUS * M_KAON_PLUS
	 + mTransverseMomentum * mTransverseMomentum );
}

void
StKinkMuDst::findTransverseMassPion()
{
  mTransverseMassPion = 
    sqrt(  M_PION_PLUS * M_PION_PLUS
	 + mTransverseMomentum * mTransverseMomentum );
}

void
StKinkMuDst::findRapidityKaon()
{
  Float_t mTotalEnergy = 
    sqrt( M_KAON_PLUS * M_KAON_PLUS
	+ mParentMomentumX * mParentMomentumX
	+ mParentMomentumY * mParentMomentumY
	+ mParentMomentumZ * mParentMomentumZ );
 
  mRapidityKaon = 
    log( (mTotalEnergy + mParentMomentumZ)/mTransverseMassKaon );
}

void
StKinkMuDst::findRapidityPion()
{
  Float_t mTotalEnergy = 
    sqrt( M_PION_PLUS * M_PION_PLUS
	+ mParentMomentumX * mParentMomentumX
	+ mParentMomentumY * mParentMomentumY
	+ mParentMomentumZ * mParentMomentumZ );
 
  mRapidityPion = 
    log( (mTotalEnergy + mParentMomentumZ)/mTransverseMassPion );
}

