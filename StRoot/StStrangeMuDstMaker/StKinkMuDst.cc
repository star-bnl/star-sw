/***********************************************************************
 *
 * $Id: StKinkMuDst.cc,v 2.0 2000/06/02 22:11:54 genevb Exp $
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
#include "StTrackFitTraits.h"

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
  mChi2Parent = kinkVertex->parent()->fitTraits().chi2(0);
  mClParent = kinkVertex->parent()->fitTraits().chi2(1);
  mChi2Daughter = kinkVertex->daughter()->fitTraits().chi2(0);
  mClDaughter = kinkVertex->daughter()->fitTraits().chi2(1);

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
  mDecayLength = kinkVertex->parent()->length();
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

