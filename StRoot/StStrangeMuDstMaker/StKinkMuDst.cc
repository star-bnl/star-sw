/***********************************************************************
 *
 * $Id: StKinkMuDst.cc,v 1.1 2000/03/30 00:18:08 genevb Exp $
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
 * Revision 1.1  2000/03/30 00:18:08  genevb
 * Introduction of StKinkMuDst
 *
 *
 ***********************************************************************/
#include "StKinkMuDst.hh"
#include "StKinkVertex.h"
#include "StTrack.h"

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

UShort_t StKinkMuDst::geantIdParent() const 
{ 
  return mParentGeantId; 
}

UShort_t StKinkMuDst::geantIdDaughter() const 
{ 
  return mDaughterGeantId; 
}

Float_t StKinkMuDst::dcaParentDaughter() const 
{
  return mDcaParentDaughter;
}

Float_t StKinkMuDst::dcaDaughterPrimaryVertex() const
{
  return mDcaDaughterPrimaryVertex;
}
   
Float_t StKinkMuDst::dcaParentPrimaryVertex() const 
{
  return mDcaParentPrimaryVertex; 
}

Float_t StKinkMuDst::hitDistanceParentDaughter() const
{
  return mHitDistanceParentDaughter; 
}

Float_t StKinkMuDst::hitDistanceParentVertex() const 
{
  return mHitDistanceParentVertex; 
}

Float_t StKinkMuDst::mindE() const
{ 
  return mMinDeltaEnergy; 
}

Float_t StKinkMuDst::decayAngle() const
{
  return mDecayAngle; 
}

Float_t StKinkMuDst::parentMomentumX() const
{
  return mParentMomentumX; 
}

Float_t StKinkMuDst::parentMomentumY() const
{
  return mParentMomentumY; 
}

Float_t StKinkMuDst::parentMomentumZ() const 
{
  return mParentMomentumZ; 
}

Float_t StKinkMuDst::daughterMomentumX() const
{ 
  return mDaughterMomentumX; 
}

Float_t StKinkMuDst::daughterMomentumY() const 
{
  return mDaughterMomentumY; 
}

Float_t StKinkMuDst::daughterMomentumZ() const
{
  return mDaughterMomentumZ; 
}

Float_t StKinkMuDst::positionX() const
{
  return mPositionX; 
}

Float_t StKinkMuDst::positionY() const
{
  return mPositionY; 
}

Float_t StKinkMuDst::positionZ() const 
{
  return mPositionZ;
}

Float_t StKinkMuDst::decayLength() const
{
  return mDecayLength;
}
  
Float_t StKinkMuDst::transverseMomentum() const
{
  return mTransverseMomentum;
}

Float_t StKinkMuDst::transverseMassKaon() const
{
  return mTransverseMassKaon;
}

Float_t StKinkMuDst::transverseMassPion() const
{
  return mTransverseMassPion;
}

Float_t StKinkMuDst::rapidityKaon() const
{
  return mRapidityKaon;
}

Float_t StKinkMuDst::rapidityPion() const
{
  return mRapidityPion;
}

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

