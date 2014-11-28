/***********************************************************************
 *
 * $Id: StKinkMuDst.cc,v 3.12 2008/07/10 16:16:55 genevb Exp $
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
 * Revision 3.12  2008/07/10 16:16:55  genevb
 * Allow for marking of bad tracks -> bad secondary vertices
 *
 * Revision 3.11  2004/02/12 20:51:41  genevb
 * Better error messages
 *
 * Revision 3.10  2004/02/03 03:49:27  genevb
 * Added keys (IDs) for Kink parent and daughter
 *
 * Revision 3.9  2003/09/02 17:59:04  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 3.8  2002/04/30 16:02:47  genevb
 * Common muDst, improved MC code, better kinks, StrangeCuts now a branch
 *
 * Revision 3.7  2001/11/05 23:41:06  genevb
 * Add more dEdx, B field info, careful of changes to TTree unrolling
 *
 * Revision 3.6  2001/05/04 20:15:14  genevb
 * Common interfaces and reorganization of components, add MC event info
 *
 * Revision 3.5  2001/04/25 18:20:17  perev
 * HPcorrs
 *
 * Revision 3.4  2001/02/14 19:37:44  wdeng
 * Get parent momentum from primary track
 *
 * Revision 3.3  2000/09/06 21:09:03  wdeng
 * Added track charges and total momenta
 *
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
#include "StTrackGeometry.h"
#include "StTrackNode.h"
#include "StTrackFitTraits.h"
#include "StDedxPidTraits.h"
#include "StMessMgr.h"

#include <stdlib.h>
#include "phys_constants.h"

ClassImp(StKinkBase)
ClassImp(StKinkMuDst)

StKinkMuDst::StKinkMuDst() : StKinkBase()
{}

StKinkMuDst::StKinkMuDst(StKinkVertex* kinkVertex) : StKinkBase()
{
  mParentGeantId   = kinkVertex->geantIdParent();
  mDaughterGeantId = kinkVertex->geantIdDaughter();
  mDcaParentDaughter = kinkVertex->dcaParentDaughter();
  mDcaDaughterPrimaryVertex = kinkVertex->dcaDaughterPrimaryVertex();
  mDcaParentPrimaryVertex   = kinkVertex->dcaParentPrimaryVertex();
  mHitDistanceParentDaughter = kinkVertex->hitDistanceParentDaughter();
  mHitDistanceParentVertex = kinkVertex->hitDistanceParentVertex();
  mDecayAngle = kinkVertex->decayAngle();
  StTrack* parent = kinkVertex->parent();
  if (!parent) gMessMgr->Error("StKinkMuDst: parent missing!");
  StTrack* daughter = kinkVertex->daughter();
  if (!daughter) gMessMgr->Error("StKinkMuDst: daughter missing!");

  StTrack* parentPrimaryTrack = parent->node()->track(primary);
  if (parentPrimaryTrack && (parentPrimaryTrack->geometry())) {
    mParentPrimMomentumX = parentPrimaryTrack->geometry()->momentum().x();
    mParentPrimMomentumY = parentPrimaryTrack->geometry()->momentum().y();
    mParentPrimMomentumZ = parentPrimaryTrack->geometry()->momentum().z();
  } else {
    mParentPrimMomentumX = 999.;
    mParentPrimMomentumY = 999.;
    mParentPrimMomentumZ = 999.;
  }
  mParentPrimMomentum = ::sqrt( mParentPrimMomentumX*mParentPrimMomentumX +
                          mParentPrimMomentumY*mParentPrimMomentumY +
                          mParentPrimMomentumZ*mParentPrimMomentumZ );

  const StThreeVectorF parentMom = kinkVertex->parentMomentum();
  mParentMomentumX = parentMom.x();
  mParentMomentumY = parentMom.y();
  mParentMomentumZ = parentMom.z();
  mParentMomentum  = parentMom.mag();
  if (parent->geometry()) {
    mParentCharge = parent->geometry()->charge();
  } else {
    mParentCharge = 0;
    gMessMgr->Warning("StKinkMuDst: parent geometry missing!");
  }
  mKeyParent = parent->key();

  const StThreeVectorF daughterMom = kinkVertex->daughterMomentum();
  mDaughterMomentumX = daughterMom.x();
  mDaughterMomentumY = daughterMom.y();
  mDaughterMomentumZ = daughterMom.z();
  mDaughterMomentum  = daughterMom.mag();
  if (daughter->geometry()) {
    mDaughterCharge = daughter->geometry()->charge();
  } else {
    mDaughterCharge = 0;
    gMessMgr->Warning("StKinkMuDst: daughter geometry missing!");
  }
  mKeyDaughter = daughter->key();

  const StThreeVectorF pos = kinkVertex->position();
  mPositionX = pos.x();
  mPositionY = pos.y();
  mPositionZ = pos.z();
  mChi2Kink = kinkVertex->chiSquared();
  mClKink = kinkVertex->probChiSquared();
  
  mChi2Parent = parent->fitTraits().chi2(0);
  mClParent = parent->fitTraits().chi2(1);
  if (parent->bad()) setParentBad();
  mDedxParent = 0.;
  mNumDedxParent = 0;
  // For now, get the truncated mean dE/dX from the TPC
  StPtrVecTrackPidTraits pidParent = parent->pidTraits(kTpcId);
  UInt_t i;
  for (i=0; i<pidParent.size(); i++) {
    StDedxPidTraits* pid = (StDedxPidTraits*) pidParent[i];
    if (pid->method() == kTruncatedMeanId) {
      mDedxParent = pid->mean();
      mErrDedxParent = pid->errorOnMean();
      mNumDedxParent = pid->numberOfPoints() + (100*((int) (pid->length())));
      break;
    }
  }

  mChi2Daughter = daughter->fitTraits().chi2(0);
  mClDaughter = daughter->fitTraits().chi2(1);
  if (daughter->bad()) setDaughterBad();
  mDedxDaughter = 0.;
  mNumDedxDaughter = 0;
  // For now, get the truncated mean dE/dX from the TPC
  StPtrVecTrackPidTraits pidDaughter = daughter->pidTraits(kTpcId);
  for (i=0; i<pidDaughter.size(); i++) {
    StDedxPidTraits* pid = (StDedxPidTraits*) pidDaughter[i];
    if (pid->method() == kTruncatedMeanId) {
      mDedxDaughter = pid->mean();
      mErrDedxDaughter = pid->errorOnMean();
      mNumDedxDaughter = pid->numberOfPoints() + (100*((int) (pid->length())));
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
    ::sqrt( mParentMomentumX * mParentMomentumX
	+ mParentMomentumY * mParentMomentumY );
}

void
StKinkMuDst::findTransverseMassKaon()
{
  mTransverseMassKaon = 
    ::sqrt(  M_KAON_PLUS * M_KAON_PLUS
	 + mTransverseMomentum * mTransverseMomentum );
}

void
StKinkMuDst::findTransverseMassPion()
{
  mTransverseMassPion = 
    ::sqrt(  M_PION_PLUS * M_PION_PLUS
	 + mTransverseMomentum * mTransverseMomentum );
}

void
StKinkMuDst::findRapidityKaon()
{
  Float_t mTotalEnergy = 
    ::sqrt( M_KAON_PLUS * M_KAON_PLUS
	+ mParentMomentumX * mParentMomentumX
	+ mParentMomentumY * mParentMomentumY
	+ mParentMomentumZ * mParentMomentumZ );
 
  mRapidityKaon = 
    ::log( (mTotalEnergy + mParentMomentumZ)/mTransverseMassKaon );
}

void
StKinkMuDst::findRapidityPion()
{
  Float_t mTotalEnergy = 
    ::sqrt( M_PION_PLUS * M_PION_PLUS
	+ mParentMomentumX * mParentMomentumX
	+ mParentMomentumY * mParentMomentumY
	+ mParentMomentumZ * mParentMomentumZ );
 
  mRapidityPion = 
    ::log( (mTotalEnergy + mParentMomentumZ)/mTransverseMassPion );
}

