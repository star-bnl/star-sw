/***********************************************************************
 *
 *  StHbtTTreeV0.h,v 1.0 1999/09/07
 *
 * Authors: Frank Laue, BNL, laue@bnl.gov
 *
 ***********************************************************************/

#include "StHbtTTreeV0.h"
#include "StHbtEvent.hh"
#include "StHbtV0.hh"


//----------------------------------------------------------------------------
//----------------------------------------------------------------------------
//----------------------------------------------------------------------------
StHbtTTreeV0::StHbtTTreeV0(const StHbtEvent* event, const StHbtV0* v){ // copy constructor
//   mDecayVertexV0 = StThreeVectorF(v->mDecayVertexV0X,v->mDecayVertexV0Y,v->mDecayVertexV0Z);
//   mDcaV0Daughters = v->mDcaV0Daughters;
//   mDcaV0ToPrimVertex = v->mDcaV0ToPrimVertex;
//   mDcaPosToPrimVertex = v->mDcaPosToPrimVertex;
//   mDcaNegToPrimVertex = v->mDcaNegToPrimVertex;
//   mMomPos = StThreeVectorF(v->mMomPosX,v->mMomPosY,v->mMomPosZ);
//   mMomNeg = StThreeVectorF(v->mMomNegX,v->mMomNegY,v->mMomNegZ);
//   mTrackTopologyMapPos = v->mTrackTopologyMapPos;
//   mTrackTopologyMapNeg = v->mTrackTopologyMapNeg;
//   mKeyPos = v->mKeyPos;
//   mKeyNeg = v->mKeyNeg;
//   mChi2V0 = v->mChi2V0;
//   mClV0 = v->mClV0;
//   mChi2Pos = v->mChi2Pos;
//   mClPos = v->mClPos;
//   mChi2Neg = v->mChi2Neg;
//   mClNeg = v->mClNeg;
//   mDedxPos = v->mDedxPos;
//   mDedxNeg = v->mDedxNeg;
//   mNumDedxPos = v->mNumDedxPos;
//   mNumDedxNeg = v->mNumDedxNeg;
}

ClassImp(StHbtTTreeV0)

/***********************************************************************
 *
 * $Log: StHbtTTreeV0.cxx,v $
 * Revision 1.1  2001/06/21 19:15:47  laue
 * Modified fiels:
 *   CTH.hh : new constructor added
 *   StHbtEvent, StHbtKink, StHbtTrack : constructors from the persistent
 *                                   (TTree) classes added
 *   StHbtLikeSignAnalysis : minor changes, for debugging
 *   StHbtTypes: split into different files
 * Added files: for the new TTree muDst's
 *   StExceptions.cxx StExceptions.hh StHbtEnumeration.hh
 *   StHbtHelix.hh StHbtHisto.hh StHbtString.hh StHbtTFile.hh
 *   StHbtTTreeEvent.cxx StHbtTTreeEvent.h StHbtTTreeKink.cxx
 *   StHbtTTreeKink.h StHbtTTreeTrack.cxx StHbtTTreeTrack.h
 *   StHbtTTreeV0.cxx StHbtTTreeV0.h StHbtVector.hh
 *
 *
 ***********************************************************************/
