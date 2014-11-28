/***********************************************************************
 *
 * $Id: StHbtTTreeV0.h,v 1.0 1999/09/07
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
  mDecayVertexV0X = v->mDecayVertexV0.x();
  mDecayVertexV0Y = v->mDecayVertexV0.y();
  mDecayVertexV0Z = v->mDecayVertexV0.z();

  mDecayLengthV0 = v->mDecayLengthV0;
  mDcaV0Daughters = v->mDcaV0Daughters;
  mDcaV0ToPrimVertex = v->mDcaV0ToPrimVertex;
  mDcaPosToPrimVertex = v->mDcaPosToPrimVertex;
  mDcaNegToPrimVertex = v->mDcaNegToPrimVertex;

  mMomPosX = v->mMomPos.x();
  mMomPosY = v->mMomPos.y();
  mMomPosZ = v->mMomPos.z(); 
  mMomNegX = v->mMomNeg.x();
  mMomNegY = v->mMomNeg.y();
  mMomNegZ = v->mMomNeg.z();

  mTrackTopologyMapPos[0] = v->mTrackTopologyMapPos[0];
  mTrackTopologyMapPos[1] = v->mTrackTopologyMapPos[1];
  mTrackTopologyMapNeg[0] = v->mTrackTopologyMapNeg[0];
  mTrackTopologyMapNeg[1] = v->mTrackTopologyMapNeg[1];

  mKeyPos = v->mKeyPos;
  mKeyNeg = v->mKeyNeg;

  mTpcHitsPos = v->mTpcHitsPos;
  mTpcHitsNeg = v->mTpcHitsNeg;

  mChi2V0 = v->mChi2V0;
  mClV0    = v->mClV0;
  mChi2Pos = v->mChi2Pos;
  mClPos   = v->mClPos;
  mChi2Neg = v->mChi2Neg;
  mClNeg   = v->mClNeg;

  mDedxPos = v->mDedxPos;
  mErrDedxPos = v->errdedxPos(); //Gael 04Fev2002
  mLenDedxPos = v->lendedxPos(); //Gael 04Fev2002

  mDedxNeg = v->mDedxNeg;
  mErrDedxNeg = v->errdedxNeg(); //Gael 04Fev2002
  mLenDedxNeg = v->lendedxNeg(); //Gael 04Fev2002

  mNumDedxPos = v->mNumDedxPos;
  mNumDedxNeg = v->mNumDedxNeg;
}

ClassImp(StHbtTTreeV0)

/***********************************************************************
 *
 * $Log: StHbtTTreeV0.cxx,v $
 * Revision 1.3  2002/02/09 19:25:36  laue
 * updates (dedx length)
 *
 * Revision 1.2  2001/09/05 20:41:42  laue
 * Updates of the hbtMuDstTree microDSTs
 *
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
