/***********************************************************************
 *
 * $Id: StHbtTTreeV0.h,v 1.0 1999/09/07
 *
 * Authors: Frank Laue, BNL, laue@bnl.gov
 *
 ***********************************************************************/

#ifndef StHbtTTreeV0_h
#define StHbtTTreeV0_h

#include "TObject.h"
#include "StHbtMaker/Infrastructure/StHbtTypes.hh"
#include "StarClassLibrary/StThreeVectorF.hh"

class StHbtEvent;
class StHbtV0;

//----------------------------------------------------------------------------
//----------------------------------------------------------------------------
//----------------------------------------------------------------------------
class StHbtTTreeV0 : public TObject  {
public:
  StHbtTTreeV0(){/* no-op */}
  StHbtTTreeV0(const StHbtEvent*, const StHbtV0* ); // copy constructor
  virtual ~StHbtTTreeV0(){/* no-op */}

  friend class StHbtTTreeReader;
  friend class StHbtV0;
protected:
  float mDecayLengthV0;
  float mDecayVertexV0X;
  float mDecayVertexV0Y;
  float mDecayVertexV0Z;
  float mDcaV0Daughters;
  float mDcaV0ToPrimVertex;
  float mDcaPosToPrimVertex;
  float mDcaNegToPrimVertex;
  float mMomPosX;
  float mMomPosY;
  float mMomPosZ;
  float mMomNegX;
  float mMomNegY;
  float mMomNegZ;
  unsigned short  mKeyPos;
  unsigned short  mKeyNeg;
  unsigned int mTrackTopologyMapPos[2];
  unsigned int mTrackTopologyMapNeg[2];
  float mChi2V0;
  float mClV0;
  float mChi2Pos;
  float mClPos;
  float mChi2Neg;
  float mClNeg;
  float mDedxPos;
  float mErrDedxPos;//Gael 04Fev2002
  float mLenDedxPos;//Gael 04Fev2002
  float mDedxNeg;
  float mErrDedxNeg;//Gael 04Fev2002
  float mLenDedxNeg;//Gael 04Fev2002

  unsigned short mNumDedxPos;
  unsigned short mNumDedxNeg;
  unsigned short mTpcHitsPos;
  unsigned short mTpcHitsNeg;
  ClassDef(StHbtTTreeV0,3)
};

#endif

/***********************************************************************
 *
 * $Log: StHbtTTreeV0.h,v $
 * Revision 1.3  2002/02/09 19:25:36  laue
 * updates (dedx length)
 *
 * Revision 1.2  2001/09/05 20:41:43  laue
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
