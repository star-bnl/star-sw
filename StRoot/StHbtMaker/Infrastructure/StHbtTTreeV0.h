/***********************************************************************
 *
 *  StHbtTTreeV0.h,v 1.0 1999/09/07
 *
 * Authors: Frank Laue, BNL, laue@bnl.gov
 *
 ***********************************************************************/

#ifndef StHbtTTreeV0_h
#define StHbtTTreeV0_h

#include "TObject.h"
#include "StarClassLibrary/StThreeVectorF.hh"
#include "StEvent/StTrackTopologyMap.h"

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
  StThreeVectorF mDecayVertexV0;
  float mDcaV0Daughters;
  float mDcaV0ToPrimVertex;
  float mDcaPosToPrimVertex;
  float mDcaNegToPrimVertex;
  StThreeVectorF mMomPos;
  StThreeVectorF mMomNeg;
  unsigned short  mKeyPos;
  unsigned short  mKeyNeg;
  StTrackTopologyMap mTopologyMapPos;
  StTrackTopologyMap mTopologyMapNeg;
  float mChi2V0;
  float mClV0;
  float mChi2Pos;
  float mClPos;
  float mChi2Neg;
  float mClNeg;
  float mDedxPos;
  float mDedxNeg;
  unsigned short mNumDedxPos;
  unsigned short mNumDedxNeg;

  ClassDef(StHbtTTreeV0,1)
};

#endif

/***********************************************************************
 *
 * $Log: StHbtTTreeV0.h,v $
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
