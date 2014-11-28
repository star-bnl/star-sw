/***********************************************************************
 *
 * $Id: StHbtTTreeKink.h,v 1.2 2001/09/05 20:41:42 laue Exp $
 *
 * Author: Frank Laue, BNL;
 *
 ***********************************************************************
 *
 * Description: Kink class with information gotten from the StKinkVertex
 *              of Wenshen Deng and Spiros Margetis
 *
 ***********************************************************************/
#ifndef StHbtTTreeKink_h
#define StHbtTTreeKink_h


class StHbtKink;
class StHbtEvent;

#include "TObject.h"
#include "StHbtMaker/Infrastructure/StHbtTypes.hh"
#include "StHbtMaker/Infrastructure/StHbtTTreeTrack.h"

class StHbtTTreeKink : public TObject {
public:
  StHbtTTreeKink(){/* no-op */}
  StHbtTTreeKink(const StHbtEvent*, const StHbtKink*); // copy constructor
  virtual ~StHbtTTreeKink(){/* no-op */}

protected:
  float mDcaParentDaughter;           // from StKinkVertex class directly 
  float mDcaDaughterPrimaryVertex;    // from StKinkVertex class directly 
  float mDcaParentPrimaryVertex;      // from StKinkVertex class directly 
  float mHitDistanceParentDaughter;   // from StKinkVertex class directly 
  float mHitDistanceParentVertex;     // from StKinkVertex class directly 
  float mDeltaEnergy[3];              // from StKinkVertex class directly 
  float mDecayAngle;                  // from StKinkVertex class directly 
  float mDecayAngleCM;                // from StKinkVertex class directly 
  StHbtTTreeTrack  mDaughter;         // from StKinkVertex class directly 
  StHbtTTreeTrack  mParent;           // from StVertex class (which StKinkVertex inherits from)
  float mPositionX;                   // from StMeasuredPoint class (which StVertex inherits from)
  float mPositionY;                   // from StMeasuredPoint class (which StVertex inherits from)
  float mPositionZ;                   // from StMeasuredPoint class (which StVertex inherits from)


  friend class StHbtKink;

  ClassDef(StHbtTTreeKink,1)
};

#endif

/***********************************************************************
 * $Log: StHbtTTreeKink.h,v $
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

















