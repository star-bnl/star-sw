/***********************************************************************
 *
 * $Id: StHbtTTreeKink.cxx,v 1.2 2001/09/05 20:41:42 laue Exp $
 *
 * Author: Frank Laue, BNL;
 *
 ***********************************************************************
 *
 * Description: Kink class with information gotten from the StKinkVertex
 *              of Wenshen Deng and Spiros Margetis
 *
 ***********************************************************************/
#include "StHbtMaker/Infrastructure/StHbtEvent.hh"
#include "StHbtMaker/Infrastructure/StHbtKink.hh"
#include "StHbtMaker/Infrastructure/StHbtTTreeTrack.h"

#include "StHbtMaker/Infrastructure/StHbtTTreeKink.h"

#ifdef __ROOT__
ClassImp(StHbtTTreeKink)


StHbtTTreeKink::StHbtTTreeKink(const StHbtEvent* e, const StHbtKink* k) {
  mDcaParentDaughter = k->mDcaParentDaughter;  
  mDcaDaughterPrimaryVertex = k->mDcaDaughterPrimaryVertex;
  mDcaParentPrimaryVertex = k->mDcaParentPrimaryVertex;
  mHitDistanceParentDaughter = k->mHitDistanceParentDaughter;
  mHitDistanceParentVertex = k->mHitDistanceParentVertex;  
  mDeltaEnergy[0] = k->mDeltaEnergy[0];           
  mDeltaEnergy[1] = k->mDeltaEnergy[1];           
  mDeltaEnergy[2] = k->mDeltaEnergy[2];           
  mDecayAngle = k->mDecayAngle;               
  mDecayAngleCM = k->mDecayAngleCM;           
  mDaughter = StHbtTTreeTrack(e, &(k->mDaughter));
  mParent = StHbtTTreeTrack(e, &(k->mParent));
  mPositionX = k->mPosition.x();
  mPositionY = k->mPosition.y();
  mPositionZ = k->mPosition.z();
};

#endif

/***********************************************************************
 * $Log: StHbtTTreeKink.cxx,v $
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

















