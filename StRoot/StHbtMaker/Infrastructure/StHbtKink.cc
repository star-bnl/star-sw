/***********************************************************************
 *
 * $Id: StHbtKink.cc,v 1.4 2001/11/14 21:07:21 lisa Exp $
 *
 * Author: Mike Lisa, Ohio State, 23May2001
 *
 ***********************************************************************
 *
 * Description: Kink class with information gotten from the StKinkVertex
 *              of Wenshen Deng and Spiros Margetis
 *
 ***********************************************************************
 *
 * $Log: StHbtKink.cc,v $
 * Revision 1.4  2001/11/14 21:07:21  lisa
 * Fixed several small things (mostly discarded const) that caused fatal errors with gcc2.95.3
 *
 * Revision 1.3  2001/09/05 21:55:23  laue
 * typo fixed
 *
 * Revision 1.2  2001/06/21 19:15:46  laue
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
 * Revision 1.1  2001/05/25 23:23:59  lisa
 * Added in StHbtKink stuff
 *
 * 
 *
 ***********************************************************************/

#include "StHbtKink.hh"
#include "phys_constants.h"
#include "StHbtMaker/Infrastructure/StHbtTrack.hh"
// -----------------------------------------------------------------------
StHbtKink::StHbtKink(const StHbtKink& k){ // copy constructor

  mDcaParentDaughter          =   k.mDcaParentDaughter;           
  mDcaDaughterPrimaryVertex   =   k.mDcaDaughterPrimaryVertex;    
  mDcaParentPrimaryVertex     =   k.mDcaParentPrimaryVertex;      
  mHitDistanceParentDaughter  =   k.mHitDistanceParentDaughter;   
  mHitDistanceParentVertex    =   k.mHitDistanceParentVertex;     
  mDeltaEnergy[0]             =   k.mDeltaEnergy[0];              
  mDeltaEnergy[1]             =   k.mDeltaEnergy[1];              
  mDeltaEnergy[2]             =   k.mDeltaEnergy[2];              
  mDecayAngle                 =   k.mDecayAngle;                  
  mDecayAngleCM               =   k.mDecayAngleCM;                
  mDaughter                   =   k.mDaughter;                    
  mParent                     =   k.mParent;                      
  mPosition                   =   k.mPosition;                

}
// -----------------------------------------------------------------------
#ifdef __ROOT__
#include "StEvent/StTrack.h"
#include "StEvent/StKinkVertex.h"
StHbtKink::StHbtKink( const StKinkVertex& SKV, StHbtThreeVector PrimaryVertex )
{ 

  mDcaParentDaughter          = SKV.dcaParentDaughter();
  mDcaDaughterPrimaryVertex   = SKV.dcaDaughterPrimaryVertex();
  mDcaParentPrimaryVertex     = SKV.dcaParentPrimaryVertex();
  mHitDistanceParentDaughter  = SKV.hitDistanceParentDaughter();
  mHitDistanceParentVertex    = SKV.hitDistanceParentVertex();
  mDeltaEnergy[0]             = SKV.dE(0);
  mDeltaEnergy[1]             = SKV.dE(1);
  mDeltaEnergy[2]             = SKV.dE(2);
  mDecayAngle                 = SKV.decayAngle();
  mDecayAngleCM               = SKV.decayAngleCM();

  // now fill member StHbtTrack data...
  const StTrack* StTrk;
  StHbtTrack* HbtTrk;
  // Daughter
  StTrk = SKV.daughter(0);
  HbtTrk = new StHbtTrack(StTrk,PrimaryVertex); // generate NEW HbtTrack from StTrack
  mDaughter = *HbtTrk;                         // invoke copy ctr of StHbtTrack
  delete HbtTrk;                               // get rid of the NEW HbtTrack - we are done with that
  // Parent
  StTrk = SKV.parent();
  HbtTrk = new StHbtTrack(StTrk,PrimaryVertex); // generate NEW HbtTrack from StTrack
  mParent = *HbtTrk;                           // invoke copy ctr of StHbtTrack
  delete HbtTrk;                               // get rid of the NEW HbtTrack - we are done with that

  // finally, the kink position
  mPosition.setX(SKV.position().x());
  mPosition.setY(SKV.position().y());
  mPosition.setZ(SKV.position().z());

}

#include "StHbtMaker/Infrastructure/StHbtTTreeEvent.h"
#include "StHbtMaker/Infrastructure/StHbtTTreeKink.h"
#include "StHbtMaker/Infrastructure/StHbtTTreeTrack.h"
StHbtKink::StHbtKink( const StHbtTTreeEvent* e, const StHbtTTreeKink* k) {
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
  StHbtTrack mDaughter = StHbtTrack(e, &k->mDaughter);      
  StHbtTrack mParent= StHbtTrack(e, &k->mParent);        
  StHbtThreeVector mPosition = StHbtThreeVector(k->mPositionX,k->mPositionY,k->mPositionZ);  
}


#endif // __ROOT__

