/***********************************************************************
 *
 * $Id: StHbtKink.cc,v 1.1 2001/05/25 23:23:59 lisa Exp $
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
  StTrack* StTrk;
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
#endif // __ROOT__

