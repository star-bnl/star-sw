/***********************************************************************
 *
 * $Id: StODMc.cc,v 2.0 2000/06/05 05:19:40 genevb Exp $
 * $Log: StODMc.cc,v $
 * Revision 2.0  2000/06/05 05:19:40  genevb
 * New version of Strangeness micro DST package
 *
 *
 ***********************************************************************
 *
 * Description: Monte Carlo "One Daughter" (OD) micro dst class
 *
 ***********************************************************************/
#include "StODMc.hh"
#include "StMcVertex.hh"
#include "StMcTrack.hh"
#include "StDecayMode.hh"

ClassImp(StODMc)

StODMc::StODMc()
{}
  
StODMc::
StODMc(StMcVertex* mcVertex, StMcTrack* mcDaughterTrack)
{
  mDecayMode = StDecayMode::Instance()->Process(mcVertex);

  mParentGeantId = mcVertex->parent()->geantId(); 
  mDaughterGeantId = mcDaughterTrack->geantId();
  mParentMomentumX = mcVertex->parent()->momentum().x();
  mParentMomentumY = mcVertex->parent()->momentum().y();
  mParentMomentumZ = mcVertex->parent()->momentum().z();
  mDaughterMomentumX = mcDaughterTrack->momentum().x();
  mDaughterMomentumY = mcDaughterTrack->momentum().y();
  mDaughterMomentumZ = mcDaughterTrack->momentum().z();
  mPositionX = mcVertex->position().x();
  mPositionY = mcVertex->position().y();
  mPositionZ = mcVertex->position().z();
  mCommonTpcHits = 0;
  mTpcHits = 0;
  mSimTpcHits = mcDaughterTrack->tpcHits().size();
}

StODMc::~StODMc() { }
 
