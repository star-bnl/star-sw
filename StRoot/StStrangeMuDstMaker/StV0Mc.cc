/***********************************************************************
 *
 * $Id: StV0Mc.cc,v 2.0 2000/06/05 05:19:45 genevb Exp $
 * $Log: StV0Mc.cc,v $
 * Revision 2.0  2000/06/05 05:19:45  genevb
 * New version of Strangeness micro DST package
 *
 *
 ***********************************************************************
 *
 * Description: Monte Carlo V0 micro dst class
 *
 ***********************************************************************/
#include "StV0Mc.hh"
#include "StMcVertex.hh"
#include "StMcTrack.hh"
#include "StDecayMode.hh"

ClassImp(StV0Mc)

StV0Mc::StV0Mc() {
}
  
StV0Mc::StV0Mc(StMcVertex* mcVertex, StMcTrack* mcPositiveTrack, 
	       StMcTrack* mcNegativeTrack) {
  
  mDecayMode = StDecayMode::Instance()->Process(mcVertex);
  
  mParentGeantId = mcVertex->parent()->geantId(); 
  mPositiveGeantId = mcPositiveTrack->geantId();
  mNegativeGeantId = mcNegativeTrack->geantId();
  
  mParentMomentumX = mcVertex->parent()->momentum().x();
  mParentMomentumY = mcVertex->parent()->momentum().y();
  mParentMomentumZ = mcVertex->parent()->momentum().z();
  
  mPositiveMomentumX = mcPositiveTrack->momentum().x();
  mPositiveMomentumY = mcPositiveTrack->momentum().y();
  mPositiveMomentumZ = mcPositiveTrack->momentum().z();
  mPositiveCommonTpcHits = 0;
  mPositiveTpcHits = 0;
  mPositiveSimTpcHits = mcPositiveTrack->tpcHits().size();
  
  mNegativeMomentumX = mcNegativeTrack->momentum().x();
  mNegativeMomentumY = mcNegativeTrack->momentum().y();
  mNegativeMomentumZ = mcNegativeTrack->momentum().z();
  mNegativeCommonTpcHits = 0;
  mNegativeTpcHits = 0;
  mNegativeSimTpcHits = mcNegativeTrack->tpcHits().size();
  
  mPositionX = mcVertex->position().x();
  mPositionY = mcVertex->position().y();
  mPositionZ = mcVertex->position().z();
}

StV0Mc::~StV0Mc() {
}
