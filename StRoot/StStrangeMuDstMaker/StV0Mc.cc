/***********************************************************************
 *
 * $Id: StV0Mc.cc,v 3.1 2001/05/04 20:15:14 genevb Exp $
 * $Log: StV0Mc.cc,v $
 * Revision 3.1  2001/05/04 20:15:14  genevb
 * Common interfaces and reorganization of components, add MC event info
 *
 * Revision 3.0  2000/07/14 12:56:50  genevb
 * Revision 3 has event multiplicities and dedx information for vertex tracks
 *
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

StV0Mc::StV0Mc() : StV0I() {}
  
StV0Mc::StV0Mc(StMcVertex* mcVertex, StMcTrack* mcPositiveTrack, 
	     StMcTrack* mcNegativeTrack, StStrangeEvMuDst* mcEvent) : StV0I() {
  
  mEvent = mcEvent;

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
  mPositiveSimTpcHits = mcPositiveTrack->tpcHits().size();
  
  mNegativeMomentumX = mcNegativeTrack->momentum().x();
  mNegativeMomentumY = mcNegativeTrack->momentum().y();
  mNegativeMomentumZ = mcNegativeTrack->momentum().z();
  mNegativeCommonTpcHits = 0;
  mNegativeSimTpcHits = mcNegativeTrack->tpcHits().size();
  
  mPositionX = mcVertex->position().x();
  mPositionY = mcVertex->position().y();
  mPositionZ = mcVertex->position().z();
}

StV0Mc::~StV0Mc() {
}
