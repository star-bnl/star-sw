/***********************************************************************
 *
 * $Id: StKinkMc.cc,v 3.2 2002/04/30 16:02:47 genevb Exp $
 * $Log: StKinkMc.cc,v $
 * Revision 3.2  2002/04/30 16:02:47  genevb
 * Common muDst, improved MC code, better kinks, StrangeCuts now a branch
 *
 * Revision 3.1  2001/05/04 20:15:13  genevb
 * Common interfaces and reorganization of components, add MC event info
 *
 * Revision 3.0  2000/07/14 12:56:48  genevb
 * Revision 3 has event multiplicities and dedx information for vertex tracks
 *
 * Revision 2.0  2000/06/05 05:19:39  genevb
 * New version of Strangeness micro DST package
 *
 *
 ***********************************************************************
 *
 * Description: Monte Carlo Kink micro dst class
 *
 ***********************************************************************/
#include "StKinkMc.hh"
#include "StMcVertex.hh"
#include "StMcTrack.hh"
#include "StDecayMode.hh"                                                       

ClassImp(StKinkMc)

StKinkMc::StKinkMc() : StKinkBase()
{}

StKinkMc::StKinkMc(StMcVertex* mcVertex, StMcTrack* mcDaughterTrack) :
  StKinkBase()
{
  mDecayMode = StDecayMode::Instance()->Process(mcVertex);

  mParentGeantId = mcVertex->parent()->geantId(); 
  mDaughterGeantId = mcDaughterTrack->geantId();
  mParentMomentumX = 0.;
  mParentMomentumY = 0.;
  mParentMomentumZ = 0.;
  for (unsigned int i=0; i < mcVertex->numberOfDaughters(); i++) {
    mParentMomentumX += mcVertex->daughter(i)->momentum().x();
    mParentMomentumY += mcVertex->daughter(i)->momentum().y();
    mParentMomentumZ += mcVertex->daughter(i)->momentum().z();
  }
  mParentPrimMomentumX = mcVertex->parent()->momentum().x();
  mParentPrimMomentumY = mcVertex->parent()->momentum().y();
  mParentPrimMomentumZ = mcVertex->parent()->momentum().z();
  mDaughterMomentumX = mcDaughterTrack->momentum().x();
  mDaughterMomentumY = mcDaughterTrack->momentum().y();
  mDaughterMomentumZ = mcDaughterTrack->momentum().z();
  mPositionX = mcVertex->position().x();
  mPositionY = mcVertex->position().y();
  mPositionZ = mcVertex->position().z();
  mCommonTpcHits = 0;
  mSimTpcHits = mcDaughterTrack->tpcHits().size();
}

StKinkMc::~StKinkMc()
{}

Int_t StKinkMc::parentCharge() const {
  return (StDecayMode::Instance()->ParentCharge(mDecayMode));
}
