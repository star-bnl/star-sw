//////////////////////////////////////////////////////////////////////
//
// $Id: StPeCEvent.cxx,v 1.2 2000/04/21 19:10:30 nystrand Exp $
// $Log: StPeCEvent.cxx,v $
// Revision 1.2  2000/04/21 19:10:30  nystrand
// Include StPeCPair class
//
// Revision 1.1  2000/03/24 22:37:06  nystrand
// First version of StPeCEvent
//
// Revision 1.0  2000/03/20 23:28:50  nystrand
//
//////////////////////////////////////////////////////////////////////
#include <iostream.h>
#include "StPeCEvent.h"
#include "StEventTypes.h"

ClassImp(StPeCEvent)

StPeCEvent::StPeCEvent() {
#ifndef __CINT__
  pPrim = new StPeCPrimaryTrackCollection;
  pNonPrim = new StPeCNonPrimaryTrackCollection;
  pPair = new StPeCPairCollection;
#endif /*__CINT__*/
}

StPeCEvent::~StPeCEvent() {
#ifndef __CINT__
 delete pPrim;
 delete pNonPrim;
 delete pPair;
#endif /*__CINT__*/
}

Long_t  StPeCEvent::eventNumber() const{ return mEventNumber; }
Long_t  StPeCEvent::runNumber() const{ return mRunNumber; }
Int_t   StPeCEvent::globMultiplicity() const{ return mGlobMultiplicity; }
Int_t   StPeCEvent::primMultiplicity() const{ return mPrimMultiplicity; }
Int_t   StPeCEvent::qTot() const{ return mQTot; }
Float_t StPeCEvent::pT() const{ return mPT; }
Float_t StPeCEvent::zVertex() const{ return mZVertex; }
#ifndef __CINT__
void StPeCEvent::addPeCPrimaryTrack(StTrack* trk) const{
  pPrim->push_back(trk);
}
StPeCPrimaryTrackCollection* StPeCEvent::getPeCPrimaryTrackCollection() const{ return pPrim; }
void StPeCEvent::addPeCNonPrimaryTrack(StTrack* trk) const{
  pNonPrim->push_back(trk);
}
StPeCNonPrimaryTrackCollection* StPeCEvent::getPeCNonPrimaryTrackCollection() const{ return pNonPrim; }
void StPeCEvent::addPeCPair(StPeCPair* pair) const{
  pPair->push_back(pair);
}
StPeCPairCollection* StPeCEvent::getPeCPairCollection() const{ return pPair; }

StLorentzVectorF StPeCEvent::getEvent4Momentum(StPeCParticle pid) const{
  Float_t mptcle=0.0;
  if(pid==pion){
    mptcle = pion_plus_mass_c2;
  }
  if(pid==kaon){
    mptcle = 493.677*MeV;
  }
  if(pid==proton){
    mptcle = proton_mass_c2;
  }
  if(pid==electron){
    mptcle = electron_mass_c2;
  }
  if(pid==muon){
    mptcle = 105.6584*MeV; 
  }
  StLorentzVectorF p4event(0.0,0.0,0.0,0.0);
  StPeCPrimaryTrackIterator miter = pPrim->begin();
  while( miter != pPrim->end() ){
    StTrack *ttp = *miter;
    StThreeVectorF p = ttp->geometry()->momentum();
    Float_t energy = p.massHypothesis(mptcle);
    StLorentzVectorF pfour(energy,p);
    p4event = p4event + pfour;
    miter++;
  }

  return p4event;
}
#endif /*__CINT__*/

Float_t StPeCEvent::mInv(StPeCParticle pid) const{

  StLorentzVectorF p4event = getEvent4Momentum(pid);

  return p4event.m();
}

Float_t StPeCEvent::yRap(StPeCParticle pid) const{

  StLorentzVectorF p4event = getEvent4Momentum(pid);

  return p4event.rapidity();
}

void StPeCEvent::setEventNumber(Long_t &val) { mEventNumber=val; } 
void StPeCEvent::setRunNumber(Long_t &val) { mRunNumber=val; }
void StPeCEvent::setGlobMultiplicity(Int_t &val) { mGlobMultiplicity=val; }
void StPeCEvent::setPrimMultiplicity(Int_t &val) { mPrimMultiplicity=val; }
void StPeCEvent::setQTot(Int_t &val) { mQTot=val; }
void StPeCEvent::setPT(Float_t &val) { mPT=val; }
void StPeCEvent::setZVertex(Float_t &val) { mZVertex=val; }

