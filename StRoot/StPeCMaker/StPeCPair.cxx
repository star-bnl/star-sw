//////////////////////////////////////////////////////////////////////
//
// $Id: StPeCPair.cxx,v 1.1 2000/04/21 19:12:30 nystrand Exp $
// $Log: StPeCPair.cxx,v $
// Revision 1.1  2000/04/21 19:12:30  nystrand
// First Version
//
// Revision 1.1  2000/03/24 22:37:06  nystrand
// First version of StPeCPair
//
// Revision 1.0  2000/03/20 23:28:50  nystrand
//
//////////////////////////////////////////////////////////////////////
#include <iostream.h>
#include "StPeCPair.h"
#include "StEventTypes.h"

ClassImp(StPeCPair)

StPeCPair::StPeCPair() {
}

StPeCPair::~StPeCPair() {
}

#ifndef __CINT__
StPeCPair::StPeCPair(StTrack* trk1, StTrack* trk2) {
  Track1 = trk1;
  Track2 = trk2;
}
void StPeCPair::setTrack1(StTrack* trk) {
  Track1 = trk;
}
void StPeCPair::setTrack2(StTrack* trk) {
  Track2 = trk;
}
StTrack* StPeCPair::getTrack1() { return Track1; }
StTrack* StPeCPair::getTrack2() { return Track2; }

StLorentzVectorF StPeCPair::getPair4Momentum(StPeCParticle pid) const{
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
  StLorentzVectorF p4pair(0.0,0.0,0.0,0.0);
  StThreeVectorF   p1 = Track1->geometry()->momentum();
  StThreeVectorF   p2 = Track2->geometry()->momentum();
  Float_t          e1 = p1.massHypothesis(mptcle);
  Float_t          e2 = p2.massHypothesis(mptcle);
  StLorentzVectorF pf1(e1,p1);
  StLorentzVectorF pf2(e2,p2);
  p4pair = pf1 + pf2;
  return p4pair;
}
#endif /*__CINT__*/

Float_t StPeCPair::mInv(StPeCParticle pid) const{
  StLorentzVectorF p4event = getPair4Momentum(pid);
  return p4event.m();
}

Float_t StPeCPair::openingAngle() const{
  // Opening angle of the pair in the CM (lab)
  // cos(theta) = (p1"dot"p2)/(abs(p1)*abs(p2))
  StThreeVectorF p1 = Track1->geometry()->momentum();
  StThreeVectorF p2 = Track2->geometry()->momentum();
  Float_t ScalarProduct = p1*p2;
  Float_t Denominator   = p1.mag()*p2.mag();
  Float_t OpnAng = acos(ScalarProduct/Denominator);
  return OpnAng;
}

Float_t StPeCPair::cosThetaStar(StPeCParticle pid) const{
  // ThetaStar is the angle between of one of the daughter tracks
  // and the Z-axis in the Helicity frame. The Helicity frame is
  // that rest frame of the parent particle in which the direction of 
  // the scattered nucleus is along the negative Z-axis. See K.Schilling,
  // P.Seyboth, G. Wolf Nucl. Phys. B 15(1970)397-412.
  // Since the outgoing nuclei are not tagged, this direction cannot 
  // be determined exactly. Because of the low momentum transfers it is,
  // however, a reasonable approximation to assume that the nuclei move 
  // parallel to the Z-axis of the lab frame. 
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
  StLorentzVectorF parent = getPair4Momentum(pid);
  StThreeVectorF   p1 = Track1->geometry()->momentum();
  StThreeVectorF   p2 = Track2->geometry()->momentum();
  Float_t          e1 = p1.massHypothesis(mptcle);
  Float_t          e2 = p2.massHypothesis(mptcle);
  StLorentzVectorF d1(e1,p1);
  StLorentzVectorF d2(e2,p2);
  // Get the sign right for the boost lab --> parent rest frame
  // Default is in the other direction
  StThreeVectorF sp = -1.0*parent.vect();
  parent.setVect(sp);
  d1 = d1.boost(parent);
  d2 = d2.boost(parent);
  Float_t d1th = d1.cosTheta();  Float_t d2th = d2.cosTheta();
  // Define cosThetaStar in the interval 0<cosThetaStar<1
  if( d1th > 0 ){
    return d1th;
  }
  else{
    return d2th;
  }
}







