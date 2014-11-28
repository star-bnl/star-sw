/***************************************
 * $Id: StHbtSmearPair.cxx,v 1.2 2003/09/02 17:58:32 perev Exp $
 *
 * Author: Mike Lisa, Ohio State lisa@mps.ohio-state.edu
 ****************************************
 *
 * Description: given a StHbtPair, this class provides a corresponding
 *     StHbtPair whose constituent StHbtParticles' momenta have been
 *     smeared according to a parameterization of the STAR momentum
 *     resolution.
 *     >> The input StHbtPair is not altered in anyway <<
 *
 ******************************************
 *
 * $Log: StHbtSmearPair.cxx,v $
 * Revision 1.2  2003/09/02 17:58:32  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.1  2001/05/23 00:19:05  lisa
 * Add in Smearing classes and methods needed for momentum resolution studies and correction
 *
 *
 ******************************************/

#include "StHbtSmearPair.h"
#include "StRandom.hh"

#ifdef __ROOT__
ClassImp(StHbtSmearPair)
#endif


//______________________________________
StHbtSmearPair::StHbtSmearPair(){
  setup();
}
//______________________________________
StHbtSmearPair::StHbtSmearPair(const StHbtPair* unSmearedPair){
  setup();
  SetUnsmearedPair(unSmearedPair);
}
//______________________________________
void StHbtSmearPair::setup(){
  mSmearedPair.SetTrack1(&mParticle1);
  mSmearedPair.SetTrack2(&mParticle2);
}
//______________________________________
void StHbtSmearPair::SetUnsmearedPair(const StHbtPair* unSmearedPair){
  mParticle1.ResetFourMomentum(SmearedMomentum(unSmearedPair->track1()->FourMomentum()));
  mParticle2.ResetFourMomentum(SmearedMomentum(unSmearedPair->track2()->FourMomentum()));
}
//______________________________________
// philosophy behind this smearing is as follows:
// what we *measure* is pT (via curvature), and the angles phi and theta
// momentum is related to this via
// px = pT*cos(phi)   --> Dpx = DpT*cos(phi) - pT*sin(phi)*Dphi = px*DpT/pT - py*Dphi
// py = pT*sin(phi)   --> Dpy = DpT*sin(phi) + pT*cos(phi)*Dphi = py*DpT/pT + px*Dphi
// pz = pT/tan(theta) --> Dpz = DpT/tan(theta) - pT*Dtheta/(sin(theta))^2
//                            = pT*(DpT/pT)/tan(theta) - pT*Dtheta/(sin(theta))^2
//                            = pz*DpT/pT - pT*Dtheta/(sin(theta))^2
//______________________________________
StHbtLorentzVector StHbtSmearPair::SmearedMomentum(StHbtLorentzVector fourmom){
  double pT = fourmom.perp();
  double mass2 = fourmom.m2();
  double px = fourmom.x();
  double py = fourmom.y();
  double pz = fourmom.z();
  double sin2theta = sin(fourmom.theta());
  sin2theta = sin2theta*sin2theta;
  //
  double DpT_div_pT = StRandom::gauss(0.0,mFracPtRes);
  double Dphi = StRandom::gauss(0.0,mPhi_a+mPhi_b*::pow(pT,mPhi_alpha));
  double Dtheta = StRandom::gauss(0.0,mTheta_a+mTheta_b*::pow(pT,mTheta_alpha));
  //
  fourmom.setX(px*(1.0+DpT_div_pT) - py*Dphi);
  fourmom.setY(py*(1.0+DpT_div_pT) + px*Dphi);
  fourmom.setZ(pz*(1.0+DpT_div_pT) - pT*Dtheta/sin2theta);
  fourmom.setE(::sqrt(mass2 + fourmom.x()*fourmom.x()+fourmom.y()*fourmom.y()+fourmom.z()*fourmom.z()));
  //
  return fourmom;
}

