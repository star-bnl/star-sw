#ifndef STHBT_GAMOVCORRECT
#define STHBT_GAMOVCORRECT


#include "StHbtMaker/Infrastructure/StHbtTypes.hh"
#include "StHbtMaker/Infrastructure/StHbtPair.hh"
#include "PhysicalConstants.h"
#include <cstdio>


double gamovCorrect(const StHbtPair* pair) {
  // G = 2*pi*(eta)/(Exp(2*pi*(eta))-1)
  // eta = (reducedMass)*e^2/((h_bar)*c*Qinv)
  double mass1 = pair->track1()->FourMomentum().m();
  double mass2 = pair->track2()->FourMomentum().m();
  double reducedMass = mass1*mass2/(mass1+mass2);
  double Qinv = fabs(pair->qInv());
  // Assumes same charge particles
  double eta = reducedMass*elm_coupling/(hbarc*Qinv);
  double gamov = twopi*eta/(exp(twopi*eta)-1);
  return (gamov);
}

#endif
