///////////////////////////////////////////////////////////////////////////////
//
// StAngleDiff.cc
//
// Description: 
//  Implements class StAngleDiff.hh for use in high-pt anglur correlations
//
// Environment:
//  Software developed for the STAR Detector at Brookhaven National Laboratory
//
// Author List: 
//  Craig Ogilvie, MIT 3/99
//
// History:
//
///////////////////////////////////////////////////////////////////////////////
#include "SystemOfUnits.h"
#include "PhysicalConstants.h"
#include "StAngleDiff.hh"

#include "StGlobalTrack.h"
#include "StThreeVectorD.hh"
#include <stdio.h>
#include <math.h>

double StAngleDiff::phiDiff(StThreeVectorD m1, 
			    StThreeVectorD m2) {
  // calculates difference in phi angles between two tracks
  // 0 < phidiff < pi

  double phi1  = m1.phi();
  double phi2  = m2.phi();

  if (phi1< 0) phi1 = phi1 + 2.*pi;
  if (phi2< 0) phi2 = phi2 + 2.*pi;

  double phidiff = fabs(phi1-phi2);
  if (phidiff > pi) {
      phidiff = 2.*pi - phidiff ;
   }
  return phidiff ;
};

 double StAngleDiff::alphaDiff(StThreeVectorD m1, 
			       StThreeVectorD m2) {
  // calculates  angle between the momenta of two tracks
  // 0 < alphadiff < 2pi

  double p1 = abs(m1);
  double p2 = abs(m2);
  double alphadiff=acos((m1.x()*m2.x()+m1.y()*m2.y()+m1.z()*m2.z())/(p1*p2));
  
  return alphadiff;
};

double  StAngleDiff::weightPhiDiff(StThreeVectorD m1, 
			       StThreeVectorD m2) {
  // placeholder for two-track acceptance function

  double weight = 1;
  if (phiDiff(m1,m2) < 0.02 ) weight = 0.0 ;
  return weight;
};

double  StAngleDiff::weightAlpha(double alphadiff) {
 // placeholder for two-track acceptance function

  double weight =1;
  if (alphadiff < 0.02 ) {
    weight = 0.;
  }
  return weight;
};







