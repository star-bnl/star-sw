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

#include "StEvent/StGlobalTrack.hh"
#include "StThreeVector.hh"
#include <stdio.h>

//double StAngleDiff::phiDiff(StGlobalTrack *track1, StGlobalTrack *track2) {
double StAngleDiff::phiDiff(StThreeVector<double> m1, 
			    StThreeVector<double> m2) {
  // calculates difference in phi angles between two tracks
  // 0 < phidiff < 2pi
  // requires bfield, see Thomas for this !!!
  //  
  //  const double  bField = 0.5*tesla;
  //  StThreeVector<double> m1 = track1->helix().momentum(bField);
  double phi1  = m1.phi();
  //  StThreeVector<double> m2 = track2->helix().momentum(bField);
  double phi2  = m2.phi();

  double phidiff = phi1-phi2;
  if (phidiff < 0) {
    phidiff =+ 2.*pi ;
  }

  return phidiff ;
};

//double StAngleDiff::alphaDiff(StGlobalTrack *track1, StGlobalTrack *track2) {
 double StAngleDiff::alphaDiff(StThreeVector<double> m1, 
			       StThreeVector<double> m2) {
  // calculates  angle between the momenta of two tracks
  // 0 < alphadiff < 2pi
  // requires bfield, see Thomas for this !!!
  //  
   //  const double  bField = 0.5*tesla;
   //  StThreeVector<double> m1 = track1->helix().momentum(bField); 
   //  StThreeVector<double> m2 = track2->helix().momentum(bField);
  double p1 = abs(m1);
  double p2 = abs(m2);
  double alphadiff=acos((m1.x()*m2.x()+m1.y()*m2.y()+m1.z()*m2.z())/(p1*p2));
  
  return alphadiff;
};

double  StAngleDiff::weightPhi(double phidiff) {
  // placeholder for two-track acceptance function

  double weight = 1;
  if (phidiff < 0.02 ) {
    weight = 0.;
  }
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







