// $Id: minv.cc,v 1.1 1999/04/06 20:47:36 akio Exp $
// $Log: minv.cc,v $
// Revision 1.1  1999/04/06 20:47:36  akio
// The first version
//
///////////////////////////////////////////////////////////////////////////////
//
// minv.cc
//
// Description: 
//  Computes the invariant mass of two particles
//
// Environment:
//  Software developed for the STAR Detector at Brookhaven National Laboratory
//
// Author List: 
//  Joakim Nystrand, LBNL  3/99
//
// History:
//
///////////////////////////////////////////////////////////////////////////////
#include <math.h>

double minv(double m1, double px1, double py1, double pz1, double m2, double px2, double py2, double pz2)
{
  double E1 = sqrt( m1*m1 + px1*px1 + py1*py1 + pz1*pz1 );
  double E2 = sqrt( m2*m2 + px2*px2 + py2*py2 + pz2*pz2 );
  double minv = sqrt( m1*m1 + m2*m2 +2*E1*E2 - 2*(px1*px2+py1*py2+pz1*pz2) );
  return minv;
}

