// $Id: StFormulary.cc,v 1.2 2000/07/18 21:22:15 oldi Exp $
// $Log: StFormulary.cc,v $
// Revision 1.2  2000/07/18 21:22:15  oldi
// Changes due to be able to find laser tracks.
// Cleanup: - new functions in StFtpcConfMapper, StFtpcTrack, and StFtpcPoint
//            to bundle often called functions
//          - short functions inlined
//          - formulas of StFormulary made static
//          - avoid streaming of objects of unknown size
//            (removes the bunch of CINT warnings during compile time)
//          - two or three minor bugs cured
//
// Revision 1.1  2000/05/10 13:39:03  oldi
// Initial version of StFtpcTrackMaker
//

//----------Author:        Markus D. Oldenburg
//----------Last Modified: 18.07.2000
//----------Copyright:     &copy MDO Production 1999

#include "StFormulary.hh"
#include "TMath.h"

///////////////////////////////////////////////////////////////////////////////
// StFormulary                                                               //
//                                                                           //
// The StFormulary class improves the TMath class by adding some often used  // 
// functions and calulations.                                                //
///////////////////////////////////////////////////////////////////////////////

ClassImp(StFormulary)

void StFormulary::Pq(const Double_t *p, const Double_t *q, Double_t x[2]) 
{
  // Calculates the x(y=0)-values of a parabola of the form
  // x^2 + p * x + q = 0.

  Int_t i;

  Double_t minus_p_halbe = -(*p)/2.;
  Double_t wurzel = TMath::Sqrt(-(*q) + TMath::Power((*p)/2., 2));

  for (i = 0; i < 2; i++) {
    x[i] = minus_p_halbe + TMath::Power(-1., i) * wurzel;
  }
  
  return;
}


Double_t StFormulary::Angle(const Double_t *x1, const Double_t *x2, Int_t dim)
{
  // Returns the angle between to vectors (in dim dimensions).

  Double_t value = ScalarProd(x1, x2, dim)/(Abs(x1, dim) * Abs(x2, dim));
  
  // This ugly if statement was necessary because if the angle was exactly 0. TMath::ACos() was confused sometimes.
  if (TMath::Abs(value) < 1.) {
    return TMath::ACos(value);
  }

  else if (value >= 1. && value < 1.00001) {
    return 0.;
  }

  else {
    return TMath::ACos(value);
  }
}


Double_t StFormulary::Dist_squared(const Double_t *p1, const Double_t *p2, Int_t dim)
{
  // Calculates the distance squared of two points in euclidian geometry.

  Double_t *minus = new Double_t[dim];

  Diff(p1, p2, minus, dim);
  Double_t dist_sq = Square(minus, dim);
  delete[] minus;

  return dist_sq;
}
