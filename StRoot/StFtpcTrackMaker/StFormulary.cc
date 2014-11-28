// $Id: StFormulary.cc,v 1.5 2003/09/16 15:27:01 jcs Exp $
// $Log: StFormulary.cc,v $
// Revision 1.5  2003/09/16 15:27:01  jcs
// removed inline as it would leave a few undefined reference
//
// Revision 1.4  2002/10/03 10:33:56  oldi
// Usage of gufld removed.
// Magnetic field is read by StMagUtilities, now.
//
// Revision 1.3  2002/04/09 16:10:09  oldi
// Method to get the magentic field factor moved to StFormulary. It works for
// simulation as well, now.
//
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


Double_t StFormulary::CheckASinArg(Double_t asin_arg)
{
  // Returns +1. if asin_arg>1. or -1. if asin_arg<-1.

  if (TMath::Abs(asin_arg) > 1.) {
    asin_arg = (asin_arg >= 0) ? +1. : -1.;
  }
  
  return asin_arg;
}


Double_t StFormulary::RelDiff(const Double_t p1, const Double_t p2)
{
  // Returns the relative diffenrence of two points.

  return (p1 - p2) / (p1 + p2);
}


Double_t StFormulary::Dist(const Double_t *p1,  const Double_t *p2, Int_t dim)
{
  // Calculates the distance of two points in euclidian geometry.
  
  return TMath::Sqrt(Dist_squared(p1, p2, dim));
}


void StFormulary::VectorProd(const Double_t *a, const Double_t *b, Double_t *c)
{
  // Calculates the (euclidian) vector product of two vectors in three dimensions.

  c[1] = a[2]*b[3] - a[3]*b[2];
  c[2] = a[3]*b[1] - a[1]*b[3];
  c[3] = a[1]*b[2] - a[2]*b[1];
  
  return;
}


Double_t StFormulary::ScalarProd(const Double_t *a, const Double_t *b, Int_t dim)
{
  // Calculates the (euclidian) scalar product of two vectors

  Double_t result = 0.;

  for (Int_t i=0; i<dim; result += a[i] * b[i], i++);

  return result;
}


Double_t StFormulary::Square(const Double_t *p, Int_t dim)
{
  // Calculates the scalar product of the given vector with itself.

  return ScalarProd(p, p, dim);
}


void StFormulary::Sum(const Double_t *p1, const Double_t *p2, Double_t *P, Int_t n)
{
  // Calculates the sum of the two n-dimensional vectors and stores the 
  // result in the vector P.

  for (Int_t i=0; i<n; P[i] = p1[i] + p2[i], i++);  
  
  return;
}


void StFormulary::Diff(const Double_t *p1, const Double_t *p2, Double_t *P, Int_t n)
{
  // Calculates the difference of the two n-dimensional vectors and stores the 
  // result in the vector P.

  for (Int_t i=0; i<n; P[i] = p1[i] - p2[i], i++);

  return;
}


Double_t StFormulary::Abs(const Double_t *p, Int_t dim)
{
  // Calculates the length of a vector in ordinary euclidian geometry.
    
  return TMath::Sqrt(Square(p, dim));
}
