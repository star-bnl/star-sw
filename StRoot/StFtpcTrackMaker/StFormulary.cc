// $Id: StFormulary.cc,v 1.1 2000/05/10 13:39:03 oldi Exp $
// $Log: StFormulary.cc,v $
// Revision 1.1  2000/05/10 13:39:03  oldi
// Initial version of StFtpcTrackMaker
//

//----------Author:        Markus D. Oldenburg
//----------Last Modified: 20.04.2000
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


StFormulary::StFormulary() : TMath() {
  // Constructor. Creates a StFormulary object.
} 


StFormulary::~StFormulary() {
  // StFormulary destructor.
}


Double_t StFormulary::RelDiff(const Double_t p1, const Double_t p2)
{
  // Returns the relative diffenrence of two points.

  return (p1 - p2)/(p1 + p2);
}


void StFormulary::Pq(const Double_t *p, const Double_t *q, Double_t x[2]) 
{
  // Calculates the x(y=0)-values of a parabola of the form
  // x^2 + p * x + q = 0.

  Int_t i;

  Double_t minus_p_halbe = -(*p)/2.;
  Double_t wurzel = Sqrt(-(*q) + Power((*p)/2., 2));

  for (i = 0; i < 2; i++) {
    x[i] = minus_p_halbe + Power(-1., i) * wurzel;
  }
  
  return;
}


Double_t StFormulary::Angle(const Double_t *x1, const Double_t *x2, Int_t dim)
{
  // Returns the angle between to vectors (in dim dimensions).

  return TMath::ACos(ScalarProd(x1, x2, dim)/(Abs(x1, dim) * Abs(x2, dim)));
}


Double_t StFormulary::Dist_squared(const Double_t *p1, const Double_t *p2, Int_t dim)
{
  // Calculates the distance squared of two points in euclidian geometry.

  Int_t i;
  Double_t *minus = new Double_t[dim];

  for (i=0; i<dim; i++) {
    minus[i] = p2[i] - p1[i];
  }

  Double_t dist_sq = Square(minus, dim);
  delete[] minus;
  return dist_sq;
}


Double_t StFormulary::Dist(const Double_t *p1,  const Double_t *p2, Int_t dim)
{
  // Calculates the distance of two points in euclidian geometry.
  
  return Sqrt(Dist_squared(p1, p2, dim));
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

  Int_t i;
  Double_t result = 0.;

  for (i=0; i<dim; i++) {
    result += a[i] * b[i];
  }

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

  Int_t i;

  for (i=0; i<n; i++) {
    P[i] = p1[i] + p2[i];
  }
  
  return;
}


void StFormulary::Diff(const Double_t *p1, const Double_t *p2, Double_t *P, Int_t n)
{
  // Calculates the difference of the two n-dimensional vectors and stores the 
  // result in the vector P.

  Int_t i;

  for (i=0; i<n; i++) {
    P[i] = p1[i] - p2[i];
  }
  
  return;
}


Double_t StFormulary::Abs(const Double_t *p, Int_t dim)
{
  // Calculates the length of a vector in ordinary euclidian geometry.
    
  return Sqrt(Square(p, dim));
}

