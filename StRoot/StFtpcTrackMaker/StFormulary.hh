// $Id: StFormulary.hh,v 1.3 2002/04/09 16:10:11 oldi Exp $
// $Log: StFormulary.hh,v $
// Revision 1.3  2002/04/09 16:10:11  oldi
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
// Revision 1.1  2000/05/10 13:39:05  oldi
// Initial version of StFtpcTrackMaker
//

#ifndef STAR_StFormulary
#define STAR_StFormulary

#include "TMath.h"

#ifndef ROOT_Rtypes
#include "Rtypes.h"
#endif

class StFormulary
{
 public:
    
    static Double_t  CheckASinArg(Double_t asin_arg);                                             // returns +1. if asin_arg>1. or -1. if asin_arg<-1.
    static Double_t  RelDiff(const Double_t p1, const Double_t p2);                               // retuns the relative difference of two values
        static void  Pq(const Double_t *p, const Double_t *q, Double_t x[2]);                     // x(y=0) values of a parabola
    static Double_t  Angle(const Double_t *x1, const Double_t *x2, Int_t dim = 3);                // angle between to vectors
    static Double_t  Dist_squared(const Double_t *p1, const Double_t *p2, Int_t dim = 3);         // squared distance of two points in space
    static Double_t  Dist(const Double_t *p1,  const Double_t *p2, Int_t dim = 3);                // distance of two points in space
        static void  VectorProd(const Double_t *a, const Double_t *b, Double_t *c);               // vector product (3D)
    static Double_t  ScalarProd(const Double_t *a, const Double_t *b, Int_t dim = 3);             // scalar product of two verctors
    static Double_t  Square(const Double_t *p, Int_t dim = 3);                                    // squared length of a vector
        static void  Sum(const Double_t *p1, const Double_t *p2, Double_t *P, Int_t n = 3);       // sum of two vectors
        static void  Diff(const Double_t *p1, const Double_t *p2, Double_t *P, Int_t n = 3);      // difference of two vectors
    static Double_t  Abs(const Double_t *p, Int_t dim = 3);                                       // length of a vector

    static Double_t  GetMagneticFieldFactor();                                                    // returns magnetic field factor

    ClassDef(StFormulary,0)  // Mathematical formulary
};    


inline Double_t StFormulary::CheckASinArg(Double_t asin_arg)
{
  // Returns +1. if asin_arg>1. or -1. if asin_arg<-1.

  if (TMath::Abs(asin_arg) > 1.) {
    asin_arg = (asin_arg >= 0) ? +1. : -1.;
  }
  
  return asin_arg;
}


inline Double_t StFormulary::RelDiff(const Double_t p1, const Double_t p2)
{
  // Returns the relative diffenrence of two points.

  return (p1 - p2) / (p1 + p2);
}


inline Double_t StFormulary::Dist(const Double_t *p1,  const Double_t *p2, Int_t dim)
{
  // Calculates the distance of two points in euclidian geometry.
  
  return TMath::Sqrt(Dist_squared(p1, p2, dim));
}


inline void StFormulary::VectorProd(const Double_t *a, const Double_t *b, Double_t *c)
{
  // Calculates the (euclidian) vector product of two vectors in three dimensions.

  c[1] = a[2]*b[3] - a[3]*b[2];
  c[2] = a[3]*b[1] - a[1]*b[3];
  c[3] = a[1]*b[2] - a[2]*b[1];
  
  return;
}


inline Double_t StFormulary::ScalarProd(const Double_t *a, const Double_t *b, Int_t dim)
{
  // Calculates the (euclidian) scalar product of two vectors

  Double_t result = 0.;

  for (Int_t i=0; i<dim; result += a[i] * b[i], i++);

  return result;
}


inline Double_t StFormulary::Square(const Double_t *p, Int_t dim)
{
  // Calculates the scalar product of the given vector with itself.

  return ScalarProd(p, p, dim);
}


inline void StFormulary::Sum(const Double_t *p1, const Double_t *p2, Double_t *P, Int_t n)
{
  // Calculates the sum of the two n-dimensional vectors and stores the 
  // result in the vector P.

  for (Int_t i=0; i<n; P[i] = p1[i] + p2[i], i++);  
  
  return;
}


inline void StFormulary::Diff(const Double_t *p1, const Double_t *p2, Double_t *P, Int_t n)
{
  // Calculates the difference of the two n-dimensional vectors and stores the 
  // result in the vector P.

  for (Int_t i=0; i<n; P[i] = p1[i] - p2[i], i++);

  return;
}


inline Double_t StFormulary::Abs(const Double_t *p, Int_t dim)
{
  // Calculates the length of a vector in ordinary euclidian geometry.
    
  return TMath::Sqrt(Square(p, dim));
}

#endif
