// $Id: StFormulary.hh,v 1.1 2000/05/10 13:39:05 oldi Exp $
// $Log: StFormulary.hh,v $
// Revision 1.1  2000/05/10 13:39:05  oldi
// Initial version of StFtpcTrackMaker
//

#ifndef STAR_StFormulary
#define STAR_StFormulary

#include "TMath.h"

class StFormulary : public TMath
{
 public:
    
              StFormulary(); // Constructor
     virtual ~StFormulary(); // Destructor

    Double_t  RelDiff(const Double_t p1, const Double_t p2);                               // retuns the relative difference of two values
        void  Pq(const Double_t *p, const Double_t *q, Double_t x[2]);                     // x(y=0) values of a parabola
    Double_t  Angle(const Double_t *x1, const Double_t *x2, Int_t dim = 3);                // angle between to vectors
    Double_t  Dist_squared(const Double_t *p1, const Double_t *p2, Int_t dim = 3);         // squared distance of two points in space
    Double_t  Dist(const Double_t *p1,  const Double_t *p2, Int_t dim = 3);                // distance of two points in space
        void  VectorProd(const Double_t *a, const Double_t *b, Double_t *c);               // vector product (3D)
    Double_t  ScalarProd(const Double_t *a, const Double_t *b, Int_t dim = 3);             // scalar product of two verctors
    Double_t  Square(const Double_t *p, Int_t dim = 3);                                    // squared length of a vector
        void  Sum(const Double_t *p1, const Double_t *p2, Double_t *P, Int_t n = 3);       // sum of two vectors
        void  Diff(const Double_t *p1, const Double_t *p2, Double_t *P, Int_t n = 3);      // difference of two vectors
    Double_t  Abs(const Double_t *p, Int_t dim = 3);                                       // length of a vector

    ClassDef(StFormulary,0)  // Mathematical formulary
};    

#endif
