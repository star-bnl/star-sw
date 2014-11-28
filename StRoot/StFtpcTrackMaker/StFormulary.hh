// $Id: StFormulary.hh,v 1.5 2003/09/16 15:27:01 jcs Exp $
// $Log: StFormulary.hh,v $
// Revision 1.5  2003/09/16 15:27:01  jcs
// removed inline as it would leave a few undefined reference
//
// Revision 1.4  2002/10/03 10:33:57  oldi
// Usage of gufld removed.
// Magnetic field is read by StMagUtilities, now.
//
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

    ClassDef(StFormulary,0)  // Mathematical formulary
};    


#endif
