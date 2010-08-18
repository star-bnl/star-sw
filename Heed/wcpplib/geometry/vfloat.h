#ifndef VFLOAT_H
#define VFLOAT_H
//#include <values.h>    // obsolete header
#include <cfloat>
#include <iostream>
using std::ostream;
#ifdef VISUAL_STUDIO
#define _USE_MATH_DEFINES
// see comment in math.h:
/* Define _USE_MATH_DEFINES before including math.h to expose these macro
 * definitions for common math constants.  These are placed under an #ifdef
 * since these commonly-defined names are not part of the C/C++ standards.
 */
#endif
#include <cmath>
#include "wcpplib/util/inlinec.h"
//#define vprecision 1.0E-12

//#define vprecision 1.0E-12   // It's precision assumed to be for calculations
// with vfloat type. For double with pentuim it is about 1.0E-16.
#define max_vfloat DBL_MAX           //1.0E+100           // ??
typedef double vfloat;
const vfloat max_safe_vfloat = 0.5*sqrt(DBL_MAX);   //e+307    
const vfloat  vprecision=1.0E-12;

/* somewhy it does not still work. To debug.
#define vprecision 1.0E-5
#define max_vfloat MAXFLOAT           //1.0E+100           // ??
typedef float vfloat;
*/
inline vfloat abslt(vfloat f) {return f > 0.0 ? f : -f ;}
inline int apeq(vfloat f1, vfloat f2, vfloat prec=vprecision)
{
  if(abslt(f1-f2) <= prec) return 1; else return 0; 
}
inline int not_apeq(vfloat f1, vfloat f2, vfloat prec=vprecision)
{
  if(abslt(f1-f2) <= prec) return 0; else return 1; 
}
/*
class vfloat
// Allows to switch float<->double and redefines operation == to
// approximate meaning as it needs for geometry calculations
{
  double r;
public:
  vfloat(double fr) {r=fr;}
  //vfloat(int fr) {r=fr;}
  //vfloat(long fr) {r=fr;}
  vfloat(const vfloat& fv) {r=fv.r;}
  //double operator=(vfloat& fv) {return fv.r;}
  //vfloat operator=(vfloat& fv) {return fv.r;}
  operator double() const {return r;}
  friend wl_inline vfloat operator*(vfloat fv1, vfloat fv2);
  friend wl_inline vfloat operator/(vfloat fv1, vfloat fv2);
  friend wl_inline vfloat operator+(vfloat fv1, vfloat fv2);
  friend wl_inline vfloat operator-(vfloat fv1, vfloat fv2);
  friend wl_inline int operator==(vfloat fv1, vfloat fv2);
  friend wl_inline int operator!=(vfloat fv1, vfloat fv2);
  friend wl_inline vfloat operator+=(vfloat& fv1, vfloat fv2);
  friend wl_inline vfloat operator-=(vfloat& fv1, vfloat fv2);
  friend wl_inline vfloat operator*=(vfloat& fv1, vfloat fv2);
  friend wl_inline vfloat operator/=(vfloat& fv1, vfloat fv2);
  friend wl_inline vfloat abslt(const vfloat& fv);
  friend wl_inline vfloat fabs(const vfloat& fv);
  vfloat(void){r=0;}
  friend wl_inline ostream& operator<<(ostream& file, const vfloat& fv);
};
#ifdef WCPPLIB_INLINE
#include "wcpplib/geometry/vfloat.ic"
#endif
*/

#endif
