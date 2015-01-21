#ifndef VFLOAT_H
#define VFLOAT_H
#include <cfloat>
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
#define max_vfloat DBL_MAX  //1.0E+100           // ??
typedef double vfloat;
const vfloat max_safe_vfloat = 0.5 * sqrt(DBL_MAX);  //e+307
const vfloat vprecision = 1.0E-12;

inline vfloat abslt(vfloat f) { return f > 0.0 ? f : -f; }

inline int apeq(vfloat f1, vfloat f2, vfloat prec = vprecision) {
  if (abslt(f1 - f2) <= prec)
    return 1;
  else
    return 0;
}
inline int not_apeq(vfloat f1, vfloat f2, vfloat prec = vprecision) {
  if (abslt(f1 - f2) <= prec)
    return 0;
  else
    return 1;
}

namespace Heed {
inline int apeq(vfloat f1, vfloat f2, vfloat prec = vprecision) {
  if (abslt(f1 - f2) <= prec)
    return 1;
  else
    return 0;
}
inline int not_apeq(vfloat f1, vfloat f2, vfloat prec = vprecision) {
  if (abslt(f1 - f2) <= prec)
    return 0;
  else
    return 1;
}
}

#endif
