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

namespace Heed {

typedef double vfloat;
const vfloat vprecision = 1.0E-12;
static const vfloat max_vfloat = DBL_MAX;

inline bool apeq(const vfloat f1, const vfloat f2, 
                 const vfloat prec = vprecision) {
  return (fabs(f1 - f2) <= prec);
}

}

#endif
