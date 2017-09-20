#ifdef VISUAL_STUDIO
#define _USE_MATH_DEFINES
// see comment in math.h:
/* Define _USE_MATH_DEFINES before including math.h to expose these macro
 * definitions for common math constants.  These are placed under an #ifdef
 * since these commonly-defined names are not part of the C/C++ standards.
 */
#endif
#include <cmath>
#include "wcpplib/random/rnorm.h"

namespace Heed {

void rnorm_double(const double r1, const double r2, double &x1, double &x2) {
  const double r = sqrt(-2.0 * log(r1));
  const double fi = 2.0 * M_PI * r2;
  x1 = r * cos(fi);
  x2 = r * sin(fi);
}

void rnorm_float(const float r1, const float r2, float &x1, float &x2) {
  const float r = sqrt(-2.0 * log(r1));
  const float fi = 2.0 * M_PI * r2;
  x1 = r * cos(fi);
  x2 = r * sin(fi);
}
}
