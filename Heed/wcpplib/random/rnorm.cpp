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
#include "wcpplib/random/ranluxint.h"

namespace Heed {

GausState gaus_state;

double rnorm_improved(void) {
  if (gaus_state.s_inited_second_ran == 1) {
    gaus_state.s_inited_second_ran = 0;
    return gaus_state.second_ran;
  }
  double x1, x2, w, y1, y2;
  do {
    x1 = 2.0 * SRANLUX() - 1.0;
    x2 = 2.0 * SRANLUX() - 1.0;
    w = x1 * x1 + x2 * x2;
  } while (w > 1.0);

  w = sqrt(-2.0 * log(w) / w);
  y1 = x1 * w;
  y2 = x2 * w;
  gaus_state.s_inited_second_ran = 1;
  gaus_state.second_ran = y2;
  return y1;
}

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

