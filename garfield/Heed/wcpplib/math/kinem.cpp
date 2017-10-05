#ifdef VISUAL_STUDIO
#define _USE_MATH_DEFINES
// see comment in math.h:
/* Define _USE_MATH_DEFINES before including math.h to expose these macro
 * definitions for common math constants.  These are placed under an #ifdef
 * since these commonly-defined names are not part of the C/C++ standards.
 */
#endif
#include <cmath>
#include "wcpplib/clhep_units/WPhysicalConstants.h"
#include "wcpplib/math/kinem.h"
#include "wcpplib/util/FunNameStack.h"

namespace Heed {

double cos_theta_two_part(const double Ep0, const double Ep1, const double Mp,
                          const double Mt) {
  mfunname("double cos_theta_two_part(...)");

  const double Mp2 = Mp * Mp;
  const double d0 = Ep0 * Ep0 - Mp2;
  check_econd11(d0, <= 0, mcerr);
  const double d1 = Ep1 * Ep1 - Mp2;
  check_econd11(d1, <= 0, mcerr);
  return (-Ep0 * Mt + Ep0 * Ep1 + Mt * Ep1 - Mp2) / sqrt(d0 * d1);
}

void theta_two_part(const double Ep0, const double Ep1, const double Mp,
                    const double Mt, double& theta_p, double& theta_t) {
  mfunname("void theta_two_part(...)");

  const double Mp2 = Mp * Mp;
  const double d0 = Ep0 * Ep0 - Mp2;
  check_econd11(d0, <= 0, mcerr);
  const double d1 = Ep1 * Ep1 - Mp2;
  check_econd11(d1, <= 0, mcerr);
  double ctheta = (-Ep0 * Mt + Ep0 * Ep1 + Mt * Ep1 - Mp2) / sqrt(d0 * d1);
  if (ctheta < -1.0) ctheta = -1.0;
  if (ctheta > 1.0) ctheta = 1.0;
  theta_p = acos(ctheta);
  if (theta_p == 0.0) {
    theta_t = CLHEP::halfpi;
    return;
  }
  double Pp1 = Ep1 * Ep1 - Mp2;
  check_econd11(Pp1, < 0, mcerr);
  if (Pp1 == 0.0) {
    theta_t = CLHEP::halfpi;
    return;
  }
  Pp1 = sqrt(Pp1);
  const double d3 = Ep0 + Mt - Ep1;
  const double dd1 = d3 * d3 - Mt * Mt;
  check_econd11(dd1, <= 0, mcerr);
  const double dd2 = sqrt(dd1);
  check_econd11(dd2, <= 0, mcerr);
  double stheta_t = -Pp1 * (sin(theta_p) / dd2);
  if (stheta_t < -1.0) stheta_t = -1.0;
  if (stheta_t > 1.0) stheta_t = 1.0;
  theta_t = asin(stheta_t);
}
}
