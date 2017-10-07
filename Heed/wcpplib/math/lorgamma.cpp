#include <cmath>
#include "wcpplib/clhep_units/WPhysicalConstants.h"
#include "wcpplib/math/lorgamma.h"
#include "wcpplib/util/FunNameStack.h"

namespace Heed {

using CLHEP::c_squared;

double lorgamma_1(double beta) {
  if (beta == 0.0) return 0.0;
  if (beta >= 1.0) {
    mcout << "double lorgamma_1(double beta): ERROR: beta>=1.0, beta=" << beta
          << "\n";
    spexit(mcerr);
  }
  beta *= beta;
  const double g2_1 = beta / (1. - beta);
  const double gam = sqrt(g2_1 + 1.);
  return g2_1 / (gam + 1.);
}

double lorbeta(const double gamma_1) {
  return sqrt(gamma_1 * (gamma_1 + 2.)) / (gamma_1 + 1.);
}

double lorbeta2(const double gamma_1) {
  const double g = gamma_1 + 1;
  return (gamma_1 * (gamma_1 + 2.)) / (g * g);
}

double lorbeta(const double momentum, const double mass) {
  double x = (mass * mass * c_squared) / (momentum * momentum);
  x = x + 1.0;
  return sqrt(1. / x);
}
}
