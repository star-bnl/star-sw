#include <stdlib.h>
#include <limits.h>
#include <cmath>
#include "wcpplib/ioniz/e_cont_enloss.h"
#include "wcpplib/math/lorgamma.h"
#include "wcpplib/clhep_units/WPhysicalConstants.h"
#include "wcpplib/util/FunNameStack.h"

// 2003,  I. Smirnov

namespace Heed {

using CLHEP::twopi;
using CLHEP::electron_mass_c2;
using CLHEP::classic_electr_radius;
using CLHEP::GeV;
using CLHEP::gram;
using CLHEP::mole;
using CLHEP::Avogadro;
using CLHEP::cm2;
using CLHEP::cm3;

double e_cont_enloss(double ratio_Z_to_A,  // do not forget:
                     // 1.0/(gram/mole)
                     double I_eff, double density,
                     double Ekin,  // in internal units
                     double Ecut,  // in internal units
                     double z) {
  mfunname("double e_cont_enloss(...)");
  const double gamma_1 = Ekin / electron_mass_c2;
  if (gamma_1 <= 0.0) return 0.;
  const double gamma = gamma_1 + 1.;
  const double gamma2 = gamma * gamma;

  const double Tcme = Ecut / electron_mass_c2;
  const double beta = lorbeta(gamma_1);
  const double beta2 = beta * beta;
  // calculation of F^+-
  double F;
  if (z > 0) {
    // positron
    const double y = 1.0 / (1.0 + gamma);
    const double D = std::min(Tcme, gamma_1);
    const double D2 = 0.5 * D * D;
    const double D3 = 2.0 * D2 * D / 3.0;
    const double D4 = D2 * D2;
    F = log(gamma_1 * D) -
        beta2 * (gamma_1 + 2.0 * D -
                 y * (3.0 * D2 + y * (D - D3 + y * (D2 - gamma_1 * D3 + D4)))) /
            gamma_1;
  } else {
    // electron
    const double D = std::min(Tcme, 0.5 * gamma_1);
    F = -1.0 - beta2 + log((gamma_1 - D) * D) + gamma_1 / (gamma_1 - D) +
        (0.5 * D * D + (1.0 + 2.0 * gamma_1) * log(1.0 - D / gamma_1)) / gamma2;
  }
  const double logI = log(I_eff / electron_mass_c2);
  // Electron density (in 1 / [length^3])
  const double eldens = ratio_Z_to_A * Avogadro * density;
  // Dimensionless constant
  double C = 1.0 + 2.0 * log((I_eff / GeV) /
                             (28.8e-9 * sqrt(density / (gram / cm3) *
                                             ratio_Z_to_A * gram / mole)));
  // Iprintn(mcout, density/(g/cm3));
  // Iprintn(mcout, ratio_Z_to_A * gram/mole);
  // Iprintn(mcout, C);
  double x0, x1;
  if (density > 0.05 * gram / cm3) {
    // mcout<<"density > 0.05 * g/cm3\n";
    if (I_eff < 1.0e-7 * GeV) {
      if (C < 3.681) {
        x0 = 1.0;
      } else {
        x0 = 0.326 * C - 1.0;
      }
      x1 = 2.0;
    } else {
      // mcout<<"I_eff >= 1.0e-7 * GeV\n";
      if (C < 5.215) {
        // mcout<<"C < 5.215\n";
        x0 = 0.2;
      } else {
        x0 = 0.326 * C - 1.5;
      }
      x1 = 3.0;
    }
  } else {
    // mcout<<"density <= 0.05 * g/cm3\n";
    if (C <= 12.25) {
      // mcout<<"C <= 12.25\n";
      double ip = long((C - 10.0) / 0.5) + 1;
      if (ip < 0) ip = 0;
      if (ip > 4) ip = 4;
      x0 = 1.6 + 0.1 * ip;
      x1 = 4.0;
    } else {
      if (C <= 13.804) {
        x0 = 2.0;
        x1 = 5.0;
      } else {
        x0 = 0.326 * C - 2.5;
        x1 = 5.0;
      }
    }
  }
  const double xa = C / 4.606;
  const double aa = 4.606 * (xa - x0) / pow(x1 - x0, 3);
  const double x = log(gamma_1 * (gamma + 1.0)) / 4.606;
  double del = 0.0;
  if (x > x0) {
    del = 4.606 * x - C;
    if (x <= x1) del = del + aa * pow(x1 - x, 3);
  }
  const double cons =
      twopi * classic_electr_radius * classic_electr_radius * electron_mass_c2;
  // double cons = 0.153536e-3 * GeV * cm2 / Avogadro;
  double dedx =
      cons * eldens * (log(2.0 * gamma_1 + 4.0) - 2.0 * logI + F - del) / beta2;
  return dedx > 0. ? dedx : 0.;
}
}
