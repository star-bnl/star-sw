#include <stdlib.h>
#include <limits.h>
#include <cmath>
#include "wcpplib/ioniz/bethe_bloch.h"
#include "wcpplib/math/lorgamma.h"
#include "wcpplib/stream/prstream.h"
#include "wcpplib/clhep_units/WPhysicalConstants.h"

// 2002, I. Smirnov

namespace Heed {

using CLHEP::pi;
using CLHEP::twopi;
using CLHEP::classic_electr_radius;
using CLHEP::electron_mass_c2;
using CLHEP::Avogadro;
using CLHEP::c_squared;

double Bethe_Bloch_energy_loss(const double ratio_Z_to_A, const double I_eff,
                               const double beta, const double z) {

  constexpr double coef1 = 4 * pi * classic_electr_radius * 
      classic_electr_radius * electron_mass_c2 * Avogadro;
  const double beta2 = beta * beta;
  const double gamma = lorgamma_1(beta) + 1.;
  const double gamma2 = gamma * gamma;
  const double coef2 = z * z * ratio_Z_to_A / beta2;
  const double sum =
      log(2. * electron_mass_c2 * beta2 * gamma2 / I_eff) - beta2;
  return coef1 * coef2 * sum;
}

double Bethe_Bloch_energy_loss_gamma_1(const double ratio_Z_to_A,
                                       const double I_eff, const double gamma_1,
                                       const double z) {
  // This constant should be 0.3071 according to PDG.
  constexpr double coef1 = 4 * pi * classic_electr_radius * 
      classic_electr_radius * electron_mass_c2 * Avogadro;  
  const double beta = lorbeta(gamma_1);
  const double beta2 = beta * beta;
  const double gamma = gamma_1 + 1.0;
  const double gamma2 = gamma * gamma;
  const double coef2 = z * z * ratio_Z_to_A / beta2;
  const double sum =
      log(2. * electron_mass_c2 * beta2 * gamma2 / I_eff) - beta2;
  return coef1 * coef2 * sum;
}

double Bethe_Bloch_restricted_energy_loss_gamma_1(
    const double ratio_Z_to_A, const double I_eff, const double m,
    const double gamma_1, const double ecut, const double z) {

  // TODO: 4 pi or 2 pi?
  constexpr double coef1 = twopi * classic_electr_radius * 
    classic_electr_radius * electron_mass_c2 * Avogadro;
  const double beta = lorbeta(gamma_1);
  const double beta2 = beta * beta;
  const double gamma = gamma_1 + 1.0;
  const double gamma2 = gamma * gamma;
  const double coef2 = z * z * ratio_Z_to_A / beta2;
  const double mrat = electron_mass_c2 / (m * c_squared);
  const double emax = 2.0 * electron_mass_c2 * beta2 * gamma2 /
                      (1.0 + 2.0 * gamma * mrat + mrat * mrat);
  double sum = 0.;
  if (ecut >= emax) {
    sum = log(2.0 * electron_mass_c2 * beta2 * gamma2 * emax / (I_eff * I_eff)) -
          2.0 * beta2;
  } else {
    sum = log(2.0 * electron_mass_c2 * beta2 * gamma2 * ecut / (I_eff * I_eff)) -
          beta2 * (1.0 + ecut / emax);
  }
  return coef1 * coef2 * sum;
}
}
