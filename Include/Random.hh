// Random number generation and sampling from random number distributions

#ifndef G_RANDOM_H
#define G_RANDOM_H

#include <cmath>
#include "RandomEngineRoot.hh"
#include "FundamentalConstants.hh"

namespace Garfield {

// Random number generator
extern RandomEngineRoot randomEngine;

// Draw a random number uniformly distributed in the range [0, 1)
inline double RndmUniform() { return randomEngine.Draw(); }

// Draw a random number uniformly distributed in the range (0, 1)
inline double RndmUniformPos() {

  double r = RndmUniform();
  while (r <= 0.) r = RndmUniform();
  return r;
}

// Draw a Gaussian random variate with mean zero and standard deviation one
inline double RndmGaussian() {

  static bool cached = false;
  static double u = 0.;
  if (cached) {
    cached = false;
    return u;
  }
  // Box-Muller algorithm
  u = 2. * RndmUniform() - 1.;
  double v = 2. * RndmUniform() - 1.;
  double r2 = u * u + v * v;
  while (r2 > 1.) {
    u = 2. * RndmUniform() - 1.;
    v = 2. * RndmUniform() - 1.;
    r2 = u * u + v * v;
  }
  const double p = sqrt(-2. * log(r2) / r2);
  u *= p;
  cached = true;
  return v * p;
}

// Draw a Gaussian random variate with mean mu and standard deviation sigma
inline double RndmGaussian(const double mu, const double sigma) {

  return mu + sigma * RndmGaussian();
}

// Draw a Lorentzian random variate with mean mu
// and half-width at half maximum gamma
inline double RndmLorentzian(const double mu, const double gamma) {

  return mu + gamma * tan(Pi * (RndmUniform() - 0.5));
}

// Draw a random number according to a Voigt function with mean mu.
// The Voigt function is a convolution of a
// Gaussian (standard deviation sigma) and
// a Lorentzian (half width gamma).
inline double RndmVoigt(const double mu, const double sigma,
                        const double gamma) {

  if (sigma <= 0.) return RndmLorentzian(mu, gamma);
  const double a = gamma / (Sqrt2 * sigma);
  const double x = RndmLorentzian(0., a) + RndmGaussian(0., 1. / Sqrt2);
  return mu + x * Sqrt2 * sigma;
}

// Draw a Polya distributed random number
inline double RndmPolya(const double theta) {

  // Algorithm from Review of Particle Physics
  // C. Amsler et al, Phys. Lett. B 667 (2008)
  if (theta <= 0.) return -log(RndmUniformPos());
  const double c = 3 * (theta + 1.) - 0.75;
  double u1, u2, v1, v2, v3;
  double x;
  while (1) {
    u1 = RndmUniformPos();
    v1 = u1 * (1. - u1);
    if (v1 == 0.) continue;
    v2 = (u1 - 0.5) * sqrt(c / v1);
    x = theta + v2;
    if (x <= 0.) continue;
    u2 = RndmUniformPos();
    v3 = 64 * u2 * u2 * pow(v1, 3);
    if (v3 <= 1. - 2 * v2 * v2 / x ||
        log(v3) <= 2 * (theta * log(x / theta) - v2)) {
      return x / (theta + 1.);
    }
  }
}
}

#endif
