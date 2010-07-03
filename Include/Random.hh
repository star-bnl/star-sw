// Random number generation and sampling from random number distributions

#ifndef G_RANDOM_H
#define G_RANDOM_H

#include <cmath>
// #include "RandomEngineGSL.hh"
#include "RandomEngineRoot.hh"

namespace Garfield {

  // Random number generator
  // extern RandomEngineGSL randomEngine;
  extern RandomEngineRoot randomEngine;

  // Draw a random number uniformly distributed in the range [0, 1)
  inline
  double RndmUniform() {

    return randomEngine.Draw();
  
  }

  // Draw a random number uniformly distributed in the range (0, 1)
  inline
  double RndmUniformPos() {

    double r = randomEngine.Draw();
    while (r <= 0.) r = randomEngine.Draw();
    return r;

  }

  // Draw a Gaussian random variate with mean zero and standard deviation one
  inline
  double RndmGaussian() {

    // Box-Muller algorithm
    double v1 = 2. * randomEngine.Draw() - 1.;
    double v2 = 2. * randomEngine.Draw() - 1.;
    double r2 = v1 * v1 + v2 * v2;
    while (r2 > 1.) {
      v1 = 2. * randomEngine.Draw() - 1.;
      v2 = 2. * randomEngine.Draw() - 1.;
      r2 = v1 * v1 + v2 * v2;
    }
    return v1 * sqrt(-2. * log(r2) / r2);

  }

  // Draw a Gaussian random variate with mean mu and standard deviation sigma
  inline
  double RndmGaussian(const double mu, const double sigma) {

    return mu + sigma * RndmGaussian();    

  }

  // Draw a Polya distributed random number
  inline
  double RndmPolya(const double theta) { 
  
    // Algorithm from Review of Particle Physics
    // C. Amsler et al, Phys. Lett. B 667 (2008)
    if (theta <= 0.) return - log(RndmUniformPos());
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
