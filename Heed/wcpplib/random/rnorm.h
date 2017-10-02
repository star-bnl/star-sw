#ifndef RNORM_H
#define RNORM_H

#include "Random.hh"

// Generation of two random numbers distributed by normal distribution.
// It is generator-independent. The two flat numbers are its parameters.

namespace Heed {

inline double rnorm_improved() { return Garfield::RndmGaussian(); }

void rnorm_double(const double r1, const double r2,  // flat random numbers
                  double &x1, double &x2);           // results

void rnorm_float(const float r1, const float r2,  // flat random numbers
                 float &x1, float &x2);           // results
}

#endif
