#ifndef RNORM_H
#define RNORM_H

// Generation of two random numbers distributed by normal distribution.
// It is generator-independent. The two flat numbers are its parameters.

namespace Heed {

class GausState {
 public:
  double second_ran;
  int s_inited_second_ran;
  GausState(void) : second_ran(0.0), s_inited_second_ran(0) {}
};
extern GausState gaus_state;

double rnorm_improved(void);  // uses calls to SRANLUX

void rnorm_double(const double r1, const double r2, // flat random numbers
                  double &x1, double &x2);          // results

void rnorm_float(const float r1, const float r2,    // flat random numbers
                 float &x1, float &x2);             // results

}

#endif
