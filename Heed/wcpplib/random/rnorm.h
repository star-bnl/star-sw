#ifndef RNORM_H
#define RNORM_H

// Generation of two random numbers distributed by normal distribution.
// It is generator-independent. The two flat numbers are its parameters. 

class GausState
{public:
  double second_ran;
  int s_inited_second_ran;
  GausState(void): second_ran(0.0), s_inited_second_ran(0) {} 
};
extern GausState gaus_state;

double rnorm_improved(void);  // uses calls to SRANLUX


void rnorm_double(double r1, double r2,    // flat random numbers 
		  double &x1, double &x2); // results

void rnorm_float(float r1, float r2,    // flat random numbers 
		 float &x1, float &x2); // results


#endif
