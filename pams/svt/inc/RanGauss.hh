// Random Generator with a Gaussian Probability
//
// Claude A Pruneau
//

#ifndef _RANGAUSS_
#define _RANGAUSS_

#include "Random.hh"
#include <math.h>

class RanGauss : public Random
{
protected:
	double sigma;
	double mean;

  int iset;
	double gset;
	double fac,r,v1,v2;

public:
	RanGauss();
	RanGauss(double mean, double sigma);
	RanGauss(double mean, double sigma, unsigned int seed);

	double getValue();

};

#endif
