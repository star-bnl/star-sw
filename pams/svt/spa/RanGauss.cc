// Random Generatir with a Gaussian Probability
//
// Claude A Pruneau
//

#include "RanGauss.hh"

RanGauss::RanGauss() : Random(), iset(0)
{
	mean = 0.;
	sigma = 1.;
}

RanGauss::RanGauss(double newMean, double newSigma) : Random() , iset(0)
{
	mean = newMean;
	sigma = newSigma;
}

RanGauss::RanGauss(double newMean, double newSigma, unsigned int seed) : Random(seed), iset(0)
{
	mean = newMean;
	sigma = newSigma;
}

double RanGauss::getValue()
{
	if  (iset == 0) 
		{
		do 
			{
				v1=2.0*Random::getValue()-1.0;
				v2=2.0*Random::getValue()-1.0;
				r=v1*v1+v2*v2;
			} while (r >= 1.0);
		fac=sqrt(-2.0*log(r)/r);
		gset=v1*fac;
		iset=1;
		return v2*fac;
		} 
	else 
		{
			iset=0;
			return gset;
		}
}
