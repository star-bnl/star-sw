#include <stdexcept>
#include <values.h>
#include <math.h>
#include "Sti/StiMath.h"
double StiMath::_logGammaCof[6]={76.18009172947146,-86.50532032941677,
				 24.01409824083091,-1.231739572450155,
				 0.1208650973866179e-2,-0.5395239384953e-5};
int StiMath::_nTop=4;
double StiMath::_fac[33]={1.0,1.,2.0,6.0,24.0};
double StiMath::_logFac[101];

double StiMath::chi2(double x, int n)
{
  double n2 = double(n)/2.;
  double logChi2 = (n2-1.)*log(x)-x/2.-n2*log(2.)-logGamma(n2);
  return exp(logChi2);
}

///Calculates the log of the gamma fct 
///based on a clever recipee by Lanczos
///This code taken from Numerical Recipees by Press et al.
///Values defined only for xx>0.
double StiMath::logGamma(double xx)
{
 double x,y,tmp,ser;
 int j;
   
 y=x=xx;
 tmp=x+5.5;
 tmp -= (x+0.5)*log(tmp);
 ser=1.000000000190015;
 for (j=0;j<=5;j++) ser += _logGammaCof[j]/++y;
 return -tmp+log(2.5066282746310005*ser/x);
}

///Calculates and returns the value of the Gamma function
///for the given value.
///Values defined only for x>0.
double StiMath::gamma(double x)
{
  if (x<=0)
    throw runtime_error("gamma(double x) -E- Invalid argument: x<=0");
  return exp(logGamma(x));  
}

///Calculates and return the factorial of given n
///valid only for n>=0.
///May overflow for large n.
double StiMath::factorial(int n)
{
  int j;
  if (n<0)
    throw runtime_error("StiMath:factorial(int n) -E- n out of bound");
  if (n>32)
    return gamma(n+1.0);
  while (n>_nTop)
    {
      j=_nTop++;
      _fac[_nTop]=_fac[j]*_nTop;
    }
  return _fac[n];
}

///Calculates and returns the log(factorial(n))
///Valid only for n>=0
///May overflow for large n
double StiMath::logFactorial(int n)
{
  if (n<0) throw runtime_error("StiMath:logFactorial(int n) -E- n out of bound");
  if (n<=1) return 0.0;
  if (n<=100) return _logFac[n]?_logFac[n]:(_logFac[n]=logGamma(n+1.0));
  else
  return logGamma(n+1.0);
}

double StiMath::binomial(int n, int k)
{
  return floor(0.5+exp(logFactorial(n)-logFactorial(k)-logFactorial(n-k)));
}
