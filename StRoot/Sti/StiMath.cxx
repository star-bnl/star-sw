#include "TObject.h"
#include <stdexcept>
#include <values.h>
#include <math.h>
#include "Sti/StiMath.h"
ClassImp(StiMath)

double StiMath::_logGamma[200];

double StiMath::chi2(double x, int n)
{
  double n2 = double(n)/2.;
  double logChi2 = (n2-1.)*log(x)-x/2.-n2*log(2.)-logGamma(n2);
  return exp(logChi2);
}



void StiMath::initialize()
{
  _logGamma[0] = 1.;
  _logGamma[1] = sqrt(M_PI);
  _logGamma[2] = 1.;
  _logGamma[3] = 1.5*_logGamma[1];
  double x;
  int i;
  for (i=4;i<200;)
    {
      x = log((double)i);
      _logGamma[i] = x+_logGamma[i-2]; ++i;
      x = log(((double)i)/2);
      _logGamma[i] = x+_logGamma[i-2]; ++i;
    }
}

///Calculates the log of the gamma fct 
///based on a clever recipee by Lanczos
///This code taken from Numerical Recipees by Press et al.
double StiMath::logGamma(double xx)
{
 double x,y,tmp,ser;
 static double cof[6]={76.18009172947146,-86.50532032941677,
  24.01409824083091,-1.231739572450155,
  0.1208650973866179e-2,-0.5395239384953e-5};
 int j;
   
 y=x=xx;
 tmp=x+5.5;
 tmp -= (x+0.5)*log(tmp);
 ser=1.000000000190015;
 for (j=0;j<=5;j++) ser += cof[j]/++y;
 return -tmp+log(2.5066282746310005*ser/x);
}


double StiMath::gamma(double x)
{
  if (x<0)
    throw runtime_error("gamma(double x) -E- Invalid argument: x<0");
  if (x>=100.)
    return DBL_MAX;
  int n = int(2*x);
  return exp(_logGamma[n]);  
}

