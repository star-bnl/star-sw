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
///for integers and half-integers.
///Returns MAX_DOUBLE for x>=100;
double StiMath::logGamma(double x)
{
  if (x<0)
    throw runtime_error("logGamma(double x) -E- Invalid argument: x<0");
  if (x>=100.)
    return DBL_MAX;
  int n = 2*x;
  return _logGamma[n];  
}


double StiMath::gamma(double x)
{
  if (x<0)
    throw runtime_error("gamma(double x) -E- Invalid argument: x<0");
  if (x>=100.)
    return DBL_MAX;
  int n = 2*x;
  return log(_logGamma[n]);  
}
