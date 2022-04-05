#include <Stiostream.h>
#include <math.h>
#include <stdlib.h>

#include "StSvtHybridNoise.hh"

ClassImp(StSvtHybridNoise)

StSvtHybridNoise::StSvtHybridNoise(int barrel, int ladder, int wafer, int hybrid):StSvtHybridPixelsC(barrel,ladder,wafer,hybrid)
{ }

//StSvtHybridNoise::~StSvtHybridNoise()
//{ }

double StSvtHybridNoise::makeGausDev(double sigma,double mean)
{

 static int iset = 0;
 static double v1,u;
 double rsq,v2;

 //if(*idum < 0) iset = 0;
  if(iset == 0)
   {
       
    do {
        v1 = 2.0*((float)rand()/(float)RAND_MAX) - 1.0;
        v2 = 2.0*((float)rand()/(float)RAND_MAX) - 1.0;
        rsq = v1*v1 + v2*v2;
        
    } while(rsq >= 1.0 || rsq == 0.0);

     u = ::sqrt(-2.0*::log(rsq)/rsq);

     iset = 1;
     //return mean + sigma*v2*u;
     return sigma*v2*u;  // sigma*::sqrt(-2.0*::log(rsq))*(v1/::sqrt(rsq))
   }
  else
    {
     iset = 0;
     // return mean + sigma*v1*u;
     return v1*sigma*u;
    }

 }

double StSvtHybridNoise::prob(double sigma,  double threshold)
{

 double num = 0, prob = 0;

   num = threshold/(M_SQRT2*sigma);
   prob = 0.5*(1. - erf(num));

   return prob;
 }

double StSvtHybridNoise::maxDistValue(double sigma ,double threshold)
{
 double num = 0, distValue = 0, coeff;

 num = threshold/(M_SQRT2*sigma);
 coeff = 1.0/(sigma*M_PI);

 distValue = coeff*exp(-num*num);

 return distValue;

 }

double StSvtHybridNoise::countAboveThreshold(double sigma, double randNum)
{

 double count = 0;

 count = sigma*::sqrt(2*M_PI)*randNum;
 count =sigma*::sqrt( -2.0*::log(count));

 return count;
 }
