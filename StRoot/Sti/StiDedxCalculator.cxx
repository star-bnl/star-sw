#include "StiDedxCalculator.h"

ClassImp(StiDedxCalculator)


StiDedxCalculator::StiDedxCalculator()
{
  setDefaults();
}
  
void  StiDedxCalculator::setDefaults()
{
  setRange(0.1,0.7);
}

float StiDedxCalculator::getDedx(int nSamples, float * samples)
{
  int   i;
  int   swap;
  float tmp;

  // simple sort of the samples in incresing order
  do {
    swap=0;
    for (i=0; i<nSamples-1; i++) 
      {
	if (samples[i]<=samples[i+1]) 
	  continue;
	tmp          = samples[i];
	samples[i]   = samples[i+1]; 
	samples[i+1] = tmp;
	swap++;
      }
  } while (swap);

  int nLow  = int(low*nSamples);
  int nHigh = int(high*nSamples);
  
  float dedx = 0;
  for (i=nLow; i<=nHigh; i++) 
    dedx += samples[i];
  dedx /= (nHigh-nLow+1);
  return dedx;
}
