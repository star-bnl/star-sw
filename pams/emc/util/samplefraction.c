/*:>------------------------------------------------------------------
**: FILE:       samplefraction.c
**: HISTORY:
**:             20-jan-99  Created by OGAWA, AKio
**:             10-feb-99  Corr. by Pavlinov, Aleksei
**:<------------------------------------------------------------------*/
#include <math.h>
#include "emc_def.h"
#include "samplefrac_def.h"

float samplefraction_(int *det, float *eta)
{
  float x;
  x = fabs(*eta);
  switch(*det)
  {
  case BEMC:
      return P0BEMC  + P1BEMC*x  + P2BEMC*x*x;
  case BPRS:
      return P0BPRS  + P1BPRS*x  + P2BPRS*x*x;
  case BSMDE: 
      return P0BSMDE + P1BSMDE*x + P2BSMDE*x*x;
  case BSMDP:
      return P0BSMDP + P1BSMDP*x + P2BSMDP*x*x;
  case EEMC: case EPRS: case ESMDE: case ESMDP:
  default:
    /*    puts("*** samplefraction: detector number is invalid"); */
    printf("*** samplefraction: detector number is invalid = %d \n",*det);
    return 0.0;
  }
}
