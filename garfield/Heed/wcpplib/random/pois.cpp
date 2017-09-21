#include <cmath>

#include "wcpplib/random/pois.h"
#include "wcpplib/random/rnorm.h"
#include "wcpplib/random/ranluxint.h"

namespace Heed {

long pois(const double amu, int &ierror) {
  // C
  // C    POISSON GENERATOR
  // C    CODED FROM LOS ALAMOS REPORT      LA-5061-MS
  // C    PROB(N)=EXP(-AMU)*AMU**N/FACT(N)
  // C        WHERE FACT(N) STANDS FOR FACTORIAL OF N
  // C    ON RETURN IERROR.EQ.0 NORMALLY
  // C              IERROR.EQ.1 IF AMU.LE.0.
  // C
  double AMUOL = -1.;
  double AMAX = 100.;
  double EXPMA = 0.;
  double PIR = 0;
  long N = 0;
  ierror = 0;
  if (amu > AMAX) goto m500;
  if (amu == AMUOL) goto m200;
  if (amu > 0.0) goto m100;
  // C    MEAN SHOULD BE POSITIVE
  ierror = 1;
  return 0;
// C    SAVE EXPONENTIAL FOR FURTHER IDENTICAL REQUESTS
m100:
  ierror = 0;
  AMUOL = amu;
  EXPMA = exp(-amu);
m200:
  PIR = 1.;
  N = -1;
m300:
  N = N + 1;
  PIR = PIR * SRANLUX();
  if (PIR > EXPMA) goto m300;
  return N;
// C   NORMAL APPROXIMATION FOR AMU.GT.AMAX
m500:
  const double RAN = rnorm_improved();
  N = long(RAN * sqrt(amu) + amu + .5);
  return N;
}
}
