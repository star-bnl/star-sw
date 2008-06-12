//
// Pibero Djawotho <pibero@indiana.edu>
// Indiana University
// 1 May 2008
//

#include "StGammaFitterResult.h"

ClassImp(StGammaFitterResult);

StGammaFitterResult::StGammaFitterResult()
{
  Clear();
}

void StGammaFitterResult::Clear(Option_t* option)
{
  yield = 0;
  yieldError = 0;
  centroid = 0;
  centroidError = 0;
  residual = 0;
  mean = 0;
  rms = 0;
  chiSquare = 0;
  ndf = 0;
  prob = 0;
}
