///
/// \author Pibero Djawotho <pibero@indiana.edu>
/// \author Indiana University
/// \date 5 July 2007
///

#include "StGammaFitterResult.h"

ClassImp(StGammaFitterResult);

StGammaFitterResult::StGammaFitterResult()
{
  setYield(0);
  setMean(0);
  setResidual(0);
  setChi2(0);
  setNdf(0);
  setSigma(0);
}
