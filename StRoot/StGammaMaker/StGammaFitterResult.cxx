//
// Pibero Djawotho <pibero@indiana.edu>
// Indiana University
// 28 July 2007
//

#include <iostream>
#include "StGammaFitterResult.h"

ClassImp(StGammaFitterResult);

StGammaFitterResult::StGammaFitterResult()
{
  setYield(0);
  setUmean(0);
  setVmean(0);
  setUresidual(0);
  setVresidual(0);
  setUsigma(0);
  setVsigma(0);
  setChiSquare(0);
  setNdf(0);
  setLikelihood(0);
}

void StGammaFitterResult::print()
{
  std::cout << *this << '\n';
}

ostream& operator<<(ostream& out, const StGammaFitterResult& fit)
{
  out <<   "yield=" << fit.yield() << "+/-" << fit.yieldError()
      << ", umean=" << fit.umean() << "+/-" << fit.umeanError()
      << ", vmean=" << fit.vmean() << "+/-" << fit.vmeanError()
      << ", uresidual=" << fit.uresidual()
      << ", vresidual=" << fit.vresidual()
      << ", usigma=" << fit.usigma()
      << ", vsigma=" << fit.vsigma()
      << ", chi2/ndf=" << fit.chiSquare() / fit.ndf()
      << ", likelihood=" << fit.likelihood();
  return out;
}
