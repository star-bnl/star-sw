// -*- mode: C++ -*-

//
// Pibero Djawotho <pibero@indiana.edu>
// Indiana University
// 28 July 2007
//
// StGammaFitterResult: container for result of gamma fitter.
//

#ifndef StGammaFitterResult_h
#define StGammaFitterResult_h

#include "TObject.h"

class StGammaFitterResult : public TObject {
public:
  StGammaFitterResult();
  ~StGammaFitterResult() {}

  float yield() const;
  float yieldError() const;
  float uyield() const;
  float uyieldError() const;
  float vyield() const;
  float vyieldError() const;
  float umean() const;
  float umeanError() const;
  float vmean() const;
  float vmeanError() const;
  float uresidual() const;
  float vresidual() const;
  float usigma() const;
  float vsigma() const;
  float chiSquare() const;
  int ndf() const;
  float likelihood() const;	// Scott's variable

  void setYield(float yield, float error = 0);
  void setUmean(float mean, float error = 0);
  void setVmean(float mean, float error = 0);
  void setUresidual(float residual);
  void setVresidual(float residual);
  void setUsigma(float sigma);
  void setVsigma(float sigma);
  void setChiSquare(float chiSquare);
  void setNdf(int ndf);
  void setLikelihood(float likelihood);
  void print();

private:
  float mYield;
  float mYieldError;
  float mUmean;
  float mUmeanError;
  float mVmean;
  float mVmeanError;
  float mUresidual;
  float mVresidual;
  float mUsigma;
  float mVsigma;
  float mChiSquare;
  int mNdf;
  float mLikelihood;

  ClassDef(StGammaFitterResult, 1);
};

ostream& operator<<(ostream& out, const StGammaFitterResult& fit);

inline float StGammaFitterResult::yield() const { return mYield; }
inline float StGammaFitterResult::yieldError() const { return mYieldError; }
inline float StGammaFitterResult::uyield() const { return mYield; }
inline float StGammaFitterResult::uyieldError() const { return mYieldError; }
inline float StGammaFitterResult::vyield() const { return mYield; }
inline float StGammaFitterResult::vyieldError() const { return mYieldError; }
inline float StGammaFitterResult::umean() const { return mUmean; }
inline float StGammaFitterResult::umeanError() const { return mUmeanError; }
inline float StGammaFitterResult::vmean() const { return mVmean; }
inline float StGammaFitterResult::vmeanError() const { return mVmeanError; }
inline float StGammaFitterResult::uresidual() const { return mUresidual; }
inline float StGammaFitterResult::vresidual() const { return mVresidual; }
inline float StGammaFitterResult::usigma() const { return mUsigma; }
inline float StGammaFitterResult::vsigma() const { return mVsigma; }
inline float StGammaFitterResult::chiSquare() const { return mChiSquare; }
inline int StGammaFitterResult::ndf() const { return mNdf; }
inline float StGammaFitterResult::likelihood() const { return mLikelihood; }

inline void StGammaFitterResult::setYield(float yield, float error) { mYield = yield; mYieldError = error; }
inline void StGammaFitterResult::setUmean(float mean, float error) { mUmean = mean; mUmeanError = error; }
inline void StGammaFitterResult::setVmean(float mean, float error) { mVmean = mean; mVmeanError = error; }
inline void StGammaFitterResult::setUresidual(float residual) { mUresidual = residual; }
inline void StGammaFitterResult::setVresidual(float residual) { mVresidual = residual; }
inline void StGammaFitterResult::setUsigma(float sigma) { mUsigma = sigma; }
inline void StGammaFitterResult::setVsigma(float sigma) { mVsigma = sigma; }
inline void StGammaFitterResult::setChiSquare(float chiSquare) { mChiSquare = chiSquare; }
inline void StGammaFitterResult::setNdf(int ndf) { mNdf = ndf; }
inline void StGammaFitterResult::setLikelihood(float likelihood) { mLikelihood = likelihood; }

#endif
