/// -*- mode: C++ -*-

///
/// \author Pibero Djawotho <pibero@indiana.edu>
/// \author Indiana University
/// \date 5 July 2007
///
/// \class StGammaFitterResult
/// \brief This class holds the result of the gamma fitter.
/// \see StGammaFitter
///

#ifndef StGammaFitterResult_h
#define StGammaFitterResult_h

#include "TObject.h"

class StGammaFitterResult : public TObject {
public:
  StGammaFitterResult() {}
  ~StGammaFitterResult() {}

  float yield() const;
  float mean() const;
  float residual() const;
  float chi2() const;
  int ndf() const;
  float sigma() const;

  void setYield(float yield);
  void setMean(float mean);
  void setResidual(float residual);
  void setChi2(float chi2);
  void setNdf(int ndf);
  void setSigma(float sigma);

private:
  float mYield;
  float mMean;
  float mResidual;
  float mChi2;
  int mNdf;
  float mSigma;

  ClassDef(StGammaFitterResult, 1);
};

inline float StGammaFitterResult::yield() const { return mYield; }
inline float StGammaFitterResult::mean() const { return mMean; }
inline float StGammaFitterResult::residual() const { return mResidual; }
inline float StGammaFitterResult::chi2() const { return mChi2; }
inline int StGammaFitterResult::ndf() const { return mNdf; }
inline float StGammaFitterResult::sigma() const { return mSigma; }

inline void StGammaFitterResult::setYield(float yield) { mYield = yield; }
inline void StGammaFitterResult::setMean(float mean) { mMean = mean; }
inline void StGammaFitterResult::setResidual(float residual) { mResidual = residual; }
inline void StGammaFitterResult::setChi2(float chi2) { mChi2 = chi2; }
inline void StGammaFitterResult::setNdf(int ndf) { mNdf = ndf; }
inline void StGammaFitterResult::setSigma(float sigma) { mSigma = sigma; }

#endif
