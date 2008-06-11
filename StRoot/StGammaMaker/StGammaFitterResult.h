// -*- mode: C++ -*- Put Emacs in C++ mode

//
// Pibero Djawotho <pibero@indiana.edu>
// Indiana University
// 1 May 2008
//
// StGammaFitterResult: container for result of gamma fitter.
//

#ifndef StGammaFitterResult_h
#define StGammaFitterResult_h

// ROOT
#include "TObject.h"

class StGammaFitterResult : public TObject {
public:
  float yield;
  float yieldError;
  float centroid;
  float centroidError;
  float residual;
  float mean;
  float rms;
  float chiSquare;
  int   ndf;
  float prob;

private:
  ClassDef(StGammaFitterResult,2);
};

#endif
