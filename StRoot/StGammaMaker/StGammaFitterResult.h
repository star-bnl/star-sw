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
  StGammaFitterResult();

  void Clear(Option_t* option = "");

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

  virtual const char* GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StGammaFitterResult.h,v 1.6 2008/06/30 14:58:40 jwebb Exp $ built "__DATE__" "__TIME__; return cvs;}

private:
  ClassDef(StGammaFitterResult,2);
};

#endif
