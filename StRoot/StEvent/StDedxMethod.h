#ifndef StDedxMethod_hh
#define StDedxMethod_hh
#include "StDedxDefinitions.h"
enum StDedxMethod {
  kUndefinedMethodId         = kUndefinedMethodIdentifier,
  kTruncatedMeanId           = kTruncatedMeanIdentifier,
  kEnsembleTruncatedMeanId   = kEnsembleTruncatedMeanIdentifier,
  kLikelihoodFitId           = kLikelihoodFitIdentifier,
  kWeightedTruncatedMeanId   = kWeightedTruncatedMeanIdentifier,
  kOtherMethodId             = kOtherMethodIdentifier
};
#endif
