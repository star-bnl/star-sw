#include "StDedxDefinitions.h"
enum StDedxMethod {
  kUndefinedMethodId         = kUndefinedMethodIdentifier,
  kTruncatedMeanId           = kTruncatedMeanIdentifier,
  kEnsembleTruncatedMeanId   = kEnsembleTruncatedMeanIdentifier,
  kLikelihoodFitId           = LikelihoodFitIdentifier,
  kWeightedTruncatedMeanId   = kWeightedTruncatedMeanIdentifier,
  kOtherMethodId             = kOtherMethodIdentifier
};
