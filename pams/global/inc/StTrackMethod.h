#ifndef StTrackMethod_hh
#define StTrackMethod_hh
#include "StTrackDefinitions.h"

enum StTrackFittingMethod {
  kUndefinedFitterId         = kUndefinedFitterIdentifier,
  kHelix2StepId              = kHelix2StepIdentifier,
  kHelix3DId                 = kHelix3DIdentifier,
  kKalmanFitId               = kKalmanFitIdentifier,
  kLine2StepId               = kLine2StepIdentifier,
  kLine3DId                  = kLine3DIdentifier,
  kL3FitId                   = kL3FitIdentifier
};
#endif
