#include "StTrackDefinitions.h"
enum StTrackFindingMethod {
  kUndefinedFinderId         = kUndefinedFinderIdentifier,
  kSvtGrouperId              = kSvtGrouperIdentifier,
  kSvtStkId                  = kSvtStkIdentifier,
  kTpcStandardId             = kTpcStandardIdentifier,
  kSvtTpcSvmId               = kSvtTpcSvmIdentifier,
  kSvtTpcEstId               = kSvtTpcEstIdentifier
 };

enum StTrackQualityScheme {
  kUndefinedQualityId        = kUndefinedQualityIdentifier,
  kGrouperPassId             = kGrouperPassIdentifier,
  kStkPassId                 = kStkPassIdentifier,
  kSvmPassId                 = kSvmPassIdentifier,
  kEstPassId                 = kEstPassIdentifier
 };

enum StTrackFittingMethod {
  kUndefinedFitterId         = kUndefinedFitterIdentifier,
  kHelix2StepId              = kHelix2StepIdentifier,
  kHelix3DId                 = kHelix3DIdentifier,
  kKalmanFitId               = kKalmanFitIdentifier,
  kLine2StepId               = kLine2StepIdentifier,
  kLine3DId                  = kLine3DIdentifier
};
