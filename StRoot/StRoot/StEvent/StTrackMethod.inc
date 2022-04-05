      INTEGER  kUndefinedFitterId,kHelix2StepId
      INTEGER  kHelix3DId,kKalmanFitId
      INTEGER  kLine2StepId,kLine3DId,kL3FitId

      INTEGER  svtGrouper, svtStk, svtOther, tpcStandard, tpcOther
      INTEGER  ftpcConformal, ftpcCurrent, svtTpcSvm, svtTpcEst
      INTEGER  svtTpcPattern, l3Standard

#include "StTrackDefinitions.h" 
* fitting method byte 0-3 (only fit method per track)
      PARAMETER (kUndefinedFitterId = kUndefinedFitterIdentifier)
      PARAMETER (kHelix2StepId = kHelix2StepIdentifier)
      PARAMETER (kHelix3DId = kHelix3DIdentifier)
      PARAMETER (kKalmanFitId = kKalmanFitIdentifier)
      PARAMETER (kLine2StepId = kLine2StepIdentifier)
      PARAMETER (kLine3DId = kLine3DIdentifier)
      PARAMETER (kL3FitId = kL3FitIdentifier)
* finding method bits (4-14) 
      PARAMETER (svtGrouper    = ksvtGrouperIdentifier)    
      PARAMETER (svtStk        = ksvtStkIdentifier)       
      PARAMETER (svtOther      = ksvtOtherIdentifier)     
      PARAMETER (tpcStandard   = ktpcStandardIdentifier)  
      PARAMETER (tpcOther      = ktpcOtherIdentifier)     
      PARAMETER (ftpcConformal = kftpcConformalIdentifier)
      PARAMETER (ftpcCurrent   = kftpcCurrentIdentifier)  
      PARAMETER (svtTpcSvm     = ksvtTpcSvmIdentifier)    
      PARAMETER (svtTpcEst     = ksvtTpcEstIdentifier)    
      PARAMETER (svtTpcPattern = ksvtTpcPatternIdentifier)
      PARAMETER (l3Standard    = kl3StandardIdentifier)  
