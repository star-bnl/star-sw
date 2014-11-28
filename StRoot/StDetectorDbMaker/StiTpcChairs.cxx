#include "StarChairDefs.h"
#include "St_db_Maker/St_db_Maker.h"
#include "StiTpcInnerHitErrorCalculator.h"
MakeChairInstance2(HitError,StiTpcInnerHitErrorCalculator,Calibrations/tracker/tpcInnerHitError);
#include "StiTpcOuterHitErrorCalculator.h"
MakeChairInstance2(HitError,StiTpcOuterHitErrorCalculator,Calibrations/tracker/tpcOuterHitError);
#include "StiTpcTrackingParameters.h"
MakeChairInstance2(TrackingParameters,StiTpcTrackingParameters,Calibrations/tracker/tpcTrackingParameters);
