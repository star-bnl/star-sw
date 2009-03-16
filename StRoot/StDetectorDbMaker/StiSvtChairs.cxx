#include "StarChairDefs.h"
#include "St_db_Maker/St_db_Maker.h"
#include "StiSvtHitErrorCalculator.h"
MakeChairInstance2(HitError,StiSvtHitErrorCalculator,Calibrations/tracker/svtHitError);
#include "StiSvtTrackingParameters.h"
MakeChairInstance2(TrackingParameters,StiSvtTrackingParameters,Calibrations/tracker/svtTrackingParameters);
