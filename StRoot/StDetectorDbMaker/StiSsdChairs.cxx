#include "StarChairDefs.h"
#include "St_db_Maker/St_db_Maker.h"
#include "StiSsdHitErrorCalculator.h"
MakeChairInstance2(HitError,StiSsdHitErrorCalculator,Calibrations/tracker/ssdHitError);
#include "StiSsdTrackingParameters.h"
MakeChairInstance2(TrackingParameters,StiSsdTrackingParameters,Calibrations/tracker/ssdTrackingParameters);
