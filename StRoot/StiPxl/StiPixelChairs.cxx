#include "StarChairDefs.h"
#include "St_db_Maker/St_db_Maker.h"
#include "StiPixelHitErrorCalculator.h"
MakeChairInstance2(HitError,StiPixelHitErrorCalculator,Calibrations/tracker/PixelHitError);
#include "StiPixelTrackingParameters.h"
MakeChairInstance2(TrackingParameters,StiPixelTrackingParameters,Calibrations/tracker/PixelTrackingParameters);
