#include "StarChairDefs.h"
#include "St_db_Maker/St_db_Maker.h"
#include "StiPxlHitErrorCalculator.h"
MakeChairInstance2(HitError,StiPxlHitErrorCalculator,Calibrations/tracker/PixelHitError);
#include "StiPxlTrackingParameters.h"
MakeChairInstance2(TrackingParameters,StiPxlTrackingParameters,Calibrations/tracker/PixelTrackingParameters);
