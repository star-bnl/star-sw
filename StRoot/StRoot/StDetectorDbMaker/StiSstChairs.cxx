#include "StarChairDefs.h"
#include "St_db_Maker/St_db_Maker.h"
#include "StiSstHitErrorCalculator.h"
MakeChairInstance2(HitError,StiSstHitErrorCalculator,Calibrations/tracker/sstHitError);
#include "StiSstTrackingParameters.h"
MakeChairInstance2(TrackingParameters,StiSstTrackingParameters,Calibrations/tracker/sstTrackingParameters);
