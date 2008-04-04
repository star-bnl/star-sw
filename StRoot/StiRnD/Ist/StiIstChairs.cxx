#include "StarChairDefs.h"
#include "St_db_Maker/St_db_Maker.h"
#include "StiIst1HitErrorCalculator.h"
MakeChairInstance2(HitError,StiIst1HitErrorCalculator,Calibrations/tracker/ist1HitError);
#include "StiIst2HitErrorCalculator.h"
MakeChairInstance2(HitError,StiIst2HitErrorCalculator,Calibrations/tracker/ist2HitError);
#include "StiIst3HitErrorCalculator.h"
MakeChairInstance2(HitError,StiIst3HitErrorCalculator,Calibrations/tracker/ist3HitError);
