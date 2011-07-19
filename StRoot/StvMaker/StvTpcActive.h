#ifndef __StvTpcActive_h_
#define __StvTpcActive_h_
#include "StarVMC/GeoTestMaker/StTGeoHelper.h"

// _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ __
class StvTpcActive: public StActiveFunctor
{
public:
    StvTpcActive();
   ~StvTpcActive(){}
int operator()(const double xyz[3]);
int VoluId();
private:

ClassDef(StvTpcActive,0)
};

#endif
