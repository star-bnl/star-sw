#ifndef __StvTpcActive_h_
#define __StvTpcActive_h_
#include "StarVMC/GeoTestMaker/StTGeoProxy.h"

// _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ __
class StvTpcActive: public StActorFunctor
{
public:
    StvTpcActive();
   ~StvTpcActive(){}
int operator()(const double xyz[3]=0);
int VoluId();
private:
int mSector;
int mIsDet;
int mTPad;

ClassDef(StvTpcActive,0)
};

#endif
