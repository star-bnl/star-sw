#ifndef __StvStarEdit_h_
#define __StvStarEdit_h_
#include "StarVMC/GeoTestMaker/StTGeoProxy.h"

// _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ __
class StvTpcEdit: public StActorFunctor
{
public:
    StvTpcEdit();
   ~StvTpcEdit(){}
int operator()(const double xyz[3]=0);
int VoluId();
private:

ClassDef(StvTpcEdit,0)
};
#endif
