#ifndef __StvStarEdit_h_
#define __StvStarEdit_h_
#include "StarVMC/GeoTestMaker/StTGeoHelper.h"

// _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ __
class StvTpcEdit: public StActiveFunctor
{
public:
    StvTpcEdit();
   ~StvTpcEdit(){}
int operator()(const double xyz[3]);
int VoluId();
private:

ClassDef(StvTpcEdit,0)
};

// _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ __
class StvTpcLayer: public StActiveFunctor
{
public:
    StvTpcLayer();
   ~StvTpcLayer(){}
int operator()(const double xyz[3]);
private:

ClassDef(StvTpcLayer,0)
};
#endif
