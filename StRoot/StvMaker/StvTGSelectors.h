#ifndef __StvTGSelectors_h_
#define __StvTGSelectors_h_
#include "StarVMC/GeoTestMaker/StTGeoProxy.h"

// _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ __
class StvTpcSelector: public StTGeoSele
{
public:
    StvTpcSelector(const char *name);
virtual ~StvTpcSelector(){}
virtual int Select(const char *path,int copyNumber, const float xyz[3]) const;
private:
int mInOut;

ClassDef(StvTpcSelector,0)
};

#endif
