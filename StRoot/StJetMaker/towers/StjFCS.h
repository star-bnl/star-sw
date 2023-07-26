#ifndef STJFCS_H
#define STJFCS_H

#include <TObject.h>

#include "StjTowerEnergyList.h"

class StjFCS : public TObject {
    
public:
    StjFCS() {}
    virtual ~StjFCS() {}
    
    virtual void Init() {}
    
    virtual StjTowerEnergyList getEnergyList() = 0;
    
    ClassDef(StjFCS, 1)
};

#endif //STJFCS_H
