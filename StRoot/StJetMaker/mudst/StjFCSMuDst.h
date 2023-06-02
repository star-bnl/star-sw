
#ifndef STJFCSMUDST_H
#define STJFCSMUDST_H

#include "StjFCS.h"

//FCS
class StFcsCollection;
class StMuFcsCollection;
class StFcsDbMaker;
class StMuDstMaker;
class StFcsDb;
class StMuFcsHit;

class StjFCSMuDst : public StjFCS {
    
public:
    StjFCSMuDst();
    virtual ~StjFCSMuDst() {}

    void setVertex(float vx, float vy, float vz)
    {
      _setVertex = true;
      _vx = vx;
      _vy = vy;
      _vz = vz;
    };
    void useECAL(){useECal = true;};
    void useHCAL(){useHCal = true;};
    
    StjTowerEnergyList getEnergyList();
    StjTowerEnergy hitenergyDeposit(const StMuFcsHit& hit);
    
private:
    
    StFcsDbMaker* mFcsDbMaker;
    StFcsCollection* mFcsColl;
    StMuFcsCollection* mMuFcsColl;
    StFcsDb* mFcsDb=0;
    
    bool _setVertex;
    
    double _vx;
    double _vy;
    double _vz;
    
    bool useECal;
    bool useHCal;
};

#endif //STJFCSMUDST_H
