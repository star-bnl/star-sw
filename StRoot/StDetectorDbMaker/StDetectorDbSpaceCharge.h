#ifndef StDetectorDbSpaceCharge_h
#define StDetectorDbSpaceCharge_h
#include <Stiostream.h>

#include "StMaker.h"


class  TTable;
struct spaceChargeCor_st;

class StDetectorDbSpaceCharge{
public:
    static StDetectorDbSpaceCharge*  instance();
    double                     getSpaceChargeCorrection();
    double                     getSpaceChargeCorrection(double scaleFactor);
    double                     getSpaceChargeSatRate();
    double                     getSpaceChargeCoulombs();
    double                     getSpaceChargeCoulombs(double scaleFactor);
    friend ostream& operator<<(ostream& os, StDetectorDbSpaceCharge& v);

    // These fuction will be public
    // but should be used only for debugging
    void update(StMaker*);
    friend class nobody; // for virtual ~
    
protected:
    virtual ~StDetectorDbSpaceCharge();
    StDetectorDbSpaceCharge();
    spaceChargeCor_st * mSpaceCharge; // points to clock struct
    TTable* mTable; // points to table, need to re-intilize mStarClockOnl every event
    StMaker * mMaker; // Holds pointer to maker
private:
    static StDetectorDbSpaceCharge* sInstance;
};

#endif
