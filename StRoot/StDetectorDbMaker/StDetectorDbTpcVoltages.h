#ifndef StDetectorDbTpcVoltages_h
#define StDetectorDbTpcVoltages_h

#include <iostream.h>
#include "StMaker.h"
#include "tables/St_tpcHighVoltages_Table.h"

struct tpcHighVoltages_st;

class StDetectorDbTpcVoltages{
public:
    static StDetectorDbTpcVoltages* instance();
    
    double getCathodeVoltage();
    double getGGVoltage();
    

    void update(StMaker*);
    friend class nobody; // for virtual ~
    friend ostream& operator<<(ostream& os, StDetectorDbTpcVoltages& v);

protected:
    virtual ~StDetectorDbTpcVoltages();
    StDetectorDbTpcVoltages();
    tpcHighVoltages_st* mTpcVoltages; // Vector To hold voltages
    TTable* mTable; // Holds the TTable
private:
    static StDetectorDbTpcVoltages* sInstance;
};



#endif
