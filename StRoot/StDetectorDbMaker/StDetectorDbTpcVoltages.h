#ifndef StDetectorDbTpcVoltages_h
#define StDetectorDbTpcVoltages_h

#include "StMaker.h"

#ifdef sun
#include <iostream.h>
#endif

#ifndef sun
class ostream;
#endif

class TTable;
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
