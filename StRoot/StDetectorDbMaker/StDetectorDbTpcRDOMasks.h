#ifndef StDetectorDbTpcRDOMasks_h
#define StDetectorDbTpcRDOMasks_h

#include <iostream.h>
#include "StMaker.h"

class StDetectorDbTpcRDOMasks{
public:
    StDetectorDbTpcRDOMasks(StMaker*);
    ~StDetectorDbTpcRDOMasks();

    void setRunNumber(unsigned int value);
    void setNumEntries(unsigned int value);
    void setSectors(unsigned int* value);
    void setMasks(unsigned int* value);

    unsigned int getRunNumber();
    unsigned int getNumEntries();
    bool isOn(unsigned int sector,unsigned int rdo);
    unsigned int getSectorMask(unsigned int sector);

    friend ostream& operator<<(ostream& os, StDetectorDbTpcRDOMasks& v);

protected:
    unsigned int mRunNumber;
    unsigned int mNumEntries;
    unsigned int* mSectors;  // Goes 1-12, where 1 has TPC sector 1,2; 2 has sectors 3,4
    unsigned int* mMasks;
        
};

struct tpcRDOMasks_st {
    unsigned int runNumber;
    unsigned int sector;
    unsigned int mask;
};

#endif
