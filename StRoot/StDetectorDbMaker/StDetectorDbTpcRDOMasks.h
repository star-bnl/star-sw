#ifndef StDetectorDbTpcRDOMasks_h
#define StDetectorDbTpcRDOMasks_h

#include "StMaker.h"


class TTable;
struct tpcRDOMasks_st;

class StDetectorDbTpcRDOMasks{
public:
    static StDetectorDbTpcRDOMasks* instance();
    
    bool isOn(unsigned int sector,unsigned int rdo);
    unsigned int getSectorMask(unsigned int sector);

    void update(StMaker*);
    friend class nobody; // for virtual ~
    friend ostream& operator<<(ostream& os, StDetectorDbTpcRDOMasks& v);

protected:
    virtual ~StDetectorDbTpcRDOMasks();
    unsigned int mNumEntries;
    StDetectorDbTpcRDOMasks();
    tpcRDOMasks_st* mMaskVector; // Vector To hold RDO Masks
    TTable* mTable; // Holds the TTable
private:
    static StDetectorDbTpcRDOMasks* sInstance;
};



#endif
