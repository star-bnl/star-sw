#ifndef StDetectorDbTriggerID_h
#define StDetectorDbTriggerID_h

#include "StMaker.h"

struct triggerID_st;

#ifdef sun
#include <iostream.h>
#endif

#ifndef sun
class ostream;
#endif

class StDetectorDbTriggerID{
public:
    static StDetectorDbTriggerID*  instance();
    unsigned int               getRunNumber();
    unsigned int               getNumRows();
    unsigned int               getIdxTrg(unsigned int entry = 0);
    unsigned int               getDaqTrgId(unsigned int entry = 0);
    unsigned int               getOfflineTrgId(unsigned int entry = 0);
    unsigned int               getTrgNameVersion(unsigned int entry = 0);
    unsigned int               getTrgVersion(unsigned int entry = 0);
    unsigned int               getThreashVersion(unsigned int entry = 0);
    unsigned int               getPsVersion(unsigned int entry = 0);
    friend ostream& operator<<(ostream& os, StDetectorDbTriggerID& v);

    // These fuction will be public
    // but should be used only for debugging
    void update(StMaker*);
    friend class nobody; // for virtual ~
    
protected:
    virtual ~StDetectorDbTriggerID();
    StDetectorDbTriggerID();
    triggerID_st * mTriggerID; // points to trigger struct
    TTable* mTable; // points to table, need to re-intilize mTriggerID every event
    unsigned int mNumRows;
    StMaker * mMaker; // Holds pointer to maker
private:
    static StDetectorDbTriggerID* sInstance;
};

#endif
