#ifndef StDetectorDbTriggerID_h
#define StDetectorDbTriggerID_h

#include "StMaker.h"

struct triggerID_st;
struct trigPrescales_st;
struct L0TriggerInfo_st;
struct defaultTrgLvl_st;

#ifdef sun
#include <iostream.h>
#endif

#ifndef sun
class ostream;
#endif

class StDetectorDbTriggerID{
public:
    static StDetectorDbTriggerID*  instance();
    unsigned int               getIDRunNumber();
    unsigned int               getIDNumRows();
    unsigned int               getIdxTrg(unsigned int entry = 0);
    unsigned int               getDaqTrgId(unsigned int entry = 0);
    unsigned int               getOfflineTrgId(unsigned int entry = 0);
    unsigned int               getTrgNameVersion(unsigned int entry = 0);
    unsigned int               getTrgVersion(unsigned int entry = 0);
    unsigned int               getThreashVersion(unsigned int entry = 0);
    unsigned int               getPsVersion(unsigned int entry = 0);

    int                        getSRunNumber();
    unsigned int               getSNumRows();
    int                        getIdxTrigger(unsigned int entry = 0);
    int                        getIdxLevel(unsigned int entry = 0);
    int                        getId(unsigned int entry = 0);
    float                      getPs(unsigned int entry = 0);


    int                        getL0RunNumber();
    unsigned int               getL0NumRows();
    int                        getL0DaqTrgId(unsigned int entry = 0);
    int                        getL0OfflineTrgId(unsigned int entry = 0);
    int                        getPsL0(unsigned int entry = 0);
    char*                      getName(unsigned int entry = 0);
    unsigned int               getDetectorLiveOnBits(unsigned int entry = 0);
    unsigned int               getDetectorLiveOffBits(unsigned int entry = 0);
    unsigned int               getDetectorRequest(unsigned int entry = 0);

    unsigned int              getDefaultTriggerLevel();
    
    friend ostream& operator<<(ostream& os, StDetectorDbTriggerID& v);

    // These fuction will be public
    // but should be used only for debugging
    void update(StMaker*);
    friend class nobody; // for virtual ~
    
protected:
// members of triggerID 
    virtual ~StDetectorDbTriggerID();
    StDetectorDbTriggerID();
    triggerID_st* mTriggerID; // points to triggerID struct
    TTable* mIDTable; // points to table, need to re-intilize mTriggerID every event
    unsigned int mIDNumRows;

// members of trigPrescales
    trigPrescales_st* mTrigPrescales; // points to prescales struct
    TTable* mSTable; // points to table, need to re-intilize mTrigPrescales every event
    unsigned int mSNumRows;

// members of L0TriggerInfo
    L0TriggerInfo_st* mL0TriggerInfo; // points to L0TriggerInfo struct
    TTable* mL0Table; // points to table, need to re-intilize mL0TriggerInfo every event
    unsigned int mL0NumRows;

// members of defaultTrgLvl 
    defaultTrgLvl_st* mDefaultTriggerLevel; // points to prescales struct
    TTable* mDefTrgLvlTable; // points to table, need to re-intilize mDefaultTriggerLevel every event
    
    StMaker* mMaker; // Holds pointer to maker
private:
    static StDetectorDbTriggerID* sInstance;
};

#endif
