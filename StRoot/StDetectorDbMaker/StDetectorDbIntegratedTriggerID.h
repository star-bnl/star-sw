#ifndef StDetectorDbIntegratedTriggerID_h
#define StDetectorDbIntegratedTriggerID_h

#include "StMaker.h"

struct triggerInfo_st;
struct defaultTrgLvl_st;

#ifdef sun
#include <iostream.h>
#endif

#ifndef sun
class ostream;
#endif

class StDetectorDbIntegratedTriggerID{
public:
    static StDetectorDbIntegratedTriggerID*  instance();
    int                        getIDRunNumber();
    unsigned int               getIDNumRows();
    int                        getIdxTrg(unsigned int entry = 0);
    int                        getDaqTrgId(unsigned int entry = 0);
    int                        getOfflineTrgId(unsigned int entry = 0);
    int                        getTrgNameVersion(unsigned int entry = 0);
    int                        getTrgVersion(unsigned int entry = 0);
    int                        getThreashVersion(unsigned int entry = 0);
    int                        getPsVersion(unsigned int entry = 0);
    int                        getPsL0(unsigned int entry = 0);
    char*                      getName(unsigned int entry = 0);
    unsigned int               getDetectorLiveOnBits(unsigned int entry = 0);
    unsigned int               getDetectorLiveOffBits(unsigned int entry = 0);
    unsigned int               getDetectorRequest(unsigned int entry = 0);
    int                        getIdxLevel(unsigned int entry = 0);
    int                        getAlgorithmId(unsigned int entry = 0);
    float                      getPs(unsigned int entry = 0);



    unsigned int              getDefaultTriggerLevel();
    
    friend ostream& operator<<(ostream& os, StDetectorDbIntegratedTriggerID& v);

    // These fuction will be public
    // but should be used only for debugging
    void update(StMaker*);
    friend class nobody; // for virtual ~
    
protected:
// members of triggerInfo 
    virtual ~StDetectorDbIntegratedTriggerID();
    StDetectorDbIntegratedTriggerID();
    triggerInfo_st* mTriggerInfo; // points to triggerID struct
    TTable* mIDTable; // points to table, need to re-intilize mTriggerID every event
    unsigned int mIDNumRows;

// members of defaultTrgLvl 
    defaultTrgLvl_st* mDefaultTriggerLevel; // points to prescales struct
    TTable* mDefTrgLvlTable; // points to table, need to re-intilize mDefaultTriggerLevel every event
    
    StMaker* mMaker; // Holds pointer to maker
private:
    static StDetectorDbIntegratedTriggerID* sInstance;
};

#endif
