#ifndef StDetectorDbClock_h
#define StDetectorDbClock_h

#include "StMaker.h"

struct starClockOnl_st;
#ifdef sun
#include <iostream.h>
#endif

#ifndef sun
class ostream;
#endif

class StDetectorDbClock{
public:
    static StDetectorDbClock*  instance();
    unsigned int               getRunNumber();
    unsigned int               getNumRows();
    double                     getCurrentFrequency();
    double                     getFrequency(unsigned int time = 0);
    bool                       getStatus(unsigned int time);
    friend ostream& operator<<(ostream& os, StDetectorDbClock& v);

    // These fuction will be public
    // but should be used only for debugging
    double                     getFrequencyEntry(unsigned int);
    unsigned int               getTimeEntry(unsigned int);
    void update(StMaker*);
    friend class nobody; // for virtual ~
    
protected:
    virtual ~StDetectorDbClock();
    StDetectorDbClock();
    starClockOnl_st * mStarClockOnl; // points to clock struct
    TTable* mTable; // points to table, need to re-intilize mStarClockOnl every event
    unsigned int mNumRows;
    StMaker * mMaker; // Holds pointer to maker
private:
    static StDetectorDbClock* sInstance;
};

#endif
