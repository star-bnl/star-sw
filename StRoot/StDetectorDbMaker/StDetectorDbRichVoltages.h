#ifndef StDetectorDbRichVoltages_h
#define StDetectorDbRichVoltages_h

#include <iostream.h>
#include "StMaker.h"

/*!
  This class holds the information of the RichVoltages.

  The user may pass in a timestamp, and the class will return the proper status for that time, or the event time will be used as a default
*/
class StDetectorDbRichVoltages{
public:
    StDetectorDbRichVoltages(StMaker*);
    ~StDetectorDbRichVoltages();

    void setEventTime(unsigned int value);
    void setRunNumber(unsigned int value);
    void setNumEntries(unsigned int value);
    void setStatusTimes(unsigned int* value);
    void setStatus(unsigned int* value);

    unsigned int getEventTime();
    unsigned int getRunNumber();
    unsigned int getNumEntries();
    unsigned int getStatus(unsigned int time = 0);

    friend ostream& operator<<(ostream& os, StDetectorDbRichVoltages& v);

protected:
    unsigned int mRunNumber;
    unsigned int mNumEntries;
    unsigned int mEventTime;
    unsigned int* mStatusTimes;
    unsigned int* mStatus;
        
};

struct richvoltages_st {
    unsigned int runNumber;
    unsigned int startStatusTime;
    unsigned int endStatusTime;
    unsigned int status;
};

#endif
