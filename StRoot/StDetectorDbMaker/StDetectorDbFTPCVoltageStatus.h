#ifndef StDetectorDbFTPCVoltageStatus_h
#define StDetectorDbFTPCVoltageStatus_h

#include "StMaker.h"


struct ftpcVoltageStatus_st;

class StDetectorDbFTPCVoltageStatus{
public:
    static StDetectorDbFTPCVoltageStatus* instance();
    
        
    unsigned int  getStatusFTPCEast();
    unsigned int  getStatusFTPCWest();

    void update(StMaker*);
    
    friend ostream& operator<<(ostream& os, StDetectorDbFTPCVoltageStatus& v);
    friend class nodbody; // For virtual ~
    
protected:
    ftpcVoltageStatus_st*    mVoltageStatus;
    
    StDetectorDbFTPCVoltageStatus();
    virtual ~StDetectorDbFTPCVoltageStatus();
private:
    static StDetectorDbFTPCVoltageStatus* sInstance;
};


#endif
