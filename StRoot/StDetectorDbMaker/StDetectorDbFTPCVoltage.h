#ifndef StDetectorDbFTPCVoltage_h
#define StDetectorDbFTPCVoltage_h

#include "StMaker.h"


struct ftpcVoltage_st;

class StDetectorDbFTPCVoltage{
public:
    static StDetectorDbFTPCVoltage* instance();
    
        
    double getCathodeVEast();
    double getAnodeV1East();
    double getAnodeV2East();
    double getAnodeV3East();
    double getAnodeV4East();
    double getAnodeV5East();
    double getAnodeV6East();
    double getCathodeVWest();
    double getAnodeV1West();
    double getAnodeV2West();
    double getAnodeV3West();
    double getAnodeV4West();
    double getAnodeV5West();
    double getAnodeV6West();

    void update(StMaker*);
    
    friend ostream& operator<<(ostream& os, StDetectorDbFTPCVoltage& v);
    friend class nodbody; // For virtual ~
    
protected:
    ftpcVoltage_st*    mVoltage;
    
    StDetectorDbFTPCVoltage();
    virtual ~StDetectorDbFTPCVoltage();
private:
    static StDetectorDbFTPCVoltage* sInstance;
};


#endif
