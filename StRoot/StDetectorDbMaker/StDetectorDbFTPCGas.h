#ifndef StDetectorDbFTPCGas_h
#define StDetectorDbFTPCGas_h

#include "StMaker.h"


struct ftpcGasSystem_st;
struct ftpcGasOut_st;
struct tpcGas_st;

class StDetectorDbFTPCGas{
public:
    static StDetectorDbFTPCGas* instance();
    
        
    double getWestO2ppm();
    double getWestO2mv();
    double getEastO2ppm();
    double getEastO2mv();
    double getExtO2ppm();
    double getExtO2mv();
    double getWestH2Odp();
    double getEastH2Odp();
    double getFlowAr();
    double getFlowCO2();
    double getGasOutEast();
    double getBody1East();
    double getBody2East();
    double getBody3East();
    double getBody4East();
    double getBody5East();
    double getBody6East();
    double getGasOutWest();
    double getBody1West();
    double getBody2West();
    double getBody3West();
    double getBody4West();
    double getBody5West();
    double getBody6West();
    double getBarometricPressure();

    void update(StMaker*);
    
    friend ostream& operator<<(ostream& os, StDetectorDbFTPCGas& v);
    friend class nodbody; // For virtual ~
    
protected:
    ftpcGasSystem_st* mGasSystem;
    ftpcGasOut_st*    mGasOut;
    tpcGas_st*        mTpcGas;
    
    StDetectorDbFTPCGas();
    virtual ~StDetectorDbFTPCGas();
private:
    static StDetectorDbFTPCGas* sInstance;
};


#endif
