#ifndef StDetectorDbFTPCGas_h
#define StDetectorDbFTPCGas_h

#include <iostream.h>
#include "StMaker.h"

class StDetectorDbFTPCGas{
public:
    StDetectorDbFTPCGas(StMaker*);
    ~StDetectorDbFTPCGas();
    void setRunNumber(unsigned int value);
    void setNumEntries(unsigned int value);
    void setTimes(unsigned int* value);
    void setEventTime(unsigned int value);

    void setWestO2ppm(double*);
    void setWestO2mv(double*);
    void setEastO2ppm(double*);
    void setEastO2mv(double*);
    void setExtO2ppm(double*);
    void setExtO2mv(double*);
    void setWestH2Odp(double*);
    void setEastH2Odp(double*);
    void setFlowAr(double*);
    void setFlowCO2(double*);
    
    unsigned int getRunNumber();
    unsigned int getNumEntries();
    unsigned int getEventTime();

    double getWestO2ppm(unsigned int time = 0);
    double getWestO2mv(unsigned int time = 0);
    double getEastO2ppm(unsigned int time = 0);
    double getEastO2mv(unsigned int time = 0);
    double getExtO2ppm(unsigned int time = 0);
    double getExtO2mv(unsigned int time = 0);
    double getWestH2Odp(unsigned int time = 0);
    double getEastH2Odp(unsigned int time = 0);
    double getFlowAr(unsigned int time = 0);
    double getFlowCO2(unsigned int time = 0);
    
    friend ostream& operator<<(ostream& os, StDetectorDbFTPCGas& v);

private:
    double interpolateValue(unsigned int time,double* array);
    unsigned int mRunNumber;
    unsigned int mNumEntries;
    unsigned int* mTimes;
    unsigned int mEventTime;
    double* mWestO2ppm;
    double* mWestO2mv;
    double* mEastO2ppm;
    double* mEastO2mv;
    double* mExtO2ppm;
    double* mExtO2mv;
    double* mWestH2Odp;
    double* mEastH2Odp;
    double* mFlowAr;
    double* mFlowCO2;
    
};

struct ftpcGasSystem_st{ 

   unsigned int  runNumber;   /*     run number  */
   unsigned int  timeOffset;   /*     eventtime  */
   double  westO2ppm;
   double  westO2mv; 
   double  eastO2ppm;
   double  eastO2mv; 
   double  extO2ppm; 
   double  extO2mv;  
   double  westH2Odp;
   double  eastH2Odp;
   double  flowAr;   
   double  flowCO2;  

 };
#endif
