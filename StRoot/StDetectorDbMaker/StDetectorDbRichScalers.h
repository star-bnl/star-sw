#ifndef StDetectorDbRichScalers_h
#define StDetectorDbRichScalers_h

#include <iostream.h>
#include "StMaker.h"

/*!
  This is a class that holds the RichScalers, which is probabaly a misnomer, as these are general STAR wide scalers, such as the CTB,ZDC and LO rates. These objects have been put in the database under richScalers so I am keeping the name.

  The getFunctions need to be passed a timestamp from the users, or it will assume the current event time.
*/

class StDetectorDbRichScalers{
public:
    StDetectorDbRichScalers(StMaker*);
    ~StDetectorDbRichScalers();
    void setRunNumber(unsigned int value);
    void setNumEntries(unsigned int value);
    void setTimes(unsigned int* value);
    void setCTBWest(double* value);
    void setCTBEast(double* value);
    void setCTBOr(double* value);
    void setTOFp(double* value);
    void setZDCWest(double* value);
    void setZDCEast(double* value);
    void setZDCX(double* value);
    void setMult(double* value);
    void setL0(double* value);
    void setEventTime(unsigned int value);
    
    unsigned int getRunNumber();
    unsigned int getNumEntries();
    unsigned int getEventTime();

    double getCTBWest(unsigned int time = 0);
    double getCTBEast(unsigned int time = 0);
    double getCTBOr(unsigned int time = 0);
    double getTOFp(unsigned int time = 0);
    double getZDCWest(unsigned int time = 0);
    double getZDCEast(unsigned int time= 0);
    double getZDCX(unsigned int time = 0);
    double getMult(unsigned int time = 0);
    double getL0(unsigned int time = 0);
    friend ostream& operator<<(ostream& os, StDetectorDbRichScalers& v);

private:
    double interpolateValue(unsigned int time,double* array);
    unsigned int mRunNumber;
    unsigned int mNumEntries;
    unsigned int* mTimes;
    unsigned int mEventTime;
    double* mCTBEast;
    double* mCTBWest;
    double* mCTBOR;
    double* mTOFp;
    double* mZDCEast;
    double* mZDCWest;
    double* mZDCX;
    double* mMult;
    double* mL0;
};
struct trigDetSums_st { 

   unsigned int  runNumber;   /*     run number  */
   unsigned int  timeOffset;   /*     run begin time  */
   double  ctbWest;   /* ctb West  */
   double  ctbEast;   /* ctb East  */
   double  ctbOR;   /* ctb Or */
   double  TOFp;   /*  TOFp; */
   double  ctbTOFp;   /* ctbTOFp  */
   double  zdcWest;   /*  zdc west */
   double  zdcEast;   /*  zdc east */
   double  zdcX;   /* zdc and  */
   double  mult;   /* mult  */
   double  L0;   /* L0 Rate  */

 };
#endif
