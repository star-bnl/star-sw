/***************************************************************************
 *
 * $Id: StRHICfCollection.h,v 2.1 2018/12/11 19:52:21 ullrich Exp $
 *
 * Author: Akio Ogawa, Minho Kim
 ***************************************************************************
 *
 * RHICf data 
 *
 ***************************************************************************
 *
 * $Log: StRHICfCollection.h,v $
 * Revision 2.1  2018/12/11 19:52:21  ullrich
 * Initial Revision.
 *
 *
 **************************************************************************/
#ifndef StRHICfCollection_hh
#define StRHICfCollection_hh

#include "Stiostream.h"
#include "StObject.h"
#include "StContainers.h"
#include "StEnumerations.h"

class StRHICfCollection : public StObject {
public:
    StRHICfCollection();
    ~StRHICfCollection();
    
    void reset();
    
    unsigned int localRunNumber() const;
    unsigned int localEventNumber() const;
    unsigned int trgm() const;
    float        plateE  (unsigned int tower, unsigned int plate);
    float        barE  (unsigned int tower, unsigned int layer, unsigned int xy, unsigned int bar);
    unsigned cad (unsigned int cadnum);
    unsigned tdc (unsigned int tdcnum);
    unsigned gpio(unsigned int gpionum);
    
    void setLocalRunNumber(unsigned int);
    void setLocalEventNumber(unsigned int);
    void setTrgm(unsigned);
    void setPlateE(unsigned int tower, unsigned int plate, float E);
    void setBarE(unsigned int tower, unsigned int layer, unsigned int xy, unsigned int bar, float E);
    void setCad(unsigned int cadnum, unsigned int cad);
    void setTdc(unsigned int tdcnum, unsigned int tdc);
    void setGpio(unsigned int gpionum, unsigned int gpio);
    
    void print(int option=0);
    
private:
    bool check(unsigned int tower, unsigned int plate);
    bool check(unsigned int tower, unsigned int layer, unsigned int xy, unsigned int bar);
    bool checkCad(unsigned int num);
    bool checkTdc(unsigned int num);
    bool checkGpio(unsigned int num);
    
protected:
    UInt_t   mLocalRunNumber;
    UInt_t   mLocalEventNumber;
    UInt_t   mTrgm;
    float          mPlateE         [kRHICfNtower][kRHICfNplate];
    float          mBarE           [kRHICfNtower][kRHICfNlayer][kRHICfNxy][kRHICfNbarLarge];
    unsigned int   mCad            [kRHICfNcad];
    unsigned int   mTdc            [kRHICfNtdc];
    unsigned int   mGpio           [kRHICfNgpio];

	ClassDef(StRHICfCollection,1)
};

#endif
