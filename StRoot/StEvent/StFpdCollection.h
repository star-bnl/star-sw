/***************************************************************************
 *
 * $Id: StFpdCollection.h,v 2.1 2002/01/03 20:57:36 ullrich Exp $
 *
 * Author: Akio Ogawa, Jan 2002
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StFpdCollection.h,v $
 * Revision 2.1  2002/01/03 20:57:36  ullrich
 * Initial Revision.
 *
 **************************************************************************/
#ifndef StFpdCollection_hh
#define StFpdCollection_hh
#include "StObject.h"

class StFpdCollection : public StObject {
public:
    StFpdCollection();
    virtual ~StFpdCollection();
    // StFpdCollection(const StFpdCollection&);            use default
    // StFpdCollection& operator=(const StFpdCollection&); use default
    
    unsigned int    numberOfADC() const;
    unsigned int    numberOfTDC() const;
    unsigned int    numberOfRegisters() const;
    unsigned int    numberOfPedestal() const;
    
    unsigned short* adc();
    unsigned short* tdc();
    unsigned short  registers(unsigned int) const;
    unsigned short* pedestal();
    int             token() const;
    
    unsigned short  north(unsigned int);
    unsigned short  south(unsigned int);
    unsigned short  top(unsigned int);
    unsigned short  bottom(unsigned int);
    unsigned short  smdx(unsigned int);
    unsigned short  smdy(unsigned int);
    unsigned short  pres1(unsigned int);
    unsigned short  pres2(unsigned int);
    
    void setAdc(unsigned int, unsigned short);
    void setTdc(unsigned int, unsigned short);
    void setRegister(unsigned int, unsigned short);
    void setPedestal(unsigned int, unsigned short);
    
    void dump();
    
protected:
    enum {mMaxAdc = 256,
	  mMaxTdc = 8,
	  mMaxRegisters = 3,
	  mMaxPedestal = 256};
    
    UShort_t mAdc[mMaxAdc];
    UShort_t mTdc[mMaxTdc];
    UShort_t mReg[mMaxRegisters];
    UShort_t mPed[mMaxPedestal];
    
    ClassDef(StFpdCollection,1)
};

inline unsigned int
StFpdCollection::numberOfADC() const {return mMaxAdc;}          

inline unsigned int
StFpdCollection::numberOfTDC() const {return mMaxTdc;}          

inline unsigned int
StFpdCollection::numberOfRegisters() const {return mMaxRegisters;}

inline unsigned int
StFpdCollection::numberOfPedestal() const {return mMaxPedestal;}

inline int
StFpdCollection::token() const {return mReg[1];}

#endif
