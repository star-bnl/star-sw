/*!
 * \class StFpdCollection 
 * \author Akio Ogawa, Jan 2002
 */
/***************************************************************************
 *
 * $Id: StFpdCollection.h,v 2.5 2002/09/25 14:04:17 akio Exp $
 *
 * Author: Akio Ogawa, Jan 2002
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StFpdCollection.h,v $
 * Revision 2.5  2002/09/25 14:04:17  akio
 * Bug fix in the service functions, no change in data
 *
 * Revision 2.4  2002/02/22 22:56:48  jeromel
 * Doxygen basic documentation in all header files. None of this is required
 * for QM production.
 *
 * Revision 2.3  2002/01/17 18:38:11  ullrich
 * Several new methods added. Bug fixed.
 *
 * Revision 2.2  2002/01/09 15:37:55  ullrich
 * AdcSum functions and scaler infos added.
 *
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
    unsigned int    numberOfPMTpEEMC() const;
    unsigned int    numberOfPMTPbg() const;
    unsigned int    numberOfPMTSmdX() const;
    unsigned int    numberOfPMTSmdY() const;
    unsigned int    numberOfScalers() const;
    
    unsigned short*       adc();
    const unsigned short* adc() const;
    unsigned short*       tdc();
    const unsigned short* tdc() const;
    unsigned short        registers(unsigned int) const;
    unsigned short*       pedestal();
    const unsigned short* pedestal() const;
    unsigned int          scaler(unsigned int) const;

    unsigned short  north(unsigned int) const;
    unsigned short  south(unsigned int) const;
    unsigned short  top(unsigned int) const;
    unsigned short  bottom(unsigned int) const;
    unsigned short  smdx(unsigned int) const;
    unsigned short  smdy(unsigned int) const;
    unsigned short  pres1(unsigned int) const;
    unsigned short  pres2(unsigned int) const;
    unsigned short  southVeto() const;

    unsigned int sumAdcNorth() const;
    unsigned int sumAdcSouth() const;
    unsigned int sumAdcTop() const;
    unsigned int sumAdcBottom() const;
    unsigned int sumAdcPreShower1() const;
    unsigned int sumAdcPreShower2() const;
    unsigned int sumAdcSmdX() const;
    unsigned int sumAdcSmdY() const;
    
    void setAdc(unsigned int, unsigned short);
    void setTdc(unsigned int, unsigned short);
    void setRegister(unsigned int, unsigned short);
    void setPedestal(unsigned int, unsigned short);
    void setScaler(unsigned int, unsigned int);

    unsigned int token() const;
    void setToken(unsigned int);

    void dump();
    
protected:
    enum {mMaxAdc = 256,
	  mMaxTdc = 8,
	  mMaxRegisters = 3,
	  mMaxPedestal = 256,
          mMaxPMTpEEMC = 12,
          mMaxPMTPbg   = 16,
          mMaxPMTSmdX  = 60,
          mMaxPMTSmdY  = 100,
          mMaxScalers  = 128};
    
    UShort_t mAdc[mMaxAdc];
    UShort_t mTdc[mMaxTdc];
    UShort_t mReg[mMaxRegisters];
    UShort_t mPed[mMaxPedestal];
    UInt_t   mScl[mMaxScalers];
    UShort_t mToken;
  
    ClassDef(StFpdCollection,2)
};

inline unsigned int
StFpdCollection::numberOfADC() const {return mMaxAdc;}          

inline unsigned int
StFpdCollection::numberOfTDC() const {return mMaxTdc;}          

inline unsigned int
StFpdCollection::numberOfRegisters() const {return mMaxRegisters;}

inline unsigned int
StFpdCollection::numberOfPedestal() const {return mMaxPedestal;}

inline unsigned int
StFpdCollection::numberOfPMTpEEMC() const {return mMaxPMTpEEMC;}

inline unsigned int
StFpdCollection::numberOfPMTPbg() const {return mMaxPMTPbg;}

inline unsigned int
StFpdCollection::numberOfPMTSmdX() const {return mMaxPMTSmdX;}

inline unsigned int
StFpdCollection::numberOfPMTSmdY() const {return mMaxPMTSmdY;}

inline unsigned int
StFpdCollection::numberOfScalers() const {return mMaxScalers;}

inline unsigned int
StFpdCollection::token() const {return mToken;}

#endif
