/*!
 * \class StBbcTriggerDetector 
 * \author Akio Ogawa, Jan 2002
 */
/***************************************************************************
 *
 * $Id: StBbcTriggerDetector.h,v 2.10 2008/08/15 18:36:18 ullrich Exp $
 *
 * Author: Akio Ogawa, Jan 2002
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StBbcTriggerDetector.h,v $
 * Revision 2.10  2008/08/15 18:36:18  ullrich
 * Minor change, move zVertex().
 *
 * Revision 2.9  2007/07/11 23:06:45  perev
 * Cleanup+fix StXXXTriggerDetector
 *
 * Revision 2.8  2007/04/24 14:52:23  ullrich
 * Fixed bug in BBC unpacking (Akio).
 *
 * Revision 2.7  2004/08/03 17:22:16  ullrich
 * Major update by Akio and Marco.
 *
 * Revision 2.6  2004/02/11 01:42:09  ullrich
 * Added new constructor to load data from StTriggerData.
 *
 * Revision 2.5  2003/01/23 23:23:29  ullrich
 * Modified to cope with changes in how BBC data is loaded for Run3.
 *
 * Revision 2.4  2002/10/17 02:07:49  akio
 * Increase # of PMT from 32 to 48 for 2003 run.
 * Schema evolution should take care for reading old files.
 * Some of access functions (for east west separately, or sums)
 * will give wrong answer if you are using new 2003 mapping.
 * One should access data through raw adc values until improvements.
 * These functions do correct job for 2002 data.
 *
 * Revision 2.3  2002/02/22 22:56:46  jeromel
 * Doxygen basic documentation in all header files. None of this is required
 * for QM production.
 *
 * Revision 2.2  2002/01/28 19:28:10  jeromel
 * Wrong format of an enum list corrected. Solaris picked this up.
 *
 * Revision 2.1  2002/01/03 20:57:37  ullrich
 * Initial Revision.
 *
 **************************************************************************/
#ifndef StBbcTriggerDetector_hh
#define StBbcTriggerDetector_hh
#include "StObject.h"

class dst_TrgDet_st;
class StTriggerData;

class StBbcTriggerDetector : public StObject {
public:
    StBbcTriggerDetector();
    StBbcTriggerDetector(const dst_TrgDet_st&);
    StBbcTriggerDetector(const StTriggerData&);
    virtual ~StBbcTriggerDetector();
    // StBbcTriggerDetector(const StBbcTriggerDetector&);            use default
    // StBbcTriggerDetector& operator=(const StBbcTriggerDetector&); use default
    
    unsigned int   numberOfPMTs() const;
    unsigned int   numberOfRegisters() const;
    unsigned int   numberOfPedestalData() const;
    unsigned int   numberOfScalars() const;
    
    unsigned short adc(unsigned int) const;
    unsigned short tdc(unsigned int) const;
    unsigned short bbcRegister(unsigned int) const;
    unsigned short pedestalData(unsigned int) const;
    unsigned int   scalar(unsigned int) const;
    
    unsigned short pedestal(unsigned int id) const;
    unsigned short pedestalWidth(unsigned int id) const;
    unsigned short mip(unsigned int id) const;
    unsigned short mipWidth(unsigned int id) const;
    

    int   adcSumEast(); 
    int   adcSumWest();
    int   adcSumEastLarge(); 
    int   adcSumWestLarge();
    int   adcSum();
    int   adcSumLarge();
    int   adcSumAll();
    unsigned short onlineTimeDifference() const; // z vertex from Layer2 DSM in channel 

  /////////////////////////////////////////////
  // No longer supported after 2003. Do not use
    int   nHitEast();
    int   nHitWest();
    int   nHitEastLarge();
    int   nHitWestLarge();
    int   nHit();
    int   nHitLarge();
    int   nHitAll();
    int   tdcEarliestEast(); // 2002 = common start = smaller channel is earlier
    int   tdcEarliestWest(); // 2003 = common stop  = larger channel os earlier
  // No longer supported after 2003. Do not use
  //////////////////////////////////////////////

    float zVertex(); //z vertex in cm

    void  setAdc(unsigned int, unsigned short);
    void  setTdc(unsigned int, unsigned short);
    void  setRegister(unsigned int, unsigned short);
    void  setPedestal(unsigned int, unsigned short);
    void  setScalar(unsigned int, unsigned int);
    void  setOnlineTimeDifference(unsigned short);
    void  dump();
    
    unsigned int   year() const;
    void           setYear(unsigned int);
    
protected:
    enum {
        mMaxPMTs = 48,
        mMaxRegisters = 2,
        mMaxPedData = 128,
        mMaxScalars = 32
    };
    char mBeg[1];//!
    UShort_t mAdc[mMaxPMTs];
    UShort_t mTdc[mMaxPMTs];
    UShort_t mReg[mMaxRegisters];
    UShort_t mPed[mMaxPedData];
    UShort_t mScl[mMaxScalars];
    UInt_t   mYear;
    UInt_t   mDSMVTX;
    char mEnd[1];//!
    ClassDef(StBbcTriggerDetector,5)
};

inline unsigned int
StBbcTriggerDetector::numberOfPMTs() const {return mMaxPMTs;}

inline unsigned int
StBbcTriggerDetector::numberOfRegisters() const {return mMaxRegisters;}

inline unsigned int
StBbcTriggerDetector::numberOfPedestalData() const {return mMaxPedData;}

inline unsigned int
StBbcTriggerDetector::numberOfScalars() const {return mMaxScalars;}
        
inline unsigned short
StBbcTriggerDetector::pedestal(unsigned int id) const {return pedestalData(id);}

inline unsigned short
StBbcTriggerDetector::pedestalWidth(unsigned int id) const {return pedestalData(id+32);}

inline unsigned short
StBbcTriggerDetector::mip(unsigned int id) const {return pedestalData(id+64);}

inline unsigned short
StBbcTriggerDetector::mipWidth(unsigned int id) const {return pedestalData(id+96);}

inline unsigned int
StBbcTriggerDetector::year() const {return mYear;}

inline void
StBbcTriggerDetector::setYear(unsigned int v) {mYear = v;}

inline unsigned short
StBbcTriggerDetector::onlineTimeDifference() const {return mDSMVTX;}

inline void
StBbcTriggerDetector::setOnlineTimeDifference(unsigned short v) {mDSMVTX = v;}

#endif
