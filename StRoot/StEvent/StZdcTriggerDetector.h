/***************************************************************************
 *
 * $Id: StZdcTriggerDetector.h,v 2.3 2000/07/13 12:51:13 ullrich Exp $
 *
 * Author: Thomas Ullrich, Sep 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StZdcTriggerDetector.h,v $
 * Revision 2.3  2000/07/13 12:51:13  ullrich
 * Added new method numberOfZdcWords to replace old one with wrong name.
 *
 * Revision 2.2  1999/12/20 12:54:48  ullrich
 * Adapted changed in trigger table dst_TrgDet
 *
 * Revision 2.1  1999/10/13 19:44:22  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef StZdcTriggerDetector_hh
#define StZdcTriggerDetector_hh
#include "StObject.h"
#include "StEnumerations.h"

class dst_TrgDet_st;

class StZdcTriggerDetector : public StObject {
public:
    StZdcTriggerDetector();
    StZdcTriggerDetector(const dst_TrgDet_st&);
    // StZdcTriggerDetector(const StZdcTriggerDetector&);            use default
    // StZdcTriggerDetector& operator=(const StZdcTriggerDetector&); use default
    virtual ~StZdcTriggerDetector();
    
    Float_t  adcSum(StBeamDirection) const;
    Float_t  adcSum() const;
    UInt_t   numberOfZdcWords() const;
    Float_t  adc(UInt_t) const;
    Float_t  tdc(UInt_t) const;

    void setAdc(UInt_t, Float_t);
    void setTdc(UInt_t, Float_t);
    void setAdcSum(StBeamDirection, Float_t);
    void setAdcSum(Float_t);

    UInt_t   numberOfZdcCounters() const;  // usage depreciated, to be removed soon
    
protected:
    enum {mMaxZdcWords = 16};
    Float_t  mAdc[mMaxZdcWords];
    Float_t  mTdc[mMaxZdcWords];
    Float_t  mSumAdc[2];
    Float_t  mSum;
    
    ClassDef(StZdcTriggerDetector,1)
};
#endif
