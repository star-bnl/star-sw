/***************************************************************************
 *
 * $Id: StZdcTriggerDetector.h,v 2.1 1999/10/13 19:44:22 ullrich Exp $
 *
 * Author: Thomas Ullrich, Sep 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StZdcTriggerDetector.h,v $
 * Revision 2.1  1999/10/13 19:44:22  ullrich
 * Initial Revision
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
    
    UInt_t   numberOfZdcCounters() const;
    Float_t  adc(UInt_t) const;
    Float_t  tdc(UInt_t) const;
    Float_t  adcSum(StBeamDirection) const;
    Float_t  adcSum() const;

    void setAdc(UInt_t, Float_t);
    void setTdc(UInt_t, Float_t);
    void setAdcSum(StBeamDirection, Float_t);
    enum {mMaxZdcCounter = 6};
    
protected:
    enum {mMaxZdcCounter = 16};
    Float_t  mAdc[mMaxZdcCounter];
    Float_t  mTdc[mMaxZdcCounter];
    Float_t  mSumAdc[2];
    Float_t  mSum;
    
    ClassDef(StZdcTriggerDetector,1)
};
#endif
