/***************************************************************************
 *
 * $Id: StCtbTriggerDetector.h,v 2.1 1999/10/13 19:42:56 ullrich Exp $
 *
 * Author: Thomas Ullrich, Sep 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StCtbTriggerDetector.h,v $
 * Revision 2.1  1999/10/13 19:42:56  ullrich
 * Initial Revision
 *
 * Revision 2.1  1999/10/13 19:42:56  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef StCtbTriggerDetector_hh
#define StCtbTriggerDetector_hh
#include "StObject.h"

class dst_TrgDet_st;

class StCtbTriggerDetector : public StObject {
public:
    StCtbTriggerDetector();
    StCtbTriggerDetector(const dst_TrgDet_st&);
    virtual ~StCtbTriggerDetector();
    // StCtbTriggerDetector(const StCtbTriggerDetector&);            use default
    // StCtbTriggerDetector& operator=(const StCtbTriggerDetector&); use default
    
    UInt_t   numberOfCtbCounters() const;
    Float_t  mips(UInt_t) const;
    Float_t  time(UInt_t) const;

    void setMips(UInt_t, Float_t);
    void setTime(UInt_t, Float_t);
    
protected:
    enum {mMaxCtbCounter = 240};
    Float_t  mMips[mMaxCtbCounter];
    Float_t  mTime[mMaxCtbCounter];
    
    ClassDef(StCtbTriggerDetector,1)
};
#endif
