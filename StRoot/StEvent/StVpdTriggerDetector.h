/***************************************************************************
 *
 * $Id: StVpdTriggerDetector.h,v 2.1 1999/10/13 19:44:20 ullrich Exp $
 *
 * Author: Thomas Ullrich, Sep 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StVpdTriggerDetector.h,v $
 * Revision 2.1  1999/10/13 19:44:20  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef StVpdTriggerDetector_hh
#define StVpdTriggerDetector_hh
#include "StObject.h"
#include "StEnumerations.h"

class dst_TrgDet_st;

class StVpdTriggerDetector : public StObject {
public:
    StVpdTriggerDetector();
    StVpdTriggerDetector(const dst_TrgDet_st&);
    // StVpdTriggerDetector(const StVpdTriggerDetector&);            use default
    // StVpdTriggerDetector& operator=(const StVpdTriggerDetector&); use default
    virtual ~StVpdTriggerDetector();
    
    UInt_t   numberOfVpdCounters() const;
    Float_t  adc(UInt_t) const;
    Float_t  time(UInt_t) const;
    Float_t  minimumTime(StBeamDirection) const;
    Float_t  vertexZ() const;

    void setAdc(UInt_t, Float_t);
    void setTime(UInt_t, Float_t);
    void setMinimumTime(StBeamDirection, Float_t);
    void setVertexZ(Float_t);
    
protected:
    enum {mMaxVpdCounter = 48};
    Float_t  mAdc[mMaxVpdCounter];
    Float_t  mTime[mMaxVpdCounter];
    Float_t  mMinimumTime[2];
    Float_t  mVertexZ;
    
    ClassDef(StVpdTriggerDetector,1)
};
#endif
