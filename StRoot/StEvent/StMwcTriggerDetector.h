/***************************************************************************
 *
 * $Id: StMwcTriggerDetector.h,v 2.1 1999/10/13 19:43:29 ullrich Exp $
 *
 * Author: Thomas Ullrich, Sep 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StMwcTriggerDetector.h,v $
 * Revision 2.1  1999/10/13 19:43:29  ullrich
 * Initial Revision
 *
 * Revision 2.1  1999/10/13 19:43:29  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef StMwcTriggerDetector_hh
#define StMwcTriggerDetector_hh
#include "StObject.h"

class dst_TrgDet_st;

class StMwcTriggerDetector : public StObject {
public:
    StMwcTriggerDetector();
    StMwcTriggerDetector(const dst_TrgDet_st&);
    // StMwcTriggerDetector(const StMwcTriggerDetector&);            use default
    // StMwcTriggerDetector& operator=(const StMwcTriggerDetector&); use default
    virtual ~StMwcTriggerDetector();

    UInt_t   numberOfMwcSectors() const;
    Float_t  mips(UInt_t) const;

    void setMips(UInt_t, Float_t);
    
protected:
    enum {mMaxMwcSectors = 96};
    Float_t  mMips[mMaxMwcSectors];
    
    ClassDef(StMwcTriggerDetector,1)
};
#endif
