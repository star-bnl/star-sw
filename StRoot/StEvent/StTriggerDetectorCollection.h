/***************************************************************************
 *
 * $Id: StTriggerDetectorCollection.h,v 2.0 1999/10/12 18:43:20 ullrich Exp $
 *
 * Author: Thomas Ullrich, Sep 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StTriggerDetectorCollection.h,v $
 * Revision 2.0  1999/10/12 18:43:20  ullrich
 * Completely Revised for New Version
 *
 **************************************************************************/
#ifndef StTriggerDetectorCollection_hh
#define StTriggerDetectorCollection_hh
#include "StObject.h"
#include "StCtbTriggerDetector.h"
#include "StMwcTriggerDetector.h"
#include "StVpdTriggerDetector.h"
#include "StZdcTriggerDetector.h"

class dst_TrgDet_st;

class StTriggerDetectorCollection : public StObject {
public:
    StTriggerDetectorCollection();
    StTriggerDetectorCollection(const dst_TrgDet_st&);
    // StTriggerDetectorCollection(const StTriggerDetectorCollection&);            use default
    // StTriggerDetectorCollection& operator=(const StTriggerDetectorCollection&); use default
    virtual ~StTriggerDetectorCollection();
    
    StCtbTriggerDetector&       ctb();
    const StCtbTriggerDetector& ctb() const;
    StMwcTriggerDetector&       mwc();
    const StMwcTriggerDetector& mwc() const;
    StVpdTriggerDetector&       vpd();
    const StVpdTriggerDetector& vpd() const;
    StZdcTriggerDetector&       zdc();
    const StZdcTriggerDetector& zdc() const;
    
protected:
    StCtbTriggerDetector mCtb;
    StMwcTriggerDetector mMwc;
    StVpdTriggerDetector mVpd;
    StZdcTriggerDetector mZdc;
    
    ClassDef(StTriggerDetectorCollection,1)
};
#endif
