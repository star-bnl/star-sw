/***************************************************************************
 *
 * $Id: StTriggerDetectorCollection.h,v 2.2 2002/02/20 03:12:15 ullrich Exp $
 *
 * Author: Thomas Ullrich, Sep 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StTriggerDetectorCollection.h,v $
 * Revision 2.2  2002/02/20 03:12:15  ullrich
 * Added EMC trigger.
 *
 * Revision 2.1  2002/01/03 20:59:34  ullrich
 * Added BBC and FPD.
 *
 * Revision 2.0  1999/10/12 18:43:20  ullrich
 * Completely Revised for New Version
 *
 **************************************************************************/
#ifndef StTriggerDetectorCollection_hh
#define StTriggerDetectorCollection_hh
#include "StObject.h"
#include "StBbcTriggerDetector.h"
#include "StCtbTriggerDetector.h"
#include "StMwcTriggerDetector.h"
#include "StVpdTriggerDetector.h"
#include "StZdcTriggerDetector.h"
#include "StEmcTriggerDetector.h"

class dst_TrgDet_st;

class StTriggerDetectorCollection : public StObject {
public:
    StTriggerDetectorCollection();
    StTriggerDetectorCollection(const dst_TrgDet_st&);
    // StTriggerDetectorCollection(const StTriggerDetectorCollection&);            use default
    // StTriggerDetectorCollection& operator=(const StTriggerDetectorCollection&); use default
    virtual ~StTriggerDetectorCollection();
    
    StBbcTriggerDetector&       bbc();
    const StBbcTriggerDetector& bbc() const;
     StCtbTriggerDetector&      ctb();
    const StCtbTriggerDetector& ctb() const;
    StMwcTriggerDetector&       mwc();
    const StMwcTriggerDetector& mwc() const;
    StVpdTriggerDetector&       vpd();
    const StVpdTriggerDetector& vpd() const;
    StZdcTriggerDetector&       zdc();
    const StZdcTriggerDetector& zdc() const;
    StEmcTriggerDetector&       emc();
    const StEmcTriggerDetector& emc() const;
    
protected:
    StCtbTriggerDetector mCtb;
    StMwcTriggerDetector mMwc;
    StVpdTriggerDetector mVpd;
    StZdcTriggerDetector mZdc;
    StBbcTriggerDetector mBbc;
    StEmcTriggerDetector mEmc;
    
    ClassDef(StTriggerDetectorCollection,3)
};
#endif
