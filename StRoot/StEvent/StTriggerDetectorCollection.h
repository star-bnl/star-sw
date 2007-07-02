/*!
 * \class StTriggerDetectorCollection 
 * \author Thomas Ullrich, Sep 1999
 */
/***************************************************************************
 *
 * $Id: StTriggerDetectorCollection.h,v 2.9 2007/07/02 20:23:09 ullrich Exp $
 *
 * Author: Thomas Ullrich, Sep 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *`
 * $Log: StTriggerDetectorCollection.h,v $
 * Revision 2.9  2007/07/02 20:23:09  ullrich
 * Added FMS and MTD.
 *
 * Revision 2.8  2006/09/14 00:02:16  ullrich
 * Removed argument (run) in constructor. Not needed anymore.
 *
 * Revision 2.7  2006/08/21 19:43:35  ullrich
 * Run number becomes constructor argument. Needed for ZDC. (Akio)
 *
 * Revision 2.6  2005/10/10 22:27:42  ullrich
 * Added setXXX functions.
 *
 * Revision 2.5  2004/08/03 17:21:15  ullrich
 * Fpd as trigger detector added.
 *
 * Revision 2.4  2004/02/11 01:42:09  ullrich
 * Added new constructor to load data from StTriggerData.
 *
 * Revision 2.3  2002/02/22 22:56:53  jeromel
 * Doxygen basic documentation in all header files. None of this is required
 * for QM production.
 *
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
#include "StFpdTriggerDetector.h"
#include "StFmsTriggerDetector.h"
#include "StMtdTriggerDetector.h"

class dst_TrgDet_st;
class StTriggerData;

class StTriggerDetectorCollection : public StObject {
public:
    StTriggerDetectorCollection();
    StTriggerDetectorCollection(const dst_TrgDet_st&);
    StTriggerDetectorCollection(const StTriggerData&);
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
    StFpdTriggerDetector&       fpd();
    const StFpdTriggerDetector& fpd() const;
    StFmsTriggerDetector&       fms();
    const StFmsTriggerDetector& fms() const;
    StMtdTriggerDetector&       mtd();
    const StMtdTriggerDetector& mtd() const;

    void setCtbTriggerDetector(const StCtbTriggerDetector&);
    void setMwcTriggerDetector(const StMwcTriggerDetector&);
    void setVpdTriggerDetector(const StVpdTriggerDetector&);
    void setZdcTriggerDetector(const StZdcTriggerDetector&);
    void setBbcTriggerDetector(const StBbcTriggerDetector&);
    void setEmcTriggerDetector(const StEmcTriggerDetector&);
    void setFpdTriggerDetector(const StFpdTriggerDetector&);
    void setFmsTriggerDetector(const StFmsTriggerDetector&);
    void setMtdTriggerDetector(const StMtdTriggerDetector&);
    
protected:
    StCtbTriggerDetector mCtb;
    StMwcTriggerDetector mMwc;
    StVpdTriggerDetector mVpd;
    StZdcTriggerDetector mZdc;
    StBbcTriggerDetector mBbc;
    StEmcTriggerDetector mEmc;
    StFpdTriggerDetector mFpd;
    StFmsTriggerDetector mFms;
    StMtdTriggerDetector mMtd;
    
    ClassDef(StTriggerDetectorCollection,6)
};

inline void StTriggerDetectorCollection::setCtbTriggerDetector(const StCtbTriggerDetector& val) {mCtb = val;}
inline void StTriggerDetectorCollection::setMwcTriggerDetector(const StMwcTriggerDetector& val) {mMwc = val;}
inline void StTriggerDetectorCollection::setVpdTriggerDetector(const StVpdTriggerDetector& val) {mVpd = val;}
inline void StTriggerDetectorCollection::setZdcTriggerDetector(const StZdcTriggerDetector& val) {mZdc = val;}
inline void StTriggerDetectorCollection::setBbcTriggerDetector(const StBbcTriggerDetector& val) {mBbc = val;}
inline void StTriggerDetectorCollection::setEmcTriggerDetector(const StEmcTriggerDetector& val) {mEmc = val;}
inline void StTriggerDetectorCollection::setFpdTriggerDetector(const StFpdTriggerDetector& val) {mFpd = val;}
inline void StTriggerDetectorCollection::setFmsTriggerDetector(const StFmsTriggerDetector& val) {mFms = val;}
inline void StTriggerDetectorCollection::setMtdTriggerDetector(const StMtdTriggerDetector& val) {mMtd = val;}

#endif
