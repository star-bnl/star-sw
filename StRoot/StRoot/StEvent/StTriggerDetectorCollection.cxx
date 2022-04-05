/***************************************************************************
 *
 * $Id: StTriggerDetectorCollection.cxx,v 2.12 2007/07/11 23:06:46 perev Exp $
 *
 * Author: Thomas Ullrich, Sep 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StTriggerDetectorCollection.cxx,v $
 * Revision 2.12  2007/07/11 23:06:46  perev
 * Cleanup+fix StXXXTriggerDetector
 *
 * Revision 2.11  2007/07/02 20:23:08  ullrich
 * Added FMS and MTD.
 *
 * Revision 2.10  2007/04/04 16:40:18  ullrich
 * Add setup of VPD data to constructor.
 *
 * Revision 2.9  2006/09/14 00:02:16  ullrich
 * Removed argument (run) in constructor. Not needed anymore.
 *
 * Revision 2.8  2006/08/21 19:43:35  ullrich
 * Run number becomes constructor argument. Needed for ZDC. (Akio)
 *
 * Revision 2.7  2004/08/03 17:21:15  ullrich
 * Fpd as trigger detector added.
 *
 * Revision 2.6  2004/02/11 01:42:09  ullrich
 * Added new constructor to load data from StTriggerData.
 *
 * Revision 2.5  2003/01/29 23:59:12  ullrich
 * Changed order of instantiation in constructor.
 *
 * Revision 2.4  2003/01/23 23:49:32  ullrich
 * Feeding now tables to BBC instance as well.
 *
 * Revision 2.3  2002/02/20 03:12:15  ullrich
 * Added EMC trigger.
 *
 * Revision 2.2  2002/01/03 20:59:33  ullrich
 * Added BBC and FPD.
 *
 * Revision 2.1  1999/10/28 22:27:58  ullrich
 * Adapted new StArray version. First version to compile on Linux and Sun.
 *
 * Revision 2.0  1999/10/12 18:43:18  ullrich
 * Completely Revised for New Version
 *
 **************************************************************************/
#include "StTriggerDetectorCollection.h"
#include "StTriggerData.h"
#include "tables/St_dst_TrgDet_Table.h"

static const char rcsid[] = "$Id: StTriggerDetectorCollection.cxx,v 2.12 2007/07/11 23:06:46 perev Exp $";

ClassImp(StTriggerDetectorCollection)

StTriggerDetectorCollection::StTriggerDetectorCollection() {/* noop */}

StTriggerDetectorCollection::StTriggerDetectorCollection(const dst_TrgDet_st& t) :
    mCtb(t), mMwc(t), mVpd(t), mZdc(t), mBbc(t), mEmc(t), mFpd(t) {/* noop */}

StTriggerDetectorCollection::StTriggerDetectorCollection(const StTriggerData& t) 
//   :mCtb(t), mVpd(t), mZdc(t), mBbc(t), mEmc(t), mFpd(t), mFms(t), mMtd(t) {/* noop */}
     :mCtb(t), mVpd(t), mZdc(t), mBbc(t), mEmc(t), mFpd(t), mFms(t), mMtd(t) {/* noop */}

StTriggerDetectorCollection::~StTriggerDetectorCollection() {/* noop */}

StBbcTriggerDetector&
StTriggerDetectorCollection::bbc() { return mBbc; }

const StBbcTriggerDetector&
StTriggerDetectorCollection::bbc() const { return mBbc; }

StCtbTriggerDetector&
StTriggerDetectorCollection::ctb() { return mCtb; }

const StCtbTriggerDetector&
StTriggerDetectorCollection::ctb() const { return mCtb; }

StMwcTriggerDetector&
StTriggerDetectorCollection::mwc() { return mMwc; }

const StMwcTriggerDetector&
StTriggerDetectorCollection::mwc() const { return mMwc; }

StVpdTriggerDetector&
StTriggerDetectorCollection::vpd() { return mVpd; }

const StVpdTriggerDetector&
StTriggerDetectorCollection::vpd() const { return mVpd; }

StZdcTriggerDetector&
StTriggerDetectorCollection::zdc() { return mZdc; }

const StZdcTriggerDetector&
StTriggerDetectorCollection::zdc() const { return mZdc; }

StEmcTriggerDetector&
StTriggerDetectorCollection::emc() { return mEmc; }

const StEmcTriggerDetector&
StTriggerDetectorCollection::emc() const { return mEmc; }

StFpdTriggerDetector&
StTriggerDetectorCollection::fpd() { return mFpd; }

const StFpdTriggerDetector&
StTriggerDetectorCollection::fpd() const { return mFpd; }

StFmsTriggerDetector&
StTriggerDetectorCollection::fms() { return mFms; }

const StFmsTriggerDetector&
StTriggerDetectorCollection::fms() const { return mFms; }

StMtdTriggerDetector&
StTriggerDetectorCollection::mtd() { return mMtd; }

const StMtdTriggerDetector&
StTriggerDetectorCollection::mtd() const { return mMtd; }
