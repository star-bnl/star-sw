/***************************************************************************
 *
 * $Id: StVpdTriggerDetector.cxx,v 2.3 1999/12/21 15:09:25 ullrich Exp $
 *
 * Author: Thomas Ullrich, Sep 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StVpdTriggerDetector.cxx,v $
 * Revision 2.3  1999/12/21 15:09:25  ullrich
 * Modified to cope with new compiler version on Sun (CC5.0).
 *
 * Revision 2.2  1999/10/28 22:28:12  ullrich
 * Adapted new StArray version. First version to compile on Linux and Sun.
 *
 * Revision 2.1  1999/10/13 19:45:48  ullrich
 * Initial Revision
 *
 **************************************************************************/
#include <algorithm>
#include "StVpdTriggerDetector.h"
#include "tables/St_dst_TrgDet_Table.h"
#if !defined(ST_NO_NAMESPACES)
using std::fill_n;
using std::copy;
#endif

static const char rcsid[] = "$Id: StVpdTriggerDetector.cxx,v 2.3 1999/12/21 15:09:25 ullrich Exp $";

ClassImp(StVpdTriggerDetector)

StVpdTriggerDetector::StVpdTriggerDetector()
{
    fill_n(mAdc, static_cast<int>(mMaxVpdCounter), 0);
    fill_n(mTime, static_cast<int>(mMaxVpdCounter), 0);
    fill_n(mMinimumTime, 2, 0);
    mVertexZ = 0;
}

StVpdTriggerDetector::StVpdTriggerDetector(const dst_TrgDet_st& t)
{
    copy(t.adcVPD+0, t.adcVPD+mMaxVpdCounter, mAdc);
    copy(t.timeVPD+0, t.timeVPD+mMaxVpdCounter, mTime);
    mMinimumTime[east] = t.TimeEastVpd;
    mMinimumTime[west] = t.TimeWestVpd;
    mVertexZ = t.vertexZ;
}

StVpdTriggerDetector::~StVpdTriggerDetector() {/* noop */}

UInt_t
StVpdTriggerDetector::numberOfVpdCounters() const {return mMaxVpdCounter;}

Float_t
StVpdTriggerDetector::adc(UInt_t i) const
{
    if (i < mMaxVpdCounter)
        return mAdc[i];
    else
        return 0;
}

Float_t
StVpdTriggerDetector::time(UInt_t i) const
{
    if (i < mMaxVpdCounter)
        return mTime[i];
    else
        return 0;
}

Float_t
StVpdTriggerDetector::minimumTime(StBeamDirection dir) const
{
    return mMinimumTime[dir];
}

Float_t
StVpdTriggerDetector::vertexZ() const {return mVertexZ;}

void
StVpdTriggerDetector::setAdc(UInt_t i, Float_t val)
{
    if (i < mMaxVpdCounter)
        mAdc[i] = val;
}

void
StVpdTriggerDetector::setTime(UInt_t i, Float_t val)
{
    if (i < mMaxVpdCounter)
        mTime[i] = val;
}

void
StVpdTriggerDetector::setMinimumTime(StBeamDirection dir, Float_t val)
{
    mMinimumTime[dir] = val;
}

void
StVpdTriggerDetector::setVertexZ(Float_t val)
{
    mVertexZ = val;
}
