/***************************************************************************
 *
 * $Id: StVpdTriggerDetector.cxx,v 2.4 2001/04/05 04:00:59 ullrich Exp $
 *
 * Author: Thomas Ullrich, Sep 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StVpdTriggerDetector.cxx,v $
 * Revision 2.4  2001/04/05 04:00:59  ullrich
 * Replaced all (U)Long_t by (U)Int_t and all redundant ROOT typedefs.
 *
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

static const char rcsid[] = "$Id: StVpdTriggerDetector.cxx,v 2.4 2001/04/05 04:00:59 ullrich Exp $";

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

unsigned int
StVpdTriggerDetector::numberOfVpdCounters() const {return mMaxVpdCounter;}

float
StVpdTriggerDetector::adc(unsigned int i) const
{
    if (i < mMaxVpdCounter)
        return mAdc[i];
    else
        return 0;
}

float
StVpdTriggerDetector::time(unsigned int i) const
{
    if (i < mMaxVpdCounter)
        return mTime[i];
    else
        return 0;
}

float
StVpdTriggerDetector::minimumTime(StBeamDirection dir) const
{
    return mMinimumTime[dir];
}

float
StVpdTriggerDetector::vertexZ() const {return mVertexZ;}

void
StVpdTriggerDetector::setAdc(unsigned int i, float val)
{
    if (i < mMaxVpdCounter)
        mAdc[i] = val;
}

void
StVpdTriggerDetector::setTime(unsigned int i, float val)
{
    if (i < mMaxVpdCounter)
        mTime[i] = val;
}

void
StVpdTriggerDetector::setMinimumTime(StBeamDirection dir, float val)
{
    mMinimumTime[dir] = val;
}

void
StVpdTriggerDetector::setVertexZ(float val)
{
    mVertexZ = val;
}
