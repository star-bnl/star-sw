/***************************************************************************
 *
 * $Id: StCtbTriggerDetector.cxx,v 2.3 1999/12/20 12:54:45 ullrich Exp $
 *
 * Author: Thomas Ullrich, Sep 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StCtbTriggerDetector.cxx,v $
 * Revision 2.3  1999/12/20 12:54:45  ullrich
 * Adapted changed in trigger table dst_TrgDet
 *
 * Revision 2.3  1999/12/20 12:54:45  ullrich
 * Adapted changed in trigger table dst_TrgDet
 *
 * Revision 2.2  1999/10/28 22:24:58  ullrich
 * Adapted new StArray version. First version to compile on Linux and Sun.
 *
 * Revision 2.1  1999/10/13 19:44:28  ullrich
 * Initial Revision
 *
 **************************************************************************/
#include <algorithm>
#include "StCtbTriggerDetector.h"
#include "tables/St_dst_TrgDet_Table.h"
#if !defined(ST_NO_NAMESPACES)
using std::fill_n;
using std::copy;
#endif

static const char rcsid[] = "$Id: StCtbTriggerDetector.cxx,v 2.3 1999/12/20 12:54:45 ullrich Exp $";

ClassImp(StCtbTriggerDetector)

StCtbTriggerDetector::StCtbTriggerDetector()
{
    fill_n(mMips, static_cast<int>(mMaxCtbCounter), 0);
    fill_n(mTime, static_cast<int>(mMaxCtbCounter), 0);
}

StCtbTriggerDetector::StCtbTriggerDetector(const dst_TrgDet_st& t)
{
    copy(t.nCtb+0, t.nCtb+mMaxCtbCounter, mMips);
    for (int i=0; i < static_cast<int>(mMaxCtbCounter); i++) {
	char c = t.timeCtb[i];             // Sun CC4.2 $#@&^%
	mTime[i] = static_cast<float>(c);
    }
}

StCtbTriggerDetector::~StCtbTriggerDetector() {/* noop */}

UInt_t
StCtbTriggerDetector::numberOfCtbCounters() const {return mMaxCtbCounter;}

Float_t
StCtbTriggerDetector::mips(UInt_t i) const
{
    if (i < mMaxCtbCounter)
        return mMips[i];
    else
        return 0;
}

Float_t
StCtbTriggerDetector::time(UInt_t i) const
{
    if (i < mMaxCtbCounter)
        return mTime[i];
    else
        return 0;
}

void
StCtbTriggerDetector::setMips(UInt_t i, Float_t val)
{
    if (i < mMaxCtbCounter)
        mMips[i] = val;
}

void
StCtbTriggerDetector::setTime(UInt_t i, Float_t val)
{
    if (i < mMaxCtbCounter)
        mTime[i] = val;
}
    
