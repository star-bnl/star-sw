/***************************************************************************
 *
 * $Id: StEventInfo.cxx,v 2.5 2009/11/23 16:34:06 fisyak Exp $
 *
 * Author: Thomas Ullrich, Jun 2000
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StEventInfo.cxx,v $
 * Revision 2.5  2009/11/23 16:34:06  fisyak
 * Cleanup, remove dependence on dst tables, clean up software monitors
 *
 * Revision 2.4  2001/09/19 04:48:08  ullrich
 * Added event size.
 *
 * Revision 2.3  2001/04/05 04:00:49  ullrich
 * Replaced all (U)Long_t by (U)Int_t and all redundant ROOT typedefs.
 *
 * Revision 2.2  2000/09/06 22:34:20  ullrich
 * Changed mBunchCrossingNumber from scalar to array to hold all 64 bits.
 *
 * Revision 2.1  2000/06/19 01:32:16  perev
 *  Thomas StEvent branches added
 *
 **************************************************************************/
#include "StEventInfo.h"
#include "tables/St_event_header_Table.h"

static const char rcsid[] = "$Id: StEventInfo.cxx,v 2.5 2009/11/23 16:34:06 fisyak Exp $";

ClassImp(StEventInfo)

StEventInfo::StEventInfo()
{
    mRunId = 0;
    mId    = 0;
    mTime  = 0;
    mTriggerMask = 0;
    mBunchCrossingNumber[0] = 0;
    mBunchCrossingNumber[1] = 0;
    mEventSize = 0;
}
StEventInfo::~StEventInfo() { /* noop */ }

const TString&
StEventInfo::type() const { return mType; }

int
StEventInfo::id() const { return mId; }

int
StEventInfo::runId() const { return mRunId; }

int
StEventInfo::time() const { return mTime; }

unsigned int
StEventInfo::triggerMask() const { return mTriggerMask; }

unsigned int
StEventInfo::bunchCrossingNumber(unsigned int i) const
{
    return i<2 ? mBunchCrossingNumber[i] : 0;
}

unsigned int
StEventInfo::eventSize() const { return mEventSize; }

void
StEventInfo::setType(const char* val) { mType = val; }

void
StEventInfo::setRunId(int val) { mRunId = val; }

void
StEventInfo::setId(int val) { mId = val; }

void
StEventInfo::setTime(int val) { mTime = val; }

void
StEventInfo::setTriggerMask(unsigned int val) { mTriggerMask = val; }

void
StEventInfo::setBunchCrossingNumber(unsigned int val, unsigned int i)
{
    if (i<2) mBunchCrossingNumber[i] = val;
}

void
StEventInfo::setEventSize(unsigned int val) { mEventSize = val; }
