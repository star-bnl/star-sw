/***************************************************************************
 *
 * $Id: StEventInfo.cxx,v 2.1 2000/06/19 01:32:16 perev Exp $
 *
 * Author: Thomas Ullrich, Jun 2000
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StEventInfo.cxx,v $
 * Revision 2.1  2000/06/19 01:32:16  perev
 * Thomas StEvent branches added
 *
 * Revision 2.1  2000/06/19 01:32:16  perev
 *  Thomas StEvent branches added
 *
 **************************************************************************/
#include "StEventInfo.h"
#include "tables/St_event_header_Table.h"

static const char rcsid[] = "$Id: StEventInfo.cxx,v 2.1 2000/06/19 01:32:16 perev Exp $";

ClassImp(StEventInfo)

StEventInfo::StEventInfo()
{
    mRunId = 0;
    mId    = 0;
    mTime  = 0;
    mTriggerMask = 0; 
    mBunchCrossingNumber = 0; 
}
 
StEventInfo::StEventInfo(const event_header_st& evtHdr)
{
    mType  = evtHdr.event_type;
    mRunId = evtHdr.exp_run_id;
    mId    = evtHdr.n_event;
    mTime  = evtHdr.time;
    mTriggerMask = evtHdr.trig_mask;
    mBunchCrossingNumber = evtHdr.bunch_cross;
}

StEventInfo::~StEventInfo() { /* noop */ }

const TString&
StEventInfo::type() const { return mType; }

Long_t
StEventInfo::id() const { return mId; }

Long_t
StEventInfo::runId() const { return mRunId; }

Long_t
StEventInfo::time() const { return mTime; }

ULong_t
StEventInfo::triggerMask() const { return mTriggerMask; }

ULong_t
StEventInfo::bunchCrossingNumber() const { return mBunchCrossingNumber; }

void
StEventInfo::setType(const Char_t* val) { mType = val; }

void
StEventInfo::setRunId(Long_t val) { mRunId = val; }

void
StEventInfo::setId(Long_t val) { mId = val; }

void
StEventInfo::setTime(Long_t val) { mTime = val; }

void
StEventInfo::setTriggerMask(ULong_t val) { mTriggerMask = val; }

void
StEventInfo::setBunchCrossingNumber(ULong_t val) { mBunchCrossingNumber = val; }
