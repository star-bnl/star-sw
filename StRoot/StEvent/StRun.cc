/***************************************************************************
 *
 * $Id: StRun.cc,v 1.5 1999/02/22 19:53:52 wenaus Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 *
 * History:
 * 15/01/1999 T. Wenaus  Add table-based constructor
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StRun.cc,v $
 * Revision 1.5  1999/02/22 19:53:52  wenaus
 * cleaner deleting
 *
 * Revision 1.4  1999/02/10 21:50:31  wenaus
 * Plug memory leaks
 *
 * Revision 1.3  1999/01/30 23:03:14  wenaus
 * table load intfc change; include ref change
 *
 * Revision 1.2  1999/01/15 22:53:49  wenaus
 * version with constructors for table-based loading
 *
 **************************************************************************/
#include "StEvent/StRun.hh"

static const char rcsid[] = "$Id: StRun.cc,v 1.5 1999/02/22 19:53:52 wenaus Exp $";

StRun::StRun()
{
    mType = "";
    mId = 0;
    mTriggerMask = 0;
    mCenterOfMassEnergy = 0;
    mEastA = 0;
    mEastZ = 0;
    mWestA = 0;
    mWestZ = 0;
    mSummary = 0;
}

StRun::StRun(dst_run_header_st& runHdr, dst_run_summary_st& runSum)
{
    mType = runHdr.event_type;
    mId = runHdr.run_id;
    mTriggerMask = runHdr.trig_mask;
    mCenterOfMassEnergy = runHdr.sqrt_s;
    mEastA = runHdr.east_a;
    mEastZ = runHdr.east_z;
    mWestA = runHdr.west_a;
    mWestZ = runHdr.west_z;
    mSummary = 0;
}

StRun::StRun(dst_run_header_st& runHdr)
{
    mType = runHdr.event_type;
    mId = runHdr.run_id;
    mTriggerMask = runHdr.trig_mask;
    mCenterOfMassEnergy = runHdr.sqrt_s;
    mEastA = runHdr.east_a;
    mEastZ = runHdr.east_z;
    mWestA = runHdr.west_a;
    mWestZ = runHdr.west_z;
}

StRun::~StRun()
{
    delete mSummary; mSummary=0;
}

const StRun& StRun::operator=(const StRun&) { return *this; } // private

StRun::StRun(const StRun&) { /* noop */ }; // private

int StRun::operator==(const StRun& r) const
{
    return mId == r.mId;
}

int StRun::operator!=(const StRun& r) const
{
    return !(r == *this);  // negate operator==
}

void StRun::setId(long val) { mId = val; }
    
void StRun::setType(const char* val) { mType = val; }

void StRun::setTriggerMask(long val) { mTriggerMask = val; }

void StRun::setCenterOfMassEnergy(double val) { mCenterOfMassEnergy = val; }

void StRun::setBeamMassNumber(StBeamDirection dir, short val)
{
    if (dir == east)
	mEastA = val;
    else
	mWestA = val;
}

void StRun::setBeamCharge(StBeamDirection dir, short val)
{
    if (dir == east)
	mEastZ = val;
    else
	mWestZ = val;
}

void StRun::setSummary(StRunSummary* val) { mSummary = val; }
