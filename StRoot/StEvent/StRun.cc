/***************************************************************************
 *
 * $Id: StRun.cc,v 1.1 1999/01/15 20:39:56 wenaus Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StRun.cc,v $
 * Revision 1.1  1999/01/15 20:39:56  wenaus
 * Commit Thomas' original code
 *
 **************************************************************************/
#include "StRun.hh"

StRun::StRun()
{
    mId = 0;
    mTriggerMask = 0;
    mCenterOfMassEnergy = 0;
    mEastA = 0;
    mEastZ = 0;
    mWestA = 0;
    mWestZ = 0;
    mSummary = 0;
}

StRun::~StRun()
{
    delete mSummary;
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
