/***************************************************************************
 *
 * $Id: StRun.cxx,v 2.2 2001/04/05 04:00:54 ullrich Exp $
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
 * $Log: StRun.cxx,v $
 * Revision 2.2  2001/04/05 04:00:54  ullrich
 * Replaced all (U)Long_t by (U)Int_t and all redundant ROOT typedefs.
 *
 * Revision 2.1  1999/10/28 22:26:27  ullrich
 * Adapted new StArray version. First version to compile on Linux and Sun.
 *
 * Revision 2.0  1999/10/12 18:42:29  ullrich
 * Completely Revised for New Version
 *
 **************************************************************************/
#include <algorithm>
#include <iostream.h>
#include "StRun.h"
#include "StRunSummary.h"
#include "tables/St_run_header_Table.h"
#include "tables/St_dst_run_summary_Table.h"
#include "SystemOfUnits.h"

TString StRun::mCvsTag = "$Id: StRun.cxx,v 2.2 2001/04/05 04:00:54 ullrich Exp $";
static const char rcsid[] = "$Id: StRun.cxx,v 2.2 2001/04/05 04:00:54 ullrich Exp $";

ClassImp(StRun)

StRun::StRun(): St_DataSet("StRun")
{
    mId = 0;
    mBfcId = 0;
    mTriggerMask = 0;
    mCenterOfMassEnergy = 0;
    mEastA = 0;
    mEastZ = 0;
    mWestA = 0;
    mWestZ = 0;
    mSummary = 0;
    mMagneticFieldZ = 0;
}

void StRun::initFromTable(const run_header_st& runHdr)
{
    mType  = runHdr.event_type;
    mId    = runHdr.exp_run_id;
    mBfcId = runHdr.bfc_run_id;
    mTriggerMask = runHdr.trig_mask;
    mCenterOfMassEnergy = runHdr.sqrt_s;
    mEastA = runHdr.east_a;
    mEastZ = runHdr.east_z;
    mWestA = runHdr.west_a;
    mWestZ = runHdr.west_z;
    mSummary = 0;
    mMagneticFieldZ = runHdr.field;
}

StRun::StRun(const run_header_st& runHdr) :
    St_DataSet("StRun")
{
    initFromTable(runHdr);
}

StRun::StRun(const run_header_st& runHdr, const dst_run_summary_st& runSum):
    St_DataSet("StRun")
{
    initFromTable(runHdr);
    mSummary = new StRunSummary(runSum);
}

StRun::~StRun()
{
    delete mSummary;
    mSummary = 0;
}

int
StRun::operator==(const StRun& r) const
{
    return mId == r.mId && mBfcId == r.mBfcId;
}

int
StRun::operator!=(const StRun& r) const
{
    return !(r == *this);
}

const TString&
StRun::cvsTag() { return mCvsTag; }

void
StRun::setId(int val) { mId = val; }
    
void
StRun::setBfcId(int val) { mBfcId = val; }
    
void
StRun::setType(const char* val) { mType = val; }

void
StRun::setTriggerMask(int val) { mTriggerMask = val; }

void
StRun::setCenterOfMassEnergy(double val) { mCenterOfMassEnergy = val; }

void
StRun::setBeamMassNumber(StBeamDirection dir, short val)
{
    if (dir == east)
        mEastA = val;
    else
        mWestA = val;
}

void
StRun::setBeamCharge(StBeamDirection dir, short val)
{
    if (dir == east)
        mEastZ = val;
    else
        mWestZ = val;
}

void
StRun::setSummary(StRunSummary* val) { mSummary = val; }

void
StRun::setMagneticField(double val) { mMagneticFieldZ = val; }

int
StRun::id() const { return mId; }

int
StRun::bfcId() const { return mBfcId; }

const TString&
StRun::type() const { return mType; }

int
StRun::triggerMask() const { return mTriggerMask; }

double
StRun::centerOfMassEnergy() const { return mCenterOfMassEnergy; }

double
StRun::magneticField() const { return mMagneticFieldZ; }

short
StRun::beamMassNumber(StBeamDirection dir) const
{
    return dir == east ? mEastA : mWestA;
}

short
StRun::beamCharge(StBeamDirection dir) const
{
    return dir == east ? mEastZ : mWestZ;
}

StRunSummary*
StRun::summary() { return mSummary; }

const StRunSummary*
StRun::summary() const { return mSummary; }

