/***************************************************************************
 *
 * $Id: StRun.cxx,v 2.0 1999/10/12 18:42:29 ullrich Exp $
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
 * Revision 2.0  1999/10/12 18:42:29  ullrich
 * Completely Revised for New Version
 *
 * Revision 2.0  1999/10/12 18:42:29  ullrich
 * Completely Revised for New Version
 *
 **************************************************************************/
#include <algorithm>
#include <iostream.h>
#include "StRun.h"
#include "StRunSummary.h"
#include "tables/run_header.h"
#include "tables/dst_run_summary.h"
#include "SystemOfUnits.h"

TString StRun::mCvsTag = "$Id: StRun.cxx,v 2.0 1999/10/12 18:42:29 ullrich Exp $";
static const char rcsid[] = "$Id: StRun.cxx,v 2.0 1999/10/12 18:42:29 ullrich Exp $";

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

Int_t
StRun::operator==(const StRun& r) const
{
    return mId == r.mId && mBfcId == r.mBfcId;
}

Int_t
StRun::operator!=(const StRun& r) const
{
    return !(r == *this);
}

const TString&
StRun::cvsTag() { return mCvsTag; }

void
StRun::setId(Long_t val) { mId = val; }
    
void
StRun::setBfcId(Long_t val) { mBfcId = val; }
    
void
StRun::setType(const Char_t* val) { mType = val; }

void
StRun::setTriggerMask(Long_t val) { mTriggerMask = val; }

void
StRun::setCenterOfMassEnergy(Double_t val) { mCenterOfMassEnergy = val; }

void
StRun::setBeamMassNumber(StBeamDirection dir, Short_t val)
{
    if (dir == east)
        mEastA = val;
    else
        mWestA = val;
}

void
StRun::setBeamCharge(StBeamDirection dir, Short_t val)
{
    if (dir == east)
        mEastZ = val;
    else
        mWestZ = val;
}

void
StRun::setSummary(StRunSummary* val) { mSummary = val; }

void
StRun::setMagneticField(Double_t val) { mMagneticFieldZ = val; }

Long_t
StRun::id() const { return mId; }

Long_t
StRun::bfcId() const { return mBfcId; }

const TString&
StRun::type() const { return mType; }

Long_t
StRun::triggerMask() const { return mTriggerMask; }

Double_t
StRun::centerOfMassEnergy() const { return mCenterOfMassEnergy; }

Double_t
StRun::magneticField() const { return mMagneticFieldZ; }

Short_t
StRun::beamMassNumber(StBeamDirection dir) const
{
    return dir == east ? mEastA : mWestA;
}

Short_t
StRun::beamCharge(StBeamDirection dir) const
{
    return dir == east ? mEastZ : mWestZ;
}

StRunSummary*
StRun::summary() { return mSummary; }

const StRunSummary*
StRun::summary() const { return mSummary; }

