/***************************************************************************
 *
 * $Id: StRun.cxx,v 1.1 1999/01/30 03:58:07 fisyak Exp $
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
 * Revision 1.1  1999/01/30 03:58:07  fisyak
 * Root Version of StEvent
 *
 * Revision 1.3  1999/01/30 23:03:14  wenaus
 * table load intfc change; include ref change
 *
 * Revision 1.2  1999/01/15 22:53:49  wenaus
 * version with constructors for table-based loading
 *
static const Char_t rcsid[] = "$Id: StRun.cxx,v 1.1 1999/01/30 03:58:07 fisyak Exp $";
 **************************************************************************/
#ifdef __ROOT__
#include "tables/run_header.h"
static const Char_t rcsid[] = "$Id: StRun.cxx,v 1.1 1999/01/30 03:58:07 fisyak Exp $";
#endif
StRun::StRun()
  StRun::StRun():
mCVSTag("$Name:  $")
ClassImp(StRun)
    mType = "";

{
    mId = 0;
    mBfcId = 0;
    mTriggerMask = 0;
    mCenterOfMassEnergy = 0;
    mEastA = 0;
StRun::StRun(dst_run_header_st* runHdr, dst_run_summary_st* runSum)
StRun::StRun(dst_run_header_st& runHdr, dst_run_summary_st& runSum):
    mType = runHdr->event_type;
    mId = runHdr->run_id;
    mTriggerMask = runHdr->trig_mask;
    mCenterOfMassEnergy = runHdr->sqrt_s;
    mEastA = runHdr->east_a;
    mEastZ = runHdr->east_z;
    mWestA = runHdr->west_a;
    mWestZ = runHdr->west_z;
    mTriggerMask = runHdr.trig_mask;
    mCenterOfMassEnergy = runHdr.sqrt_s;
    mEastA = runHdr.east_a;
    mSummary = 0;
{
    delete mSummary;
    mSummary = new StRunSummary(runSum);
}
    delete mSummary; mSummary=0;
{
    delete mSummary;
const StRun& StRun::operator=(const StRun&) { return *this; } // private

StRun::StRun(const StRun&) { /* noop */ }; // private

Int_t StRun::operator==(const StRun& r) const

    return mId == r.mId;
StRun::operator==(const StRun& r) const
{
Int_t StRun::operator!=(const StRun& r) const

    return !(r == *this);  // negate operator==
StRun::operator!=(const StRun& r) const
{
void StRun::setId(Long_t val) { mId = val; }
    
void StRun::setType(const Char_t* val) { mType = val; }
    
void StRun::setTriggerMask(Long_t val) { mTriggerMask = val; }

void StRun::setCenterOfMassEnergy(Double_t val) { mCenterOfMassEnergy = val; }

void StRun::setBeamMassNumber(StBeamDirection dir, Short_t val)

void
	mEastA = val;
{
	mWestA = val;
        mEastA = val;
    else
void StRun::setBeamCharge(StBeamDirection dir, Short_t val)

void
	mEastZ = val;
{
	mWestZ = val;
        mEastZ = val;
    else
void StRun::setSummary(StRunSummary* val) { mSummary = val; }
const StRunSummary*
StRun::summary() const { return mSummary; }

