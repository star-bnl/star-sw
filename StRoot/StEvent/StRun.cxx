/***************************************************************************
 *
 * $Id: StRun.cxx,v 1.6 1999/05/04 20:59:25 fisyak Exp $
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
 * Revision 1.6  1999/05/04 20:59:25  fisyak
 * move CVS Tag to StRun
 *
 * Revision 1.6  1999/05/04 20:59:25  fisyak
 * move CVS Tag to StRun
 *
 * Revision 1.5  1999/04/30 13:16:28  fisyak
 * add StArray for StRootEvent
 *
 * Revision 1.4  1999/04/28 22:27:34  fisyak
 * New version with pointer instead referencies
 *
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
static const Char_t rcsid[] = "$Id: StRun.cxx,v 1.6 1999/05/04 20:59:25 fisyak Exp $";
 **************************************************************************/
#include "tables/run_header.h"
static const Char_t rcsid[] = "$Id: StRun.cxx,v 1.6 1999/05/04 20:59:25 fisyak Exp $";

TString StRun::mCvsTag = "$Id: StRun.cxx,v 1.6 1999/05/04 20:59:25 fisyak Exp $";
static const char rcsid[] = "$Id: StRun.cxx,v 1.6 1999/05/04 20:59:25 fisyak Exp $";
  StRun::StRun():
St_DataSet("Run"),
mCVSTag("$Name:  $")
ClassImp(StRun)
    mType = "";

{
    mId = 0;
    mBfcId = 0;
    mTriggerMask = 0;
    mCenterOfMassEnergy = 0;
    mEastA = 0;
    mEastZ = 0;
    mWestZ = 0;
    mSummary = 0;
StRun::StRun(dst_run_header_st& runHdr, dst_run_summary_st& runSum):
St_DataSet("Run"),
mCVSTag("$Name:  $")
}
    mType = runHdr.event_type;
    mId = runHdr.run_id;
    mType  = runHdr.event_type;
    mId    = runHdr.exp_run_id;
    mBfcId = runHdr.bfc_run_id;
    mTriggerMask = runHdr.trig_mask;
    mCenterOfMassEnergy = runHdr.sqrt_s;
    mEastA = runHdr.east_a;
    mEastZ = runHdr.east_z;
    St_DataSet("StRun")
{
StRun::StRun(dst_run_header_st& runHdr)

    mType = runHdr.event_type;
    mId = runHdr.run_id;
    mTriggerMask = runHdr.trig_mask;
    mCenterOfMassEnergy = runHdr.sqrt_s;
    mEastA = runHdr.east_a;
    mWestA = runHdr.west_a;
    mWestZ = runHdr.west_z;
    mSummary = 0;
{
    initFromTable(runHdr);
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

