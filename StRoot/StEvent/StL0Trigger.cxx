/***************************************************************************
 *
 * $Id: StL0Trigger.cxx,v 2.4 2001/07/19 00:04:06 ullrich Exp $
 *
 * Author: Thomas Ullrich, Sep 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StL0Trigger.cxx,v $
 * Revision 2.4  2001/07/19 00:04:06  ullrich
 * Updated to handle new trigger info.
 *
 * Revision 2.3  2001/04/05 04:00:51  ullrich
 * Replaced all (U)Long_t by (U)Int_t and all redundant ROOT typedefs.
 *
 * Revision 2.2  1999/12/21 15:08:59  ullrich
 * Modified to cope with new compiler version on Sun (CC5.0).
 *
 * Revision 2.1  1999/10/28 22:25:59  ullrich
 * Adapted new StArray version. First version to compile on Linux and Sun.
 *
 * Revision 2.0  1999/10/12 18:42:24  ullrich
 * Completely Revised for New Version
 *
 **************************************************************************/
#include <algorithm>
#include "StL0Trigger.h"
#include "tables/St_dst_L0_Trigger_Table.h"
#include "tables/St_dst_TrgDet_Table.h"
#if !defined(ST_NO_NAMESPACES)
using std::fill_n;
using std::copy;
#endif

static const char rcsid[] = "$Id: StL0Trigger.cxx,v 2.4 2001/07/19 00:04:06 ullrich Exp $";

ClassImp(StL0Trigger)

StL0Trigger::StL0Trigger() 
{
    mMwcCtbMultiplicity = 0;
    mMwcCtbDipole = 0;
    mMwcCtbTopology = 0;
    mMwcCtbMoment = 0;
    fill_n(mCoarsePixelArray, static_cast<int>(mMaxPixels), 0);
    mDsmInput = 0;
    mDetectorBusy = 0; 
    mTriggerToken = 0;
    mDsmAddress = 0;  
    mAddBits = 0;   
    fill_n(mLastDsmArray, static_cast<unsigned short>(mMaxLastDsm), 0);
    fill_n(mBcDataArray, static_cast<unsigned short>(mMaxBcData), 0);
}

StL0Trigger::StL0Trigger(const dst_L0_Trigger_st& t)
{
    mTriggerWord        = t.TriggerWd;
    mTriggerActionWord  = t.TriggerActionWd;
    mMwcCtbMultiplicity = t.MWC_CTB_mul;
    mMwcCtbDipole       = t.MWC_CTB_dipole;
    mMwcCtbTopology     = t.MWC_CTB_topology;
    mMwcCtbMoment       = t.MWC_CTB_moment;
    copy(t.CPA+0, t.CPA+mMaxPixels, mCoarsePixelArray);
    mDsmInput = t.DSMInput;
    mDetectorBusy = t.DetectorBusy; 
    mTriggerToken = t.TrgToken;
    mDsmAddress = t.DSMAddress;  
    mAddBits = t.addBits;   
    fill_n(mLastDsmArray, static_cast<unsigned short>(mMaxLastDsm), 0);
    fill_n(mBcDataArray, static_cast<unsigned short>(mMaxBcData), 0);
}

StL0Trigger::StL0Trigger(const dst_L0_Trigger_st& t, const dst_TrgDet_st& n)
{
    // from dst_L0_Trigger_st
    mTriggerWord        = t.TriggerWd;
    mTriggerActionWord  = t.TriggerActionWd;
    mMwcCtbMultiplicity = t.MWC_CTB_mul;
    mMwcCtbDipole       = t.MWC_CTB_dipole;
    mMwcCtbTopology     = t.MWC_CTB_topology;
    mMwcCtbMoment       = t.MWC_CTB_moment;
    copy(t.CPA+0, t.CPA+mMaxPixels, mCoarsePixelArray);
    mDsmInput = t.DSMInput;
    mDetectorBusy = t.DetectorBusy; 
    mTriggerToken = t.TrgToken;
    mDsmAddress = t.DSMAddress;  
    mAddBits = t.addBits;
    // from dst_TrgDet_st
    copy(n.lastDSM+0, n.lastDSM+mMaxLastDsm, mLastDsmArray);
    copy(n.BCdata+0, n.BCdata+mMaxBcData, mBcDataArray);
}

StL0Trigger::~StL0Trigger() { /* noop */ }

unsigned int
StL0Trigger::coarsePixelArraySize() {return mMaxPixels;}

int
StL0Trigger::coarsePixelArray(unsigned int i)
{
    if (i < mMaxPixels)
        return mCoarsePixelArray[i];
    else
        return 0;
}

unsigned int
StL0Trigger::lastDsmArraySize() const {return mMaxLastDsm;}

unsigned short
StL0Trigger::lastDsmArray(unsigned int i)
{
    if (i < mMaxLastDsm)
        return mLastDsmArray[i];
    else
        return 0;
}

unsigned int
StL0Trigger::bcDataArraySize() const {return mMaxBcData;}

unsigned short
StL0Trigger::bcDataArray(unsigned int i)
{
    if (i < mMaxBcData)
        return mBcDataArray[i];
    else
        return 0;
}   

int
StL0Trigger::mwcCtbMultiplicity() const { return mMwcCtbMultiplicity;}

int
StL0Trigger::mwcCtbDipole() const { return mMwcCtbDipole;}

int
StL0Trigger::mwcCtbTopology() const { return mMwcCtbTopology;}

int
StL0Trigger::mwcCtbMoment() const { return mMwcCtbMoment;}

unsigned short
StL0Trigger::dsmInput() const {return mDsmInput;}

unsigned char
StL0Trigger::detectorBusy() const {return mDetectorBusy;}

unsigned short
StL0Trigger::triggerToken() const {return mTriggerToken;}

unsigned short
StL0Trigger::dsmAddress() const {return mDsmAddress;}

unsigned char
StL0Trigger::addBits() const {return mAddBits;}

void
StL0Trigger::setMwcCtbMultiplicity(int val) { mMwcCtbMultiplicity = val; }

void
StL0Trigger::setMwcCtbDipole(int val) { mMwcCtbDipole = val; }
   
void
StL0Trigger::setMwcCtbTopology(int val) { mMwcCtbTopology = val; }

void
StL0Trigger::setMwcCtbMoment(int val) { mMwcCtbMoment = val; }

void
StL0Trigger::setCoarsePixelArray(unsigned int i, int val)
{
    if (i < mMaxPixels)
        mCoarsePixelArray[i] = val;
}

void
StL0Trigger::setDsmInput(unsigned short val) {mDsmInput = val;}

void
StL0Trigger::setDetectorBusy(unsigned char val) {mDetectorBusy = val;} 

void
StL0Trigger::setTriggerToken(unsigned short val) {mTriggerToken = val;}

void
StL0Trigger::setDsmAddress(unsigned short val) {mDsmAddress = val;}  

void
StL0Trigger::setAddBits(unsigned char val) {mAddBits = val;}   


void
StL0Trigger::setLastDsmArray(unsigned int i, unsigned short val)
{
    if (i < mMaxLastDsm)
        mLastDsmArray[i] = val;
}

void
StL0Trigger::setBcDataArray(unsigned int i, unsigned short val)
{
    if (i < mMaxBcData)
        mBcDataArray[i] = val;
}
