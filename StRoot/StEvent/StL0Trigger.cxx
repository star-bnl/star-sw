/***************************************************************************
 *
 * $Id: StL0Trigger.cxx,v 2.2 1999/12/21 15:08:59 ullrich Exp $
 *
 * Author: Thomas Ullrich, Sep 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StL0Trigger.cxx,v $
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
#if !defined(ST_NO_NAMESPACES)
using std::fill_n;
using std::copy;
#endif

static const char rcsid[] = "$Id: StL0Trigger.cxx,v 2.2 1999/12/21 15:08:59 ullrich Exp $";

ClassImp(StL0Trigger)

StL0Trigger::StL0Trigger()
{
    mMwcCtbMultiplicity = 0;
    mMwcCtbDipole = 0;
    mMwcCtbTopology = 0;
    mMwcCtbMoment = 0;
    fill_n(mCoarsePixelArray, static_cast<int>(mMaxPixels), 0);
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
}

StL0Trigger::~StL0Trigger() { /* noop */ }

UInt_t
StL0Trigger::coarsePixelArraySize() {return mMaxPixels;}

Long_t
StL0Trigger::coarsePixelArray(UInt_t i)
{
    if (i < mMaxPixels)
        return mCoarsePixelArray[i];
    else
        return 0;
}

Long_t
StL0Trigger::mwcCtbMultiplicity() const { return mMwcCtbMultiplicity;}

Long_t
StL0Trigger::mwcCtbDipole() const { return mMwcCtbDipole;}

Long_t
StL0Trigger::mwcCtbTopology() const { return mMwcCtbTopology;}

Long_t
StL0Trigger::mwcCtbMoment() const { return mMwcCtbMoment;}

void
StL0Trigger::setMwcCtbMultiplicity(Long_t val) { mMwcCtbMultiplicity = val; }

void
StL0Trigger::setMwcCtbDipole(Long_t val) { mMwcCtbDipole = val; }
   
void
StL0Trigger::setMwcCtbTopology(Long_t val) { mMwcCtbTopology = val; }

void
StL0Trigger::setMwcCtbMoment(Long_t val) { mMwcCtbMoment = val; }

void
StL0Trigger::setCoarsePixelArray(UInt_t i, Long_t val)
{
    if (i < mMaxPixels)
        mCoarsePixelArray[i] = val;
}
