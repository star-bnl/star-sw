/***************************************************************************
 *
 * $Id: StL0Trigger.cxx,v 2.3 2001/04/05 04:00:51 ullrich Exp $
 *
 * Author: Thomas Ullrich, Sep 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StL0Trigger.cxx,v $
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
#if !defined(ST_NO_NAMESPACES)
using std::fill_n;
using std::copy;
#endif

static const char rcsid[] = "$Id: StL0Trigger.cxx,v 2.3 2001/04/05 04:00:51 ullrich Exp $";

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

int
StL0Trigger::mwcCtbMultiplicity() const { return mMwcCtbMultiplicity;}

int
StL0Trigger::mwcCtbDipole() const { return mMwcCtbDipole;}

int
StL0Trigger::mwcCtbTopology() const { return mMwcCtbTopology;}

int
StL0Trigger::mwcCtbMoment() const { return mMwcCtbMoment;}

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
