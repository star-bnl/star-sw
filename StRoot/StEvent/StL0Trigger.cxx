/***************************************************************************
 *
 * $Id: StL0Trigger.cxx,v 1.3 1999/04/27 01:24:21 fisyak Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StL0Trigger.cxx,v $
 * Revision 1.3  1999/04/27 01:24:21  fisyak
 * Fix intermidaiate version with pointer instead of referencies
 *
 * Revision 1.4  1999/04/28 22:27:33  fisyak
 * New version with pointer instead referencies
 *
 * Revision 1.2  1999/01/15 22:53:47  wenaus
 * version with constructors for table-based loading
 *
 * Revision 2.1  1999/10/28 22:25:59  ullrich
 *
#ifdef __ROOT__
 * Completely Revised for New Version
static const Char_t rcsid[] = "$Id: StL0Trigger.cxx,v 1.3 1999/04/27 01:24:21 fisyak Exp $";
#endif
#include "tables/dst_L0_Trigger.h"
#include <algorithm>
#include "StL0Trigger.h"
#include "tables/St_dst_L0_Trigger_Table.h"

static const char rcsid[] = "$Id: StL0Trigger.cxx,v 1.3 1999/04/27 01:24:21 fisyak Exp $";

ClassImp(StL0Trigger)

    mMwcCtbMultiplicity = 0;
    mTriggerActionWord  = t.TriggerActionWd;
    mMwcCtbMultiplicity = t.MWC_CTB_mul;
    mMwcCtbDipole       = t.MWC_CTB_dipole;
    mMwcCtbTopology     = t.MWC_CTB_topology;
void StL0Trigger::setMwcCtbMultiplicity(Long_t val) { mMwcCtbMultiplicity = val; } 

void StL0Trigger::setMwcCtbDipole(Long_t val) { mMwcCtbDipole = val; }            

void StL0Trigger::setMwcCtbTopology(Long_t val) { mMwcCtbTopology = val; } 

void StL0Trigger::setMwcCtbMoment(Long_t val) { mMwcCtbMoment = val; }
void
StL0Trigger::setCoarsePixelArray(UInt_t i, Long_t val)
{
    if (i < mMaxPixels)
        mCoarsePixelArray[i] = val;
}
