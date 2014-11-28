/***************************************************************************
 *
 * $Id: StL1Trigger.cxx,v 2.1 2001/11/07 21:18:17 ullrich Exp $
 *
 * Author: Thomas Ullrich, Nov 2001
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StL1Trigger.cxx,v $
 * Revision 2.1  2001/11/07 21:18:17  ullrich
 * Initial Revision.
 *
 **************************************************************************/
#include "StL1Trigger.h"
#include "tables/St_dst_L0_Trigger_Table.h"
#include "tables/St_dst_L1_Trigger_Table.h"

static const char rcsid[] = "$Id: StL1Trigger.cxx,v 2.1 2001/11/07 21:18:17 ullrich Exp $";

ClassImp(StL1Trigger)

StL1Trigger::StL1Trigger() 
{
    mTriggerWordPrime = 0;
}

StL1Trigger::StL1Trigger(const dst_L0_Trigger_st& t1, const dst_L1_Trigger_st& t2)
{
    mTriggerWord        = t1.TriggerWd;        // inheritance legacy
    mTriggerActionWord  = t1.TriggerActionWd;  // inheritance legacy
    mTriggerWordPrime   = t2.L1_result[1];
}

StL1Trigger::~StL1Trigger() { /* noop */ }

unsigned int
StL1Trigger::triggerWordPrime() const {return mTriggerWordPrime;}
    
void
StL1Trigger::setTriggerWordPrime(unsigned int val) {mTriggerWordPrime = val;}
