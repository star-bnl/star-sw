/***************************************************************************
 *
 * $Id: StL0Trigger.cc,v 1.2 1999/01/15 22:53:47 wenaus Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StL0Trigger.cc,v $
 * Revision 1.2  1999/01/15 22:53:47  wenaus
 * version with constructors for table-based loading
 *
 **************************************************************************/
#include "StEvent/StL0Trigger.hh"

static const char rcsid[] = "$Id: StL0Trigger.cc,v 1.2 1999/01/15 22:53:47 wenaus Exp $";

StL0Trigger::StL0Trigger()
{
    mMwcCtbMultiplicity = 0;
    mMwcCtbDipole = 0;
    mMwcCtbTopology = 0;
    mMwcCtbMoment = 0;

}

StL0Trigger::~StL0Trigger() { /* noop */ }

void StL0Trigger::setMwcCtbMultiplicity(long val) { mMwcCtbMultiplicity = val; } 

void StL0Trigger::setMwcCtbDipole(long val) { mMwcCtbDipole = val; }            

void StL0Trigger::setMwcCtbTopology(long val) { mMwcCtbTopology = val; } 

void StL0Trigger::setMwcCtbMoment(long val) { mMwcCtbMoment = val; }
