/***************************************************************************
 *
 * $Id: StTrigger.cc,v 1.2 1999/01/15 22:54:11 wenaus Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StTrigger.cc,v $
 * Revision 1.2  1999/01/15 22:54:11  wenaus
 * version with constructors for table-based loading
 *
 **************************************************************************/
#include "StEvent/StTrigger.hh"

static const char rcsid[] = "$Id: StTrigger.cc,v 1.2 1999/01/15 22:54:11 wenaus Exp $";

StTrigger::StTrigger()
{
    mTriggerActionWord = 0;
    mTriggerWord = 0;
}

StTrigger::StTrigger(unsigned short aw, unsigned short w)
{
    mTriggerActionWord = aw;
    mTriggerWord = w;
}

StTrigger::~StTrigger() { /* noop */}

int StTrigger::operator==(const StTrigger& t) const
{
    return mTriggerActionWord == t.mTriggerActionWord &&
	mTriggerWord == t.mTriggerWord;
}

int StTrigger::operator!=(const StTrigger& t) const
{
    return !(t == *this);
}

void StTrigger::setTriggerActionWord(unsigned short val) { mTriggerActionWord = val; }

void StTrigger::setTriggerWord(unsigned short val) { mTriggerActionWord = val; }
