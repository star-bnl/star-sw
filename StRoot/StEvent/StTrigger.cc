/***************************************************************************
 *
 * $Id: StTrigger.cc,v 1.1 1999/01/15 20:40:15 wenaus Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StTrigger.cc,v $
 * Revision 1.1  1999/01/15 20:40:15  wenaus
 * Commit Thomas' original code
 *
 **************************************************************************/
#include "StTrigger.hh"

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
