/***************************************************************************
 *
 * $Id: StTrigger.cxx,v 2.1 2001/04/05 04:00:59 ullrich Exp $
 *
 * Author: Thomas Ullrich, Sep 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StTrigger.cxx,v $
 * Revision 2.1  2001/04/05 04:00:59  ullrich
 * Replaced all (U)Long_t by (U)Int_t and all redundant ROOT typedefs.
 *
 * Revision 2.0  1999/10/12 18:43:10  ullrich
 * Completely Revised for New Version
 *
 **************************************************************************/
#include "StTrigger.h"

static const char rcsid[] = "$Id";

ClassImp(StTrigger)

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

int
StTrigger::operator==(const StTrigger& t) const
{
    return mTriggerActionWord == t.mTriggerActionWord &&
        mTriggerWord == t.mTriggerWord;
}

int
StTrigger::operator!=(const StTrigger& t) const
{
    return !(t == *this);
}

unsigned short
StTrigger::triggerActionWord() const { return mTriggerActionWord; }

unsigned short
StTrigger::triggerWord() const { return mTriggerWord; }

void
StTrigger::setTriggerActionWord(unsigned short val) { mTriggerActionWord = val; }

void
StTrigger::setTriggerWord(unsigned short val) { mTriggerWord = val; }
