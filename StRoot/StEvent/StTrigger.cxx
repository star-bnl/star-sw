/***************************************************************************
 *
 * $Id: StTrigger.cxx,v 2.2 2001/08/29 18:53:37 ullrich Exp $
 *
 * Author: Thomas Ullrich, Sep 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StTrigger.cxx,v $
 * Revision 2.2  2001/08/29 18:53:37  ullrich
 * Changed trigger words to UInt_t (was UShort_t)
 *
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

StTrigger::StTrigger(unsigned int aw, unsigned int w)
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

unsigned int
StTrigger::triggerActionWord() const { return mTriggerActionWord; }

unsigned int
StTrigger::triggerWord() const { return mTriggerWord; }

void
StTrigger::setTriggerActionWord(unsigned int val) { mTriggerActionWord = val; }

void
StTrigger::setTriggerWord(unsigned int val) { mTriggerWord = val; }
