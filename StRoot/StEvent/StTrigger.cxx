/***************************************************************************
 *
 * $Id: StTrigger.cxx,v 2.0 1999/10/12 18:43:10 ullrich Exp $
 *
 * Author: Thomas Ullrich, Sep 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StTrigger.cxx,v $
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

StTrigger::StTrigger(UShort_t aw, UShort_t w)
{
    mTriggerActionWord = aw;
    mTriggerWord = w;
}

StTrigger::~StTrigger() { /* noop */}

Int_t
StTrigger::operator==(const StTrigger& t) const
{
    return mTriggerActionWord == t.mTriggerActionWord &&
        mTriggerWord == t.mTriggerWord;
}

Int_t
StTrigger::operator!=(const StTrigger& t) const
{
    return !(t == *this);
}

UShort_t
StTrigger::triggerActionWord() const { return mTriggerActionWord; }

UShort_t
StTrigger::triggerWord() const { return mTriggerWord; }

void
StTrigger::setTriggerActionWord(UShort_t val) { mTriggerActionWord = val; }

void
StTrigger::setTriggerWord(UShort_t val) { mTriggerWord = val; }
