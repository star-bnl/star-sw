/***************************************************************************
 *
 * $Id: StCtbCounter.cc,v 1.1 1999/01/15 20:39:37 wenaus Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StCtbCounter.cc,v $
 * Revision 1.1  1999/01/15 20:39:37  wenaus
 * Commit Thomas' original code
 *
 **************************************************************************/
#include "StCtbCounter.hh"

StCtbCounter::StCtbCounter()
{
    mId = 0;  
    mMips = 0;
    mTime = 0;    
}

StCtbCounter::StCtbCounter(short id, float m, float t)
{
    mId = id;  
    mMips = m;
    mTime = t;    
}

StCtbCounter::~StCtbCounter() { /* noop */ }

void StCtbCounter::setId(short val) { mId = val; }

void StCtbCounter::setMips(float val) { mMips = val; }

void StCtbCounter::setTime(float val) { mTime = val; }
