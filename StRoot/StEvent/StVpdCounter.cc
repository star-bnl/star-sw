/***************************************************************************
 *
 * $Id: StVpdCounter.cc,v 1.1 1999/01/15 20:40:28 wenaus Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StVpdCounter.cc,v $
 * Revision 1.1  1999/01/15 20:40:28  wenaus
 * Commit Thomas' original code
 *
 **************************************************************************/
#include "StVpdCounter.hh"

StVpdCounter::StVpdCounter()
{
    mId = 0;  
    mAdc = 0;
    mTime = 0;    
}

StVpdCounter::StVpdCounter(short id, float a, float t)
{
    mId = id;  
    mAdc = a;
    mTime = t;    
}

StVpdCounter::~StVpdCounter() { /* noop */ }

void StVpdCounter::setId(short val) { mId = val; }

void StVpdCounter::setAdc(float val) { mAdc = val; }

void StVpdCounter::setTime(float val) { mTime = val; }
