/***************************************************************************
 *
 * $Id: StVpdCounter.cc,v 1.2 1999/01/15 22:54:24 wenaus Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StVpdCounter.cc,v $
 * Revision 1.2  1999/01/15 22:54:24  wenaus
 * version with constructors for table-based loading
 *
 **************************************************************************/
#include "StEvent/StVpdCounter.hh"

static const char rcsid[] = "$Id: StVpdCounter.cc,v 1.2 1999/01/15 22:54:24 wenaus Exp $";

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
