/***************************************************************************
 *
 * $Id: StZdcSegment.cc,v 1.2 1999/01/15 22:54:27 wenaus Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StZdcSegment.cc,v $
 * Revision 1.2  1999/01/15 22:54:27  wenaus
 * version with constructors for table-based loading
 *
 **************************************************************************/
#include "StEvent/StZdcSegment.hh"

static const char rcsid[] = "$Id: StZdcSegment.cc,v 1.2 1999/01/15 22:54:27 wenaus Exp $";

StZdcSegment::StZdcSegment()
{
    mId = 0;  
    mAdc = 0;
    mTdc = 0;    
}

StZdcSegment::StZdcSegment(short id, float a, float t)
{
    mId = id;  
    mAdc = a;
    mTdc = t;    
}

StZdcSegment::~StZdcSegment() { /* noop */ }

void StZdcSegment::setId(short val) { mId = val; }

void StZdcSegment::setAdc(float val) { mAdc = val; }

void StZdcSegment::setTdc(float val) { mTdc = val; }
