/***************************************************************************
 *
 * $Id: StZdcSegment.cc,v 1.1 1999/01/15 20:40:30 wenaus Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StZdcSegment.cc,v $
 * Revision 1.1  1999/01/15 20:40:30  wenaus
 * Commit Thomas' original code
 *
 **************************************************************************/
#include "StZdcSegment.hh"

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
