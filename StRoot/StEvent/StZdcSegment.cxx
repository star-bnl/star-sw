/***************************************************************************
 *
 * $Id: StZdcSegment.cxx,v 1.1 1999/01/30 03:58:10 fisyak Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StZdcSegment.cxx,v $
 * Revision 1.1  1999/01/30 03:58:10  fisyak
 * Root Version of StEvent
 *
 * Revision 1.4  1999/04/28 22:27:41  fisyak
 * New version with pointer instead referencies
 *
 * Revision 1.2  1999/01/15 22:54:27  wenaus
 * version with constructors for table-based loading
 *
 **************************************************************************/
#include "StZdcSegment.h"
#ifdef __ROOT__


#endif
ClassImp(StZdcSegment)
StCollectionImp(ZdcSegment)

StZdcSegment::StZdcSegment()
{
    mId = 0;  
    mAdc = 0;
    mTdc = 0;    
}

StZdcSegment::StZdcSegment(Short_t id, Float_t a, Float_t t)
{
    mId = id;  
    mAdc = a;
    mTdc = t;    
}

StZdcSegment::~StZdcSegment() { /* noop */ }

void StZdcSegment::setId(Short_t val) { mId = val; }

void StZdcSegment::setAdc(Float_t val) { mAdc = val; }

void StZdcSegment::setTdc(Float_t val) { mTdc = val; }
