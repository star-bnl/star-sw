/***************************************************************************
 *
 * $Id: StVpdCounter.cxx,v 1.2 1999/02/09 19:53:20 fisyak Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StVpdCounter.cxx,v $
 * Revision 1.2  1999/02/09 19:53:20  fisyak
 * Import new Torre staff
 *
 * Revision 1.4  1999/04/28 22:27:40  fisyak
 * New version with pointer instead referencies
 *
 * Revision 1.2  1999/01/15 22:54:24  wenaus
 * version with constructors for table-based loading
 *
 **************************************************************************/
#include "StVpdCounter.h"
#ifdef __ROOT__

static const Char_t rcsid[] = "$Id: StVpdCounter.cxx,v 1.2 1999/02/09 19:53:20 fisyak Exp $";
#endif
ClassImp(StVpdCounter)

StCollectionImp(VpdCounter)
StVpdCounter::StVpdCounter()
{
    mId = 0;  
    mAdc = 0;
    mTime = 0;    
}

StVpdCounter::StVpdCounter(Short_t id, Float_t a, Float_t t)
{
    mId = id;  
    mAdc = a;
    mTime = t;    
}

StVpdCounter::~StVpdCounter() { /* noop */ }

void StVpdCounter::setId(Short_t val) { mId = val; }

void StVpdCounter::setAdc(Float_t val) { mAdc = val; }

void StVpdCounter::setTime(Float_t val) { mTime = val; }
