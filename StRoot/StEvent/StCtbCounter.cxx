/***************************************************************************
 *
 * $Id: StCtbCounter.cxx,v 1.3 1999/04/27 01:24:15 fisyak Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StCtbCounter.cxx,v $
 * Revision 1.3  1999/04/27 01:24:15  fisyak
 * Fix intermidaiate version with pointer instead of referencies
 *
 * Revision 1.4  1999/04/28 22:27:28  fisyak
 * New version with pointer instead referencies
 *
 * Revision 1.2  1999/01/15 22:53:28  wenaus
 * version with constructors for table-based loading
 *
 **************************************************************************/
#include "StCtbCounter.h"
#ifdef __ROOT__

static const Char_t rcsid[] = "$Id: StCtbCounter.cxx,v 1.3 1999/04/27 01:24:15 fisyak Exp $";
#endif
ClassImp(StCtbCounter)

StCollectionImp(CtbCounter)
StCtbCounter::StCtbCounter()
{
    mId = 0;  
    mMips = 0;
    mTime = 0;    
}

StCtbCounter::StCtbCounter(Short_t id, Float_t m, Float_t t)
{
    mId = id;  
    mMips = m;
    mTime = t;    
}

StCtbCounter::~StCtbCounter() { /* noop */ }

void StCtbCounter::setId(Short_t val) { mId = val; }


StCollectionImp(CtbCounter)
void StCtbCounter::setMips(Float_t val) { mMips = val; }

void StCtbCounter::setTime(Float_t val) { mTime = val; }
