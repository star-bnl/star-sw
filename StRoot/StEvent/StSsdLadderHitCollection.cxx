/***************************************************************************
 *
 * $Id: StSsdLadderHitCollection.cxx,v 2.1 2000/01/05 16:00:08 ullrich Exp $
 *
 * Author: Lilian Martin, Thomas Ullrich, Dec 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StSsdLadderHitCollection.cxx,v $
 * Revision 2.1  2000/01/05 16:00:08  ullrich
 * Initial Revision.
 *
 **************************************************************************/
#include "StSsdLadderHitCollection.h"

static const char rcsid[] = "$Id: StSsdLadderHitCollection.cxx,v 2.1 2000/01/05 16:00:08 ullrich Exp $";

ClassImp(StSsdLadderHitCollection)

StSsdLadderHitCollection::StSsdLadderHitCollection() { /* noop */ }

StSsdLadderHitCollection::~StSsdLadderHitCollection() { /* noop */ }

UInt_t
StSsdLadderHitCollection::numberOfWafers() const {return mMaxNumberOfWafers;}

ULong_t
StSsdLadderHitCollection::numberOfHits() const
{
    ULong_t sum = 0;
    for (unsigned int j=0; j<numberOfWafers(); j++) {
        sum += mWafers[j].hits().size();
    }
    return sum;
}

StSsdWaferHitCollection*
StSsdLadderHitCollection::wafer(UInt_t i)
{
    if (i < numberOfWafers())
        return &(mWafers[i]);
    else
        return 0;
}

const StSsdWaferHitCollection*
StSsdLadderHitCollection::wafer(UInt_t i) const
{
    if (i < numberOfWafers())
        return &(mWafers[i]);
    else
        return 0;
}
    
