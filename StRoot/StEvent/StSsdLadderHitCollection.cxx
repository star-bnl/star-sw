/***************************************************************************
 *
 * $Id: StSsdLadderHitCollection.cxx,v 2.2 2001/04/05 04:00:55 ullrich Exp $
 *
 * Author: Lilian Martin, Thomas Ullrich, Dec 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StSsdLadderHitCollection.cxx,v $
 * Revision 2.2  2001/04/05 04:00:55  ullrich
 * Replaced all (U)Long_t by (U)Int_t and all redundant ROOT typedefs.
 *
 * Revision 2.1  2000/01/05 16:00:08  ullrich
 * Initial Revision.
 *
 **************************************************************************/
#include "StSsdLadderHitCollection.h"

static const char rcsid[] = "$Id: StSsdLadderHitCollection.cxx,v 2.2 2001/04/05 04:00:55 ullrich Exp $";

ClassImp(StSsdLadderHitCollection)

StSsdLadderHitCollection::StSsdLadderHitCollection() { /* noop */ }

StSsdLadderHitCollection::~StSsdLadderHitCollection() { /* noop */ }

unsigned int
StSsdLadderHitCollection::numberOfWafers() const {return mMaxNumberOfWafers;}

unsigned int
StSsdLadderHitCollection::numberOfHits() const
{
    unsigned int sum = 0;
    for (unsigned int j=0; j<numberOfWafers(); j++) {
        sum += mWafers[j].hits().size();
    }
    return sum;
}

StSsdWaferHitCollection*
StSsdLadderHitCollection::wafer(unsigned int i)
{
    if (i < numberOfWafers())
        return &(mWafers[i]);
    else
        return 0;
}

const StSsdWaferHitCollection*
StSsdLadderHitCollection::wafer(unsigned int i) const
{
    if (i < numberOfWafers())
        return &(mWafers[i]);
    else
        return 0;
}
    
