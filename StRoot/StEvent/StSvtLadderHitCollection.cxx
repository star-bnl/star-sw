/***************************************************************************
 *
 * $Id: StSvtLadderHitCollection.cxx,v 2.2 1999/10/28 22:26:50 ullrich Exp $
 *
 * Author: Thomas Ullrich, Sep 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StSvtLadderHitCollection.cxx,v $
 * Revision 2.2  1999/10/28 22:26:50  ullrich
 * Adapted new StArray version. First version to compile on Linux and Sun.
 *
 * Revision 2.2  1999/10/28 22:26:50  ullrich
 * Adapted new StArray version. First version to compile on Linux and Sun.
 *
 * Revision 2.1  1999/10/13 19:45:16  ullrich
 * Initial Revision
 *
 **************************************************************************/
#include "StSvtLadderHitCollection.h"

static const char rcsid[] = "$Id: StSvtLadderHitCollection.cxx,v 2.2 1999/10/28 22:26:50 ullrich Exp $";

ClassImp(StSvtLadderHitCollection)

StSvtLadderHitCollection::StSvtLadderHitCollection()
{
    mLayerNumber = -1;
}

StSvtLadderHitCollection::~StSvtLadderHitCollection() { /* noop */ }

void
StSvtLadderHitCollection::setLayerNumber(Int_t i)
{
    if (mLayerNumber == -1) mLayerNumber = i;
}
    
UInt_t
StSvtLadderHitCollection::numberOfWafers() const
{
    switch (mLayerNumber) {
    case 0:
    case 1:
        return 4;
        break;
    case 2:
    case 3:
        return 6;
        break;
    case 4:
    case 5:
        return 7;
        break;
    default:
        return 0;
    }
}

ULong_t
StSvtLadderHitCollection::numberOfHits() const
{
    ULong_t sum = 0;
    for (unsigned int j=0; j<numberOfWafers(); j++) {
        sum += mWafers[j].hits().size();
    }
    return sum;
}

StSvtWaferHitCollection*
StSvtLadderHitCollection::wafer(UInt_t i)
{
    if (i < numberOfWafers())
        return &(mWafers[i]);
    else
        return 0;
}

const StSvtWaferHitCollection*
StSvtLadderHitCollection::wafer(UInt_t i) const
{
    if (i < numberOfWafers())
        return &(mWafers[i]);
    else
        return 0;
}
    
