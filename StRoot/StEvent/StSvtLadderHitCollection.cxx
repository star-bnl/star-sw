/***************************************************************************
 *
 * $Id: StSvtLadderHitCollection.cxx,v 2.4 2001/04/05 04:00:56 ullrich Exp $
 *
 * Author: Thomas Ullrich, Sep 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StSvtLadderHitCollection.cxx,v $
 * Revision 2.4  2001/04/05 04:00:56  ullrich
 * Replaced all (U)Long_t by (U)Int_t and all redundant ROOT typedefs.
 *
 * Revision 2.3  2000/02/17 18:13:16  ullrich
 * Changed the SVT hit storage model. Hits are now stored according
 * to barrel/ladder/wafer not by layer/ladder/wafer.
 *
 * Revision 2.2  1999/10/28 22:26:50  ullrich
 * Adapted new StArray version. First version to compile on Linux and Sun.
 *
 * Revision 2.1  1999/10/13 19:45:16  ullrich
 * Initial Revision
 *
 **************************************************************************/
#include "StSvtLadderHitCollection.h"

static const char rcsid[] = "$Id: StSvtLadderHitCollection.cxx,v 2.4 2001/04/05 04:00:56 ullrich Exp $";

ClassImp(StSvtLadderHitCollection)

StSvtLadderHitCollection::StSvtLadderHitCollection()
{
    mBarrelNumber = -1;
}

StSvtLadderHitCollection::~StSvtLadderHitCollection() { /* noop */ }

void
StSvtLadderHitCollection::setBarrelNumber(int i)
{
    if (mBarrelNumber == -1) mBarrelNumber = i;
}
    
unsigned int
StSvtLadderHitCollection::numberOfWafers() const
{
    switch (mBarrelNumber) {
    case 0:
        return 4;
        break;
    case 1:
        return 6;
        break;
    case 2:
        return 7;
        break;
    default:
        return 0;
    }
}

unsigned int
StSvtLadderHitCollection::numberOfHits() const
{
    unsigned int sum = 0;
    for (unsigned int j=0; j<numberOfWafers(); j++) {
        sum += mWafers[j].hits().size();
    }
    return sum;
}

StSvtWaferHitCollection*
StSvtLadderHitCollection::wafer(unsigned int i)
{
    if (i < numberOfWafers())
        return &(mWafers[i]);
    else
        return 0;
}

const StSvtWaferHitCollection*
StSvtLadderHitCollection::wafer(unsigned int i) const
{
    if (i < numberOfWafers())
        return &(mWafers[i]);
    else
        return 0;
}
    
