/***************************************************************************
 *
 * $Id: StSvtBarrelHitCollection.cxx,v 2.1 2000/02/17 18:15:09 ullrich Exp $
 *
 * Author: Thomas Ullrich, Feb 2000
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StSvtBarrelHitCollection.cxx,v $
 * Revision 2.1  2000/02/17 18:15:09  ullrich
 * Initial Revision
 *
 **************************************************************************/
#include "StSvtBarrelHitCollection.h"

static const char rcsid[] = "$Id: StSvtBarrelHitCollection.cxx,v 2.1 2000/02/17 18:15:09 ullrich Exp $";

ClassImp(StSvtBarrelHitCollection)

StSvtBarrelHitCollection::StSvtBarrelHitCollection()
{
    mBarrelNumber = -1;
}

StSvtBarrelHitCollection::~StSvtBarrelHitCollection() { /* noop */ }

void
StSvtBarrelHitCollection::setBarrelNumber(Int_t i)
{
    if (mBarrelNumber == -1) mBarrelNumber = i;
}
    
UInt_t
StSvtBarrelHitCollection::numberOfLadders() const
{
    switch (mBarrelNumber) {
    case 0:
        return 8;
        break;
    case 1:
        return 12;
        break;
    case 2:
        return 16;
        break;
    default:
        return 0;
    }
}

ULong_t
StSvtBarrelHitCollection::numberOfHits() const
{
    ULong_t sum = 0;
    for (unsigned int j=0; j<numberOfLadders(); j++) {
        for (unsigned int k=0; k<mLadders[j].numberOfWafers(); k++) {
            sum += mLadders[j].wafer(k)->hits().size();
        }
    }
    return sum;
}

StSvtLadderHitCollection*
StSvtBarrelHitCollection::ladder(UInt_t i)
{
    if (i < numberOfLadders())
        return &(mLadders[i]);
    else
        return 0;
}

const StSvtLadderHitCollection*
StSvtBarrelHitCollection::ladder(UInt_t i) const
{
    if (i < numberOfLadders())
        return &(mLadders[i]);
    else
        return 0;
}
    
