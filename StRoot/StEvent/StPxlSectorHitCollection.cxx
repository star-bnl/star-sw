/***************************************************************************
 *
 * $Id: StPxlSectorHitCollection.cxx,v 2.1 2013/03/05 14:40:41 ullrich Exp $
 *
 * Author: X. Dong, Jan 2013
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StPxlSectorHitCollection.cxx,v $
 * Revision 2.1  2013/03/05 14:40:41  ullrich
 * Initial Revision.
 *
 **************************************************************************/
#include "StPxlSectorHitCollection.h"
#include "StPxlHit.h"

ClassImp(StPxlSectorHitCollection)

StPxlSectorHitCollection::StPxlSectorHitCollection() { /* noop */ }

StPxlSectorHitCollection::~StPxlSectorHitCollection() { /* noop */ }

StPxlLadderHitCollection* 
StPxlSectorHitCollection::ladder(unsigned int i)
{
    return (i < mNumberOfLadders) ? &(mLadders[i]) : 0;
}

const StPxlLadderHitCollection* 
StPxlSectorHitCollection::ladder(unsigned int i) const
{ 
    return (i < mNumberOfLadders) ? &(mLadders[i]) : 0;
}

unsigned int StPxlSectorHitCollection::numberOfHits() const
{
    unsigned int sum = 0;
    for ( int i=0; i < mNumberOfLadders; i++) {
        for ( int j=0; j < mLadders[i].numberOfSensors(); j++) {
            sum += mLadders[i].sensor(j)->hits().size();
        }
    }
    return sum;
}
