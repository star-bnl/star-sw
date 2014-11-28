/***************************************************************************
 *
 * $Id: StPxlLadderHitCollection.cxx,v 2.1 2013/03/05 14:40:41 ullrich Exp $
 *
 * Author: X. Dong, Jan 2013
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StPxlLadderHitCollection.cxx,v $
 * Revision 2.1  2013/03/05 14:40:41  ullrich
 * Initial Revision.
 *
 **************************************************************************/
#include "StPxlLadderHitCollection.h"
#include "StPxlHit.h"

ClassImp(StPxlLadderHitCollection)

StPxlLadderHitCollection::StPxlLadderHitCollection() { /* noop */ }

StPxlLadderHitCollection::~StPxlLadderHitCollection() { /* noop */ }

StPxlSensorHitCollection* 
StPxlLadderHitCollection::sensor(unsigned int i)
{
    return (i < mNumberOfSensors) ? &(mSensors[i]) : 0;
}

const StPxlSensorHitCollection* 
StPxlLadderHitCollection::sensor(unsigned int i) const
{ 
    return (i < mNumberOfSensors) ? &(mSensors[i]) : 0;
}

unsigned int StPxlLadderHitCollection::numberOfHits() const
{
    unsigned int sum = 0;
    for ( int i=0; i < mNumberOfSensors; i++) {
        sum += mSensors[i].hits().size();
    }
    return sum;
}
