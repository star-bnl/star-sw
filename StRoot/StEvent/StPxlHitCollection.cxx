/***************************************************************************
 *
 * $Id: StPxlHitCollection.cxx,v 2.1 2013/03/05 14:40:21 ullrich Exp $
 *
 * Author: X. Dong, Jan 2013
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StPxlHitCollection.cxx,v $
 * Revision 2.1  2013/03/05 14:40:21  ullrich
 * Initial Revision.
 *
 **************************************************************************/
#include "StPxlHitCollection.h"
#include "StPxlSectorHitCollection.h"
#include "StPxlLadderHitCollection.h"
#include "StPxlSensorHitCollection.h"
#include "StPxlHit.h"

ClassImp(StPxlHitCollection)

StPxlHitCollection::StPxlHitCollection() { /* noop */ }

StPxlHitCollection::~StPxlHitCollection() { /* noop */}

bool
StPxlHitCollection::addHit(StPxlHit* hit)
{
    unsigned int s, l, w;
    if (hit &&
        (s = hit->sector()-1) < mNumberOfSectors &&
        (l = hit->ladder()-1) < mSectors[s].numberOfLadders() &&
        (w = hit->sensor()-1) < mSectors[s].ladder(l)->numberOfSensors()) {
        mSectors[s].ladder(l)->sensor(w)->hits().push_back(hit);
        return true;
    }
    else
        return false;
}

unsigned int
StPxlHitCollection::numberOfHits() const
{
    unsigned int sum = 0;
    for (int i=0; i<mNumberOfSectors; i++) {
        for (unsigned int j=0; j<mSectors[i].numberOfLadders(); j++) {
            for (unsigned int k=0; k<mSectors[i].ladder(j)->numberOfSensors(); k++) {
                sum += mSectors[i].ladder(j)->sensor(k)->hits().size();
            }
        }
    }
    return sum;
}

StPxlSectorHitCollection*
StPxlHitCollection::sector(unsigned int i)
{
    return (i < mNumberOfSectors) ? &(mSectors[i]) : 0;
}

const StPxlSectorHitCollection*
StPxlHitCollection::sector(unsigned int i) const
{
    return (i < mNumberOfSectors) ? &(mSectors[i]) : 0;
}
