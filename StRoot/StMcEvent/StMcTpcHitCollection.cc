/***************************************************************************
 *
 * $Id: StMcTpcHitCollection.cc,v 2.0 1999/11/17 02:01:00 calderon Exp $
 *
 * Author: Manuel Calderon de la Barca Sanchez, Oct 1999
 ***************************************************************************
 *
 * Description: Monte Carlo Tpc Hit Collection class
 *
 ***************************************************************************
 *
 * $Log: StMcTpcHitCollection.cc,v $
 * Revision 2.0  1999/11/17 02:01:00  calderon
 * Completely revised for new StEvent
 *
 *
 **************************************************************************/
#include "StMcTpcHitCollection.hh"
#include "StMcTpcPadrowHitCollection.hh"
#include "StMcTpcHit.hh"

static const char rcsid[] = "$Id: StMcTpcHitCollection.cc,v 2.0 1999/11/17 02:01:00 calderon Exp $";

#ifdef PERSISTENT
ClassImp(StMcTpcHitCollection)
#endif

StMcTpcHitCollection::StMcTpcHitCollection() { /* noop */ }

StMcTpcHitCollection::~StMcTpcHitCollection() { /* noop */ }
    
bool
StMcTpcHitCollection::addHit(StMcTpcHit* hit)
{
    unsigned int s, r;
    if (hit &&
        (s = hit->sector()) < mNumberOfSectors &&
        (r = hit->padrow()) < mSectors[s].numberOfPadrows()) {
        mSectors[s].padrow(r)->hits().push_back(hit);
        return true;
    }
    else
        return false;
}

unsigned int
StMcTpcHitCollection::numberOfSectors() const { return mNumberOfSectors; }

unsigned long
StMcTpcHitCollection::numberOfHits() const
{
    unsigned long sum = 0;
    for (int i=0; i<mNumberOfSectors; i++) {
	sum += mSectors[i].numberOfHits();
    }
    return sum;
}

StMcTpcSectorHitCollection*
StMcTpcHitCollection::sector(unsigned int i)
{
    if (i < mNumberOfSectors)
        return &(mSectors[i]);
    else
        return 0;
}

const StMcTpcSectorHitCollection*
StMcTpcHitCollection::sector(unsigned int i) const
{
    if (i < mNumberOfSectors)
        return &(mSectors[i]);
    else
        return 0;
}
