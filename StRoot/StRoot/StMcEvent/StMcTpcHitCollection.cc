/***************************************************************************
 *
 * $Id: StMcTpcHitCollection.cc,v 2.2 2005/01/27 23:40:49 calderon Exp $
 *
 * Author: Manuel Calderon de la Barca Sanchez, Oct 1999
 ***************************************************************************
 *
 * Description: Monte Carlo Tpc Hit Collection class
 *
 ***************************************************************************
 *
 * $Log: StMcTpcHitCollection.cc,v $
 * Revision 2.2  2005/01/27 23:40:49  calderon
 * Adding persistency to StMcEvent as a step for Virtual MonteCarlo.
 *
 * Revision 2.1  1999/12/14 07:04:49  calderon
 * Numbering scheme as per SVT request.
 *
 * Revision 2.0  1999/11/17 02:01:00  calderon
 * Completely revised for new StEvent
 *
 *
 **************************************************************************/
#include "StMcTpcHitCollection.hh"
#include "StMcTpcPadrowHitCollection.hh"
#include "StMcTpcHit.hh"

static const char rcsid[] = "$Id: StMcTpcHitCollection.cc,v 2.2 2005/01/27 23:40:49 calderon Exp $";

ClassImp(StMcTpcHitCollection)

StMcTpcHitCollection::StMcTpcHitCollection() { /* noop */ }

StMcTpcHitCollection::~StMcTpcHitCollection() { /* noop */ }
    
bool
StMcTpcHitCollection::addHit(StMcTpcHit* hit)
{
    unsigned int s, r;
    if (hit &&
        (s = hit->sector()-1) < mNumberOfSectors &&
        (r = hit->padrow()-1) < mSectors[s].numberOfPadrows()) {
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
