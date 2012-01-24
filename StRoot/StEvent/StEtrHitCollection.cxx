/***************************************************************************
 *
 * $Id: StEtrHitCollection.cxx,v 2.1 2012/01/24 03:06:12 perev Exp $
 *
 * Author: Ming Shao, Jan 5, 2012
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StEtrHitCollection.cxx,v $
 * Revision 2.1  2012/01/24 03:06:12  perev
 * Add Etr
 *
 *
 * Revision 1.0  2012/01/05 Ming
 * Initial Revision
 *
 **************************************************************************/
#include "StEtrHitCollection.h"
#include "StEtrHit.h"

static const char rcsid[] = "$Id: StEtrHitCollection.cxx,v 2.1 2012/01/24 03:06:12 perev Exp $";

ClassImp(StEtrHitCollection)

StEtrHitCollection::StEtrHitCollection() { /* noop */ }

StEtrHitCollection::~StEtrHitCollection() { /* noop */ }
    
bool
StEtrHitCollection::addHit(StEtrHit* hit)
{
    unsigned int p, s;
    if (hit &&
        (p = hit->layer()) < mNumberOfLayers &&
        (s = hit->sector()) < mNumberOfSectors) {
        mHits.push_back(hit);
        return kTRUE;
    }
    else
        return kFALSE;
}

unsigned int
StEtrHitCollection::numberOfLayers() const { return mNumberOfLayers; }

unsigned int
StEtrHitCollection::numberOfSectors() const { return mNumberOfSectors; }

unsigned int
StEtrHitCollection::numberOfHits() const
{
  return mHits.size();
}

const StSPtrVecEtrHit&
StEtrHitCollection::hits() const { return mHits; }
      
StSPtrVecEtrHit&
StEtrHitCollection::hits() { return mHits; }
