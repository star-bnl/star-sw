/***************************************************************************
 *
 ***************************************************************************/
#include "StMcEtrHitCollection.hh"
#include "StMcEtrHit.hh"

static const char rcsid[] = "$Id: StMcEtrHitCollection.cc,v 2.1 2012/03/22 01:06:09 perev Exp $";

ClassImp(StMcEtrHitCollection)

StMcEtrHitCollection::StMcEtrHitCollection() { /* noop */ }

StMcEtrHitCollection::~StMcEtrHitCollection()
{ 
    // If the hit provides its own new/delete operator.
    for (unsigned int i=0; i<mHits.size(); i++) {
        delete mHits[i];
        mHits[i]=0;
    }
} 

    
bool
StMcEtrHitCollection::addHit(StMcEtrHit* hit)
{
    unsigned int p;
    if (hit && (p = hit->layer()) < mNumberOfLayers && (p = hit->sector()) < mNumberOfSectors) { 
      mHits.push_back(hit);
      return true;
    }
    else
      return false;
}

unsigned int
StMcEtrHitCollection::numberOfLayers() const { return mNumberOfLayers; }

unsigned int
StMcEtrHitCollection::numberOfSectors() const { return mNumberOfSectors; }

unsigned long
StMcEtrHitCollection::numberOfHits() const
{
  return mHits.size();
}

const StSPtrVecMcEtrHit&
StMcEtrHitCollection::hits() const { return mHits; }

StSPtrVecMcEtrHit&
StMcEtrHitCollection::hits() { return mHits; }

