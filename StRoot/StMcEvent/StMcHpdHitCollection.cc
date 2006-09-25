/***************************************************************************
 * First version of hpd hit collection
 **************************************************************************/
#include "StMcHpdHitCollection.hh"
#include "StMcHpdHit.hh"

static const char rcsid[] = "$Id: StMcHpdHitCollection.cc,v 2.1 2006/09/25 14:20:43 fisyak Exp $";

ClassImp(StMcHpdHitCollection)

StMcHpdHitCollection::StMcHpdHitCollection() { /* noop */ }

StMcHpdHitCollection::~StMcHpdHitCollection() { /* noop */ }
    
bool
StMcHpdHitCollection::addHit(StMcHpdHit* hit)
{
    unsigned int p;
       if (hit && (p = hit->layer()-1) < mNumberOfLayers) {
      mLayers[p].hits().push_back(hit);
      return true;
    }
    else
      return false;
}

unsigned int
StMcHpdHitCollection::numberOfLayers() const { return mNumberOfLayers; }

unsigned long
StMcHpdHitCollection::numberOfHits() const
{
    unsigned long sum = 0;
    for (int i=0; i<mNumberOfLayers; i++)
      sum += mLayers[i].numberOfHits();

    return sum;
}

StMcHpdLayerHitCollection*
StMcHpdHitCollection::layer(unsigned int i)
{
    if (i < mNumberOfLayers)
        return &(mLayers[i]);
    else
        return 0;
}

const StMcHpdLayerHitCollection*
StMcHpdHitCollection::layer(unsigned int i) const
{
    if (i < mNumberOfLayers)
        return &(mLayers[i]);
    else
        return 0;
}
