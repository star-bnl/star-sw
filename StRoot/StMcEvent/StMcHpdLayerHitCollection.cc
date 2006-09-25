/***************************************************************************
 * First version of StMcHpdLayerHitCollection
 **************************************************************************/
#include "StMcHpdLayerHitCollection.hh"
#include "StMcHpdHit.hh"
static const char rcsid[] = "$Id: StMcHpdLayerHitCollection.cc,v 2.1 2006/09/25 14:20:43 fisyak Exp $";

ClassImp(StMcHpdLayerHitCollection)

StMcHpdLayerHitCollection::StMcHpdLayerHitCollection() { /* noop */ }

StMcHpdLayerHitCollection::~StMcHpdLayerHitCollection()
{
    // If the hit provides its own new/delete operator.
    for (unsigned int i=0; i<mHits.size(); i++) {
	delete mHits[i];
	mHits[i]=0;
  }
}

const StSPtrVecMcHpdHit&
StMcHpdLayerHitCollection::hits() const { return mHits; }

StSPtrVecMcHpdHit&
StMcHpdLayerHitCollection::hits() { return mHits; }

unsigned long StMcHpdLayerHitCollection::numberOfHits() const
{
  return mHits.size();
}
