/***************************************************************************
 *
 * $Id: StMcPixelLayerHitCollection.cc,v 2.1 2003/08/20 18:50:21 calderon Exp $
 *
 * Author: Fabrice Retiere/Kai Schweda, Aug 2003
 ***************************************************************************
 *
 * Description: Monte Carlo Pixel Layer Hit Collection class
 *
 ***************************************************************************
 *
 * $Log: StMcPixelLayerHitCollection.cc,v $
 * Revision 2.1  2003/08/20 18:50:21  calderon
 * Addition of Tof classes and Pixel classes.  Modified track, event, and
 * container code to reflect this.
 * Fix bug in StMcVertex and in clearing of some hit collections.
 *
 *
 *
 **************************************************************************/
#include "StMcPixelLayerHitCollection.hh"
#include "StMcPixelHit.hh"
static const char rcsid[] = "$Id: StMcPixelLayerHitCollection.cc,v 2.1 2003/08/20 18:50:21 calderon Exp $";

#ifdef PERSISTENT
ClassImp(StMcPixelLayerHitCollection)
#endif

StMcPixelLayerHitCollection::StMcPixelLayerHitCollection() { /* noop */ }

StMcPixelLayerHitCollection::~StMcPixelLayerHitCollection()
{
    // If the hit provides its own new/delete operator.
    for (unsigned int i=0; i<mHits.size(); i++) {
	delete mHits[i];
	mHits[i]=0;
  }
}

const StSPtrVecMcPixelHit&
StMcPixelLayerHitCollection::hits() const { return mHits; }

StSPtrVecMcPixelHit&
StMcPixelLayerHitCollection::hits() { return mHits; }

unsigned long StMcPixelLayerHitCollection::numberOfHits() const
{
  return mHits.size();
}
