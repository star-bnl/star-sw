/***************************************************************************
 *
 * $Id: StMcSsdLayerHitCollection.cc,v 2.2 2005/01/27 23:40:48 calderon Exp $
 *
 * Author: Fabrice Retiere/Kai Schweda, Aug 2003
 ***************************************************************************
 *
 * Description: Monte Carlo Ssd Layer Hit Collection class
 *
 ***************************************************************************
 *
 * $Log: StMcSsdLayerHitCollection.cc,v $
 * Revision 2.2  2005/01/27 23:40:48  calderon
 * Adding persistency to StMcEvent as a step for Virtual MonteCarlo.
 *
 * Revision 2.1  2004/09/14 05:00:30  calderon
 * Added support for Ist, Ssd and changes to Pixel, from "El Kai".
 *
 * Revision 2.1  2003/08/20 18:50:21  calderon
 * Addition of Tof classes and Ssd classes.  Modified track, event, and
 * container code to reflect this.
 * Fix bug in StMcVertex and in clearing of some hit collections.
 *
 *
 *
 **************************************************************************/
#include "StMcSsdLayerHitCollection.hh"
#include "StMcSsdHit.hh"
static const char rcsid[] = "$Id: StMcSsdLayerHitCollection.cc,v 2.2 2005/01/27 23:40:48 calderon Exp $";

ClassImp(StMcSsdLayerHitCollection)

StMcSsdLayerHitCollection::StMcSsdLayerHitCollection() { /* noop */ }

StMcSsdLayerHitCollection::~StMcSsdLayerHitCollection()
{
    // If the hit provides its own new/delete operator.
    for (unsigned int i=0; i<mHits.size(); i++) {
	delete mHits[i];
	mHits[i]=0;
  }
}

const StSPtrVecMcSsdHit&
StMcSsdLayerHitCollection::hits() const { return mHits; }

StSPtrVecMcSsdHit&
StMcSsdLayerHitCollection::hits() { return mHits; }

unsigned long StMcSsdLayerHitCollection::numberOfHits() const
{
  return mHits.size();
}
