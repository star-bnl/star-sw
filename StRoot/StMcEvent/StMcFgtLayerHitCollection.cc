/***************************************************************************
 *
 * $Id: StMcFgtLayerHitCollection.cc,v 2.1 2005/04/18 20:11:33 calderon Exp $
 *
 * Author: Fabrice Retiere/Kai Schweda, Aug 2003
 ***************************************************************************
 *
 * Description: Monte Carlo Fgt Layer Hit Collection class
 *
 ***************************************************************************
 *
 * $Log: StMcFgtLayerHitCollection.cc,v $
 * Revision 2.1  2005/04/18 20:11:33  calderon
 * Addition of Fgt and Fst files.  Modified other files to accomodate changes.
 *
 *
 *
 **************************************************************************/
#include "StMcFgtLayerHitCollection.hh"
#include "StMcFgtHit.hh"
static const char rcsid[] = "$Id: StMcFgtLayerHitCollection.cc,v 2.1 2005/04/18 20:11:33 calderon Exp $";

ClassImp(StMcFgtLayerHitCollection)

StMcFgtLayerHitCollection::StMcFgtLayerHitCollection() { /* noop */ }

StMcFgtLayerHitCollection::~StMcFgtLayerHitCollection()
{
    // If the hit provides its own new/delete operator.
    for (unsigned int i=0; i<mHits.size(); i++) {
	delete mHits[i];
	mHits[i]=0;
    }
}

const StSPtrVecMcFgtHit&
StMcFgtLayerHitCollection::hits() const { return mHits; }

StSPtrVecMcFgtHit&
StMcFgtLayerHitCollection::hits() { return mHits; }

unsigned long StMcFgtLayerHitCollection::numberOfHits() const
{
  return mHits.size();
}
