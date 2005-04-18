/***************************************************************************
 *
 * $Id: StMcFstLayerHitCollection.cc,v 2.1 2005/04/18 20:11:33 calderon Exp $
 *
 * Author: Fabrice Retiere/Kai Schweda, Aug 2003
 ***************************************************************************
 *
 * Description: Monte Carlo Fst Layer Hit Collection class
 *
 ***************************************************************************
 *
 * $Log: StMcFstLayerHitCollection.cc,v $
 * Revision 2.1  2005/04/18 20:11:33  calderon
 * Addition of Fgt and Fst files.  Modified other files to accomodate changes.
 *
 *
 *
 **************************************************************************/
#include "StMcFstLayerHitCollection.hh"
#include "StMcFstHit.hh"
static const char rcsid[] = "$Id: StMcFstLayerHitCollection.cc,v 2.1 2005/04/18 20:11:33 calderon Exp $";

ClassImp(StMcFstLayerHitCollection)

StMcFstLayerHitCollection::StMcFstLayerHitCollection() { /* noop */ }

StMcFstLayerHitCollection::~StMcFstLayerHitCollection()
{
    // If the hit provides its own new/delete operator.
    for (unsigned int i=0; i<mHits.size(); i++) {
	delete mHits[i];
	mHits[i]=0;
  }
}

const StSPtrVecMcFstHit&
StMcFstLayerHitCollection::hits() const { return mHits; }

StSPtrVecMcFstHit&
StMcFstLayerHitCollection::hits() { return mHits; }

unsigned long StMcFstLayerHitCollection::numberOfHits() const
{
  return mHits.size();
}
