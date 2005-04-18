/***************************************************************************
 *
 * $Id: StMcFstHitCollection.cc,v 2.1 2005/04/18 20:11:33 calderon Exp $
 *
 * Author: Manuel Calderon de la Barca Sanchez, Apr 2005
 ***************************************************************************
 *
 * Description: Monte Carlo Forward Silicon Tracker Hit Collection class
 *
 ***************************************************************************
 *
 * $Log: StMcFstHitCollection.cc,v $
 * Revision 2.1  2005/04/18 20:11:33  calderon
 * Addition of Fgt and Fst files.  Modified other files to accomodate changes.
 *
 *
 *
 **************************************************************************/
#include "StMcFstHitCollection.hh"
#include "StMcFstHit.hh"

static const char rcsid[] = "$Id: StMcFstHitCollection.cc,v 2.1 2005/04/18 20:11:33 calderon Exp $";

ClassImp(StMcFstHitCollection)

StMcFstHitCollection::StMcFstHitCollection() { /* noop */ }

StMcFstHitCollection::~StMcFstHitCollection() { /* noop */ }
    
bool
StMcFstHitCollection::addHit(StMcFstHit* hit)
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
StMcFstHitCollection::numberOfLayers() const { return mNumberOfLayers; }

unsigned long
StMcFstHitCollection::numberOfHits() const
{
    unsigned long sum = 0;
    for (int i=0; i<mNumberOfLayers; i++)
      sum += mLayers[i].numberOfHits();

    return sum;
}

StMcFstLayerHitCollection*
StMcFstHitCollection::layer(unsigned int i)
{
    if (i < mNumberOfLayers)
        return &(mLayers[i]);
    else
        return 0;
}

const StMcFstLayerHitCollection*
StMcFstHitCollection::layer(unsigned int i) const
{
    if (i < mNumberOfLayers)
        return &(mLayers[i]);
    else
        return 0;
}
