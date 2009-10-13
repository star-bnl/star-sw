/***************************************************************************
 *
 * $Id: StMcFgtHitCollection.cc,v 2.2 2009/10/13 19:14:27 perev Exp $
 *
 * Authors: Kai Schweda (FGT Software)
 * Manuel Calderon de la Barca Sanchez (StMcEvent), Apr 2005
 ***************************************************************************
 *
 * Description: Monte Carlo Forward Gem Tracker (FGT) Hit Collection class
 *
 ***************************************************************************
 *
 * $Log: StMcFgtHitCollection.cc,v $
 * Revision 2.2  2009/10/13 19:14:27  perev
 * Wei-Ming update
 *
 * Revision 2.1  2005/04/18 20:11:33  calderon
 * Addition of Fgt and Fst files.  Modified other files to accomodate changes.
 *
 *
 **************************************************************************/
#include "StMcFgtHitCollection.hh"
#include "StMcFgtHit.hh"

static const char rcsid[] = "$Id: StMcFgtHitCollection.cc,v 2.2 2009/10/13 19:14:27 perev Exp $";

ClassImp(StMcFgtHitCollection)

StMcFgtHitCollection::StMcFgtHitCollection() { /* noop */ }

StMcFgtHitCollection::~StMcFgtHitCollection() { /* noop */ }
    
bool
StMcFgtHitCollection::addHit(StMcFgtHit* hit)
{
    unsigned int p;
    if (hit && (p = hit->layer()) < mNumberOfLayers) { // layer = disk WMZ
      mLayers[p].hits().push_back(hit);
      return true;
    }
    else
      return false;
}

unsigned int
StMcFgtHitCollection::numberOfLayers() const { return mNumberOfLayers; }

unsigned long
StMcFgtHitCollection::numberOfHits() const
{
    unsigned long sum = 0;
    for (int i=0; i<mNumberOfLayers; i++)
      sum += mLayers[i].numberOfHits();

    return sum;
}

StMcFgtLayerHitCollection*
StMcFgtHitCollection::layer(unsigned int i)
{
    if (i < mNumberOfLayers)
        return &(mLayers[i]);
    else
        return 0;
}

const StMcFgtLayerHitCollection*
StMcFgtHitCollection::layer(unsigned int i) const
{
    if (i < mNumberOfLayers)
        return &(mLayers[i]);
    else
        return 0;
}
