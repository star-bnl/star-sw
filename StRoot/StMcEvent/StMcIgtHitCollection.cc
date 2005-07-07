/*!
 * \class  StMcIgtHitCollection
 * \brief  Monte Carlo Hit Collection class for the GEM Tracker.
 * The Igt collections stores the hits in 2 layers. This class
 * holds 2 containers of hits, one for each layer.
 * \author Gerrit van Nieuwenhuizen, Manuel Calderon de la Barca Sanchez
 * \date   July 2005
 *
 ***************************************************************************
 *
 * $Id: StMcIgtHitCollection.cc,v 2.1 2005/07/07 18:20:49 calderon Exp $
 *
 ***************************************************************************
 *
 * $Log: StMcIgtHitCollection.cc,v $
 * Revision 2.1  2005/07/07 18:20:49  calderon
 * Added support for IGT detector.
 *
 *
 *
 **************************************************************************/
#include "StMcIgtHitCollection.hh"
#include "StMcIgtHit.hh"

static const char rcsid[] = "$Id: StMcIgtHitCollection.cc,v 2.1 2005/07/07 18:20:49 calderon Exp $";

ClassImp(StMcIgtHitCollection)

StMcIgtHitCollection::StMcIgtHitCollection() { /* noop */ }

StMcIgtHitCollection::~StMcIgtHitCollection() { /* noop */ }
    
bool
StMcIgtHitCollection::addHit(StMcIgtHit* hit)
{
    unsigned int p;
    if (hit && (p = hit->layer()-1) < mNumberOfLayers) {
      mLayers[p].hits().push_back(hit);
      return true;
    }
    else {
      return false;
    }
}

unsigned int
StMcIgtHitCollection::numberOfLayers() const 
{ 
    return mNumberOfLayers; 
}

unsigned long
StMcIgtHitCollection::numberOfHits() const
{
    unsigned long sum = 0;
    for (int i=0; i<mNumberOfLayers; i++) {
      sum += mLayers[i].numberOfHits();
    }

    return sum;
}

StMcIgtLayerHitCollection*
StMcIgtHitCollection::layer(unsigned int i)
{
    if (i < mNumberOfLayers) {
        return &(mLayers[i]);
    }
    else {
        return 0;
    }
}

const StMcIgtLayerHitCollection*
StMcIgtHitCollection::layer(unsigned int i) const
{
    if (i < mNumberOfLayers) {
        return &(mLayers[i]);
    }
    else {
        return 0;
    }
}
