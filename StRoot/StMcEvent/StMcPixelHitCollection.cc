/***************************************************************************
 *
 * $Id: StMcPixelHitCollection.cc,v 2.1 2003/08/20 18:50:21 calderon Exp $
 *
 * Author: Manuel Calderon de la Barca Sanchez, Oct 1999
 ***************************************************************************
 *
 * Description: Monte Carlo Pixel Hit Collection class
 *
 ***************************************************************************
 *
 * $Log: StMcPixelHitCollection.cc,v $
 * Revision 2.1  2003/08/20 18:50:21  calderon
 * Addition of Tof classes and Pixel classes.  Modified track, event, and
 * container code to reflect this.
 * Fix bug in StMcVertex and in clearing of some hit collections.
 *
 * Revision 2.1  1999/12/14 07:04:49  calderon
 * Numbering scheme as per SVT request.
 *
 * Revision 2.0  1999/11/17 02:00:59  calderon
 * Completely revised for new StEvent
 *
 *
 **************************************************************************/
#include "StMcPixelHitCollection.hh"
#include "StMcPixelHit.hh"

static const char rcsid[] = "$Id: StMcPixelHitCollection.cc,v 2.1 2003/08/20 18:50:21 calderon Exp $";

#ifdef PERSISTENT
ClassImp(StMcPixelHitCollection)
#endif

StMcPixelHitCollection::StMcPixelHitCollection() { /* noop */ }

StMcPixelHitCollection::~StMcPixelHitCollection() { /* noop */ }
    
bool
StMcPixelHitCollection::addHit(StMcPixelHit* hit)
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
StMcPixelHitCollection::numberOfLayers() const { return mNumberOfLayers; }

unsigned long
StMcPixelHitCollection::numberOfHits() const
{
    unsigned long sum = 0;
    for (int i=0; i<mNumberOfLayers; i++)
      sum += mLayers[i].numberOfHits();

    return sum;
}

StMcPixelLayerHitCollection*
StMcPixelHitCollection::layer(unsigned int i)
{
    if (i < mNumberOfLayers)
        return &(mLayers[i]);
    else
        return 0;
}

const StMcPixelLayerHitCollection*
StMcPixelHitCollection::layer(unsigned int i) const
{
    if (i < mNumberOfLayers)
        return &(mLayers[i]);
    else
        return 0;
}
