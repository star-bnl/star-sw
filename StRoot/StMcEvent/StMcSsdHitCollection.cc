/***************************************************************************
 *
 * $Id: StMcSsdHitCollection.cc,v 2.2 2005/01/27 23:40:47 calderon Exp $
 *
 * Author: Manuel Calderon de la Barca Sanchez, Oct 1999
 ***************************************************************************
 *
 * Description: Monte Carlo Ssd Hit Collection class
 *
 ***************************************************************************
 *
 * $Log: StMcSsdHitCollection.cc,v $
 * Revision 2.2  2005/01/27 23:40:47  calderon
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
 * Revision 2.1  1999/12/14 07:04:49  calderon
 * Numbering scheme as per SVT request.
 *
 * Revision 2.0  1999/11/17 02:00:59  calderon
 * Completely revised for new StEvent
 *
 *
 **************************************************************************/
#include "StMcSsdHitCollection.hh"
#include "StMcSsdHit.hh"

static const char rcsid[] = "$Id: StMcSsdHitCollection.cc,v 2.2 2005/01/27 23:40:47 calderon Exp $";

ClassImp(StMcSsdHitCollection)

StMcSsdHitCollection::StMcSsdHitCollection() { /* noop */ }

StMcSsdHitCollection::~StMcSsdHitCollection() { /* noop */ }
    
bool
StMcSsdHitCollection::addHit(StMcSsdHit* hit)
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
StMcSsdHitCollection::numberOfLayers() const { return mNumberOfLayers; }

unsigned long
StMcSsdHitCollection::numberOfHits() const
{
    unsigned long sum = 0;
    for (int i=0; i<mNumberOfLayers; i++)
      sum += mLayers[i].numberOfHits();

    return sum;
}

StMcSsdLayerHitCollection*
StMcSsdHitCollection::layer(unsigned int i)
{
    if (i < mNumberOfLayers)
        return &(mLayers[i]);
    else
        return 0;
}

const StMcSsdLayerHitCollection*
StMcSsdHitCollection::layer(unsigned int i) const
{
    if (i < mNumberOfLayers)
        return &(mLayers[i]);
    else
        return 0;
}
