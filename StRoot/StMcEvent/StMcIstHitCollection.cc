/***************************************************************************
 *
 * $Id: StMcIstHitCollection.cc,v 2.4 2015/03/13 18:44:58 perev Exp $
 *
 * Author: Manuel Calderon de la Barca Sanchez, Oct 1999
 ***************************************************************************
 *
 * Description: Monte Carlo Pixel Hit Collection class
 *
 ***************************************************************************
 *
 * $Log: StMcIstHitCollection.cc,v $
 * Revision 2.4  2015/03/13 18:44:58  perev
 * Roll back
 *
 * Revision 2.2  2005/05/11 20:54:29  calderon
 * Added persistency: ClassImp, ClassDef and inheritance from StObject.
 *
 * Revision 2.1  2004/09/14 05:00:29  calderon
 * Added support for Ist, Ssd and changes to Pixel, from "El Kai".
 *
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
#include "StMcIstHitCollection.hh"
#include "StMcIstHit.hh"

static const char rcsid[] = "$Id: StMcIstHitCollection.cc,v 2.4 2015/03/13 18:44:58 perev Exp $";

ClassImp(StMcIstHitCollection)

StMcIstHitCollection::StMcIstHitCollection() { /* noop */ }

StMcIstHitCollection::~StMcIstHitCollection() { /* noop */ }
    
bool
StMcIstHitCollection::addHit(StMcIstHit* hit)
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
StMcIstHitCollection::numberOfLayers() const { return mNumberOfLayers; }

unsigned long
StMcIstHitCollection::numberOfHits() const
{
    unsigned long sum = 0;
    for (int i=0; i<mNumberOfLayers; i++)
      sum += mLayers[i].numberOfHits();

    return sum;
}

StMcIstLayerHitCollection*
StMcIstHitCollection::layer(unsigned int i)
{
    if (i < mNumberOfLayers)
        return &(mLayers[i]);
    else
        return 0;
}

const StMcIstLayerHitCollection*
StMcIstHitCollection::layer(unsigned int i) const
{
    if (i < mNumberOfLayers)
        return &(mLayers[i]);
    else
        return 0;
}
