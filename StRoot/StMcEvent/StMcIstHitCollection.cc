/***************************************************************************
 *
 * $Id: StMcIstHitCollection.cc,v 2.3 2015/03/12 23:18:06 perev Exp $
 *
 * Author: Manuel Calderon de la Barca Sanchez, Oct 1999
 ***************************************************************************
 *
 * Description: Monte Carlo Pixel Hit Collection class
 *
 ***************************************************************************
 *
 * Switch layers to ladders (Amilkar)
 *
 * $Log: StMcIstHitCollection.cc,v $
 * Revision 2.3  2015/03/12 23:18:06  perev
 * Switch layers to ladders (Amilkar)
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

static const char rcsid[] = "$Id: StMcIstHitCollection.cc,v 2.3 2015/03/12 23:18:06 perev Exp $";

ClassImp(StMcIstHitCollection)

StMcIstHitCollection::StMcIstHitCollection() { /* noop */ }

StMcIstHitCollection::~StMcIstHitCollection() { /* noop */ }
    
bool
StMcIstHitCollection::addHit(StMcIstHit* hit)
{
  unsigned int p, w;
  if (hit && ((p = hit->ladder()-1) < mNumberOfLadders) &&
              (w = hit->sensor()-1) < ladder(p)->numberOfSensors()) {
    //mLadders[p].hits().push_back(hit);
    ladder(p)->sensor(w)->hits().push_back(hit);
    return true;
    
  }
    else
      return false;
}

unsigned int
StMcIstHitCollection::numberOfLadders() const { return mNumberOfLadders; }

unsigned long
StMcIstHitCollection::numberOfHits() const
{
    unsigned long sum = 0;
    for (int i=0; i<mNumberOfLadders; i++)
      sum += mLadders[i].numberOfHits();

    return sum;
}

StMcIstLadderHitCollection*
StMcIstHitCollection::ladder(unsigned int i)
{
    if (i < mNumberOfLadders)
        return &(mLadders[i]);
    else
        return 0;
}

const StMcIstLadderHitCollection*
StMcIstHitCollection::ladder(unsigned int i) const
{
    if (i < mNumberOfLadders)
        return &(mLadders[i]);
    else
        return 0;
}
