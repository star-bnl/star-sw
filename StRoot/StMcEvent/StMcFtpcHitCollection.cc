/***************************************************************************
 *
 * $Id: StMcFtpcHitCollection.cc,v 2.2 2005/01/27 23:40:47 calderon Exp $
 *
 * Author: Manuel Calderon de la Barca Sanchez, Oct 1999
 ***************************************************************************
 *
 * Description: Monte Carlo Ftpc Hit Collection class
 *
 ***************************************************************************
 *
 * $Log: StMcFtpcHitCollection.cc,v $
 * Revision 2.2  2005/01/27 23:40:47  calderon
 * Adding persistency to StMcEvent as a step for Virtual MonteCarlo.
 *
 * Revision 2.1  1999/12/14 07:04:49  calderon
 * Numbering scheme as per SVT request.
 *
 * Revision 2.0  1999/11/17 02:00:59  calderon
 * Completely revised for new StEvent
 *
 *
 **************************************************************************/
#include "StMcFtpcHitCollection.hh"
#include "StMcFtpcHit.hh"

static const char rcsid[] = "$Id: StMcFtpcHitCollection.cc,v 2.2 2005/01/27 23:40:47 calderon Exp $";

ClassImp(StMcFtpcHitCollection)

StMcFtpcHitCollection::StMcFtpcHitCollection() { /* noop */ }

StMcFtpcHitCollection::~StMcFtpcHitCollection() { /* noop */ }
    
bool
StMcFtpcHitCollection::addHit(StMcFtpcHit* hit)
{
    unsigned int p;
    if (hit && (p = hit->plane()-1) < mNumberOfPlanes) {
      mPlanes[p].hits().push_back(hit);
      return true;
    }
    else
      return false;
}

unsigned int
StMcFtpcHitCollection::numberOfPlanes() const { return mNumberOfPlanes; }

unsigned long
StMcFtpcHitCollection::numberOfHits() const
{
    unsigned long sum = 0;
    for (int i=0; i<mNumberOfPlanes; i++)
      sum += mPlanes[i].numberOfHits();

    return sum;
}

StMcFtpcPlaneHitCollection*
StMcFtpcHitCollection::plane(unsigned int i)
{
    if (i < mNumberOfPlanes)
        return &(mPlanes[i]);
    else
        return 0;
}

const StMcFtpcPlaneHitCollection*
StMcFtpcHitCollection::plane(unsigned int i) const
{
    if (i < mNumberOfPlanes)
        return &(mPlanes[i]);
    else
        return 0;
}
