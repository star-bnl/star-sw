/***************************************************************************
 *
 * $Id: StMcSvtLadderHitCollection.cc,v 2.1 1999/11/19 19:06:33 calderon Exp $
 *
 * Author: Manuel Calderon de la Barca Sanchez, Oct 1999
 ***************************************************************************
 *
 * Description: Monte Carlo Svt Ladder Hit Collection class
 *
 ***************************************************************************
 *
 * $Log: StMcSvtLadderHitCollection.cc,v $
 * Revision 2.1  1999/11/19 19:06:33  calderon
 * Recommit after redoing the files.
 *
 * Revision 2.0  1999/11/17 02:01:00  calderon
 * Completely revised for new StEvent
 *
 *
 **************************************************************************/
#include "StMcSvtLadderHitCollection.hh"

static const char rcsid[] = "$Id: StMcSvtLadderHitCollection.cc,v 2.1 1999/11/19 19:06:33 calderon Exp $";

#ifdef PERSISTENT
ClassImp(StMcSvtLadderHitCollection)
#endif

StMcSvtLadderHitCollection::StMcSvtLadderHitCollection()
{
    mLayerNumber = -1;
}

StMcSvtLadderHitCollection::~StMcSvtLadderHitCollection() { /* noop */ }

void
StMcSvtLadderHitCollection::setLayerNumber(int i)
{
    if (mLayerNumber == -1) mLayerNumber = i;
}
    
unsigned int
StMcSvtLadderHitCollection::numberOfWafers() const
{
    switch (mLayerNumber) {
    case 0:
    case 1:
        return 4;
        break;
    case 2:
    case 3:
        return 6;
        break;
    case 4:
    case 5:
        return 7;
        break;
    default:
        return 0;
    }
}

unsigned long
StMcSvtLadderHitCollection::numberOfHits() const
{
    unsigned long sum = 0;
    for (unsigned int j=0; j<numberOfWafers(); j++) {
        sum += mWafers[j].hits().size();
    }
    return sum;
}

StMcSvtWaferHitCollection*
StMcSvtLadderHitCollection::wafer(unsigned int i)
{
    if (i < numberOfWafers())
        return &(mWafers[i]);
    else
        return 0;
}

const StMcSvtWaferHitCollection*
StMcSvtLadderHitCollection::wafer(unsigned int i) const
{
    if (i < numberOfWafers())
        return &(mWafers[i]);
    else
        return 0;
}
    
