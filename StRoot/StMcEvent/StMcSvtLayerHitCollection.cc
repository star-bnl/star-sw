/***************************************************************************
 *
 * $Id: StMcSvtLayerHitCollection.cc,v 2.1 1999/12/14 07:04:49 calderon Exp $
 *
 * Author: Manuel Calderon de la Barca Sanchez, Oct 1999
 ***************************************************************************
 *
 * Description: Monte Carlo Svt Layer Hit Collection class
 *
 ***************************************************************************
 *
 * $Log: StMcSvtLayerHitCollection.cc,v $
 * Revision 2.1  1999/12/14 07:04:49  calderon
 * Numbering scheme as per SVT request.
 *
 * Revision 2.0  1999/11/17 02:01:00  calderon
 * Completely revised for new StEvent
 *
 *
 **************************************************************************/
#include "StMcSvtLayerHitCollection.hh"

static const char rcsid[] = "$Id: StMcSvtLayerHitCollection.cc,v 2.1 1999/12/14 07:04:49 calderon Exp $";

#ifdef PERSISTENT
ClassImp(StMcSvtLayerHitCollection)
#endif

StMcSvtLayerHitCollection::StMcSvtLayerHitCollection()
{
    mLayerNumber = -1;
}

StMcSvtLayerHitCollection::~StMcSvtLayerHitCollection() { /* noop */ }

void
StMcSvtLayerHitCollection::setLayerNumber(int i)
{
    if (mLayerNumber == -1) mLayerNumber = i;
}
    
unsigned int
StMcSvtLayerHitCollection::numberOfLadders() const
{
    switch (mLayerNumber) {
    case 1:
    case 2:
        return 4;
        break;
    case 3:
    case 4:
        return 6;
        break;
    case 5:
    case 6:
        return 8;
        break;
    default:
        return 0;
    }
}

unsigned long
StMcSvtLayerHitCollection::numberOfHits() const
{
    unsigned long sum = 0;
    for (unsigned int j=0; j<numberOfLadders(); j++) {
        for (unsigned int k=0; k<mLadders[j].numberOfWafers(); k++) {
            sum += mLadders[j].wafer(k)->hits().size();
        }
    }
    return sum;
}

StMcSvtLadderHitCollection*
StMcSvtLayerHitCollection::ladder(unsigned int i)
{
    if (i < numberOfLadders())
        return &(mLadders[i]);
    else
        return 0;
}

const StMcSvtLadderHitCollection*
StMcSvtLayerHitCollection::ladder(unsigned int i) const
{
    if (i < numberOfLadders())
        return &(mLadders[i]);
    else
        return 0;
}
    
