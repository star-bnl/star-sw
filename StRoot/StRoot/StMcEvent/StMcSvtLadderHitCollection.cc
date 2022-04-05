/***************************************************************************
 *
 * $Id: StMcSvtLadderHitCollection.cc,v 2.5 2005/01/27 23:40:48 calderon Exp $
 *
 * Author: Manuel Calderon de la Barca Sanchez, Oct 1999
 ***************************************************************************
 *
 * Description: Monte Carlo Svt Ladder Hit Collection class
 *
 ***************************************************************************
 *
 * $Log: StMcSvtLadderHitCollection.cc,v $
 * Revision 2.5  2005/01/27 23:40:48  calderon
 * Adding persistency to StMcEvent as a step for Virtual MonteCarlo.
 *
 * Revision 2.4  2000/04/19 14:34:48  calderon
 * More corrections for the SSD, thanks Helen
 *
 * Revision 2.3  2000/03/06 18:05:22  calderon
 * 1) Modified SVT Hits storage scheme from layer-ladder-wafer to
 * barrel-ladder-wafer.
 * 2) Added Rich Hit class and collection, and links to them in other
 * classes.
 *
 * Revision 2.2  1999/12/14 07:04:49  calderon
 * Numbering scheme as per SVT request.
 *
 * Revision 2.1  1999/11/19 19:06:33  calderon
 * Recommit after redoing the files.
 *
 * Revision 2.0  1999/11/17 02:01:00  calderon
 * Completely revised for new StEvent
 *
 *
 **************************************************************************/
#include "StMcSvtLadderHitCollection.hh"

static const char rcsid[] = "$Id: StMcSvtLadderHitCollection.cc,v 2.5 2005/01/27 23:40:48 calderon Exp $";
ClassImp(StMcSvtLadderHitCollection);
StMcSvtLadderHitCollection::StMcSvtLadderHitCollection()
{
    mBarrelNumber = -1;
}

StMcSvtLadderHitCollection::~StMcSvtLadderHitCollection() { /* noop */ }

void
StMcSvtLadderHitCollection::setBarrelNumber(int i)
{
    if (mBarrelNumber == -1) mBarrelNumber = i;
}
    
unsigned int
StMcSvtLadderHitCollection::numberOfWafers() const
{
    switch (mBarrelNumber) {
    case 0:
        return 4;
        break;
    case 1:
        return 6;
        break;
    case 2:
        return 7;
        break;
    case 3:  // SSD
	return 16;
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
    
