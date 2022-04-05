/***************************************************************************
 *
 * $Id: StMcSsdLadderHitCollection.cc,v 2.1 2005/11/22 21:44:52 fisyak Exp $
 *
 * Author: Fabrice Retiere/Kai Schweda, Aug 2003
 ***************************************************************************
 *
 * Description: Monte Carlo Ssd Ladder Hit Collection class
 *
 ***************************************************************************
 *
 * $Log: StMcSsdLadderHitCollection.cc,v $
 * Revision 2.1  2005/11/22 21:44:52  fisyak
 * Add compress Print for McEvent, add Ssd collections
 *
 * Revision 2.2  2005/01/27 23:40:48  calderon
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
 *
 *
 **************************************************************************/
#include "StMcSsdLadderHitCollection.hh"
#include "StMcSsdHit.hh"
static const char rcsid[] = "$Id: StMcSsdLadderHitCollection.cc,v 2.1 2005/11/22 21:44:52 fisyak Exp $";

ClassImp(StMcSsdLadderHitCollection)
unsigned long
StMcSsdLadderHitCollection::numberOfHits() const
{
    unsigned long sum = 0;
    for (unsigned int j=0; j<numberOfWafers(); j++) {
        sum += mWafers[j].hits().size();
    }
    return sum;
}

StMcSsdWaferHitCollection*
StMcSsdLadderHitCollection::wafer(unsigned int i)
{
    if (i < numberOfWafers())
        return &(mWafers[i]);
    else
        return 0;
}

const StMcSsdWaferHitCollection*
StMcSsdLadderHitCollection::wafer(unsigned int i) const
{
    if (i < numberOfWafers())
        return &(mWafers[i]);
    else
        return 0;
}
