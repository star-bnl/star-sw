/***************************************************************************
 *
 * $Id: StMcSvtBarrelHitCollection.cc,v 2.3 2005/01/27 23:40:48 calderon Exp $
 *
 * Author: Manuel Calderon de la Barca Sanchez, March 2000
 ***************************************************************************
 *
 * Description: Monte Carlo Svt Barrel Hit Collection class
 *
 ***************************************************************************
 *
 * $Log: StMcSvtBarrelHitCollection.cc,v $
 * Revision 2.3  2005/01/27 23:40:48  calderon
 * Adding persistency to StMcEvent as a step for Virtual MonteCarlo.
 *
 * Revision 2.2  2000/04/19 18:32:23  calderon
 * Added check for SSD in barrel collection
 * put default numbers for l, d, w in SvtHitCollection
 *
 * Revision 2.1  2000/03/06 18:05:22  calderon
 * 1) Modified SVT Hits storage scheme from layer-ladder-wafer to
 * barrel-ladder-wafer.
 * 2) Added Rich Hit class and collection, and links to them in other
 * classes.
 *
 *
 **************************************************************************/
#include "StMcSvtBarrelHitCollection.hh"

static const char rcsid[] = "$Id: StMcSvtBarrelHitCollection.cc,v 2.3 2005/01/27 23:40:48 calderon Exp $";
ClassImp(StMcSvtBarrelHitCollection);
StMcSvtBarrelHitCollection::StMcSvtBarrelHitCollection()
{
    mBarrelNumber = -1;
}

StMcSvtBarrelHitCollection::~StMcSvtBarrelHitCollection() { /* noop */ }

void
StMcSvtBarrelHitCollection::setBarrelNumber(int i)
{
    if (mBarrelNumber == -1) mBarrelNumber = i;
}
    
unsigned int
StMcSvtBarrelHitCollection::numberOfLadders() const
{
    switch (mBarrelNumber) {
    case 0:
	return 8;
	break;
    case 1:
	return 12;
	break;
    case 2:
        return 16;
        break;
    case 3:
	return 20;
	break;
    default:
        return 0;
    }
}

unsigned long
StMcSvtBarrelHitCollection::numberOfHits() const
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
StMcSvtBarrelHitCollection::ladder(unsigned int i)
{
    if (i < numberOfLadders())
        return &(mLadders[i]);
    else
        return 0;
}

const StMcSvtLadderHitCollection*
StMcSvtBarrelHitCollection::ladder(unsigned int i) const
{
    if (i < numberOfLadders())
        return &(mLadders[i]);
    else
        return 0;
}
    
