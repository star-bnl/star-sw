/***************************************************************************
 *
 * $Id: StMcSvtHitCollection.cc,v 2.4 2005/01/27 23:40:48 calderon Exp $
 *
 * Author: Manuel Calderon de la Barca Sanchez, Oct 1999
 ***************************************************************************
 *
 * Description: Monte Carlo Svt Hit Collection class
 *
 ***************************************************************************
 *
 * $Log: StMcSvtHitCollection.cc,v $
 * Revision 2.4  2005/01/27 23:40:48  calderon
 * Adding persistency to StMcEvent as a step for Virtual MonteCarlo.
 *
 * Revision 2.3  2000/04/19 18:32:23  calderon
 * Added check for SSD in barrel collection
 * put default numbers for l, d, w in SvtHitCollection
 *
 * Revision 2.2  2000/03/06 18:05:22  calderon
 * 1) Modified SVT Hits storage scheme from layer-ladder-wafer to
 * barrel-ladder-wafer.
 * 2) Added Rich Hit class and collection, and links to them in other
 * classes.
 *
 * Revision 2.1  1999/12/14 07:04:49  calderon
 * Numbering scheme as per SVT request.
 *
 * Revision 2.0  1999/11/17 02:00:59  calderon
 * Completely revised for new StEvent
 *
 *
 **************************************************************************/
#include "StMcSvtHitCollection.hh"
#include "StMcSvtHit.hh"

static const char rcsid[] = "$Id: StMcSvtHitCollection.cc,v 2.4 2005/01/27 23:40:48 calderon Exp $";
ClassImp(StMcSvtHitCollection);
StMcSvtHitCollection::StMcSvtHitCollection()
{
    //
    //  Barrel and ladder collections have to know
    //  their barrel number in order to return the
    //  proper numberOfLadders() and numberOfWafers().
    //
    for (int i=0; i<mNumberOfBarrels; i++) {
        mBarrels[i].setBarrelNumber(i);
        for (unsigned int j=0; j<mBarrels[i].numberOfLadders(); j++)
            mBarrels[i].ladder(j)->setBarrelNumber(i);
    }
}

StMcSvtHitCollection::~StMcSvtHitCollection() { /* noop */ }
    
unsigned int
StMcSvtHitCollection::numberOfBarrels() const { return mNumberOfBarrels; }

bool
StMcSvtHitCollection::addHit(StMcSvtHit* hit)
{
    unsigned int l, d, w;
    l = d = w = 99999; // set to some default invalid number
    if (hit &&
        (l = hit->barrel()-1) < mNumberOfBarrels &&
        (d = hit->ladder()-1) < mBarrels[l].numberOfLadders() &&
        (w = hit->wafer()-1) < mBarrels[l].ladder(d)->numberOfWafers()) {
        mBarrels[l].ladder(d)->wafer(w)->hits().push_back(hit);
        return true;
    }
    else {
	//cout << "\nBAD Hit:\n" << *hit << endl; 
        return false;
    }
}

unsigned long
StMcSvtHitCollection::numberOfHits() const
{
    unsigned long sum = 0;
    for (int i=0; i<mNumberOfBarrels; i++)
        for (unsigned int j=0; j<mBarrels[i].numberOfLadders(); j++)
            for (unsigned int k=0; k<mBarrels[i].ladder(j)->numberOfWafers(); k++)
                sum += mBarrels[i].ladder(j)->wafer(k)->hits().size();
    return sum;
}

StMcSvtBarrelHitCollection*
StMcSvtHitCollection::barrel(unsigned int i)
{
    if (i < mNumberOfBarrels)
        return &(mBarrels[i]);
    else
        return 0;
}

const StMcSvtBarrelHitCollection*
StMcSvtHitCollection::barrel(unsigned int i) const
{
    if (i < mNumberOfBarrels)
        return &(mBarrels[i]);
    else
        return 0;
}
