/***************************************************************************
 *
 * $Id: StMcSvtHitCollection.cc,v 2.1 1999/12/14 07:04:49 calderon Exp $
 *
 * Author: Manuel Calderon de la Barca Sanchez, Oct 1999
 ***************************************************************************
 *
 * Description: Monte Carlo Svt Hit Collection class
 *
 ***************************************************************************
 *
 * $Log: StMcSvtHitCollection.cc,v $
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

static const char rcsid[] = "$Id: StMcSvtHitCollection.cc,v 2.1 1999/12/14 07:04:49 calderon Exp $";

#ifdef PERSISTENT
ClassImp(StMcSvtHitCollection)
#endif

StMcSvtHitCollection::StMcSvtHitCollection()
{
    //
    //  Layer and ladder collections have to know
    //  their layer number in order to return the
    //  proper numberOfLadders() and numberOfWafers().
    //
    for (int i=0; i<mNumberOfLayers; i++) {
        mLayers[i].setLayerNumber(i+1);
        for (unsigned int j=0; j<mLayers[i].numberOfLadders(); j++)
            mLayers[i].ladder(j)->setLayerNumber(i+1);
    }
}

StMcSvtHitCollection::~StMcSvtHitCollection() { /* noop */ }
    
unsigned int
StMcSvtHitCollection::numberOfLayers() const { return mNumberOfLayers; }

bool
StMcSvtHitCollection::addHit(StMcSvtHit* hit)
{
    unsigned int l, d, w;
    if (hit &&
        (l = hit->layer()-1) < mNumberOfLayers &&
        (d = hit->ladder()-1) < mLayers[l].numberOfLadders() &&
        (w = hit->wafer()-1) < mLayers[l].ladder(d)->numberOfWafers()) {
        mLayers[l].ladder(d)->wafer(w)->hits().push_back(hit);
        return true;
    }
    else
        return false;
}

unsigned long
StMcSvtHitCollection::numberOfHits() const
{
    unsigned long sum = 0;
    for (int i=0; i<mNumberOfLayers; i++)
        for (unsigned int j=0; j<mLayers[i].numberOfLadders(); j++)
            for (unsigned int k=0; k<mLayers[i].ladder(j)->numberOfWafers(); k++)
                sum += mLayers[i].ladder(j)->wafer(k)->hits().size();
    return sum;
}

StMcSvtLayerHitCollection*
StMcSvtHitCollection::layer(unsigned int i)
{
    if (i < mNumberOfLayers)
        return &(mLayers[i]);
    else
        return 0;
}

const StMcSvtLayerHitCollection*
StMcSvtHitCollection::layer(unsigned int i) const
{
    if (i < mNumberOfLayers)
        return &(mLayers[i]);
    else
        return 0;
}
