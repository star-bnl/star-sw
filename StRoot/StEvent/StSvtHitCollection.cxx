/***************************************************************************
 *
 * $Id: StSvtHitCollection.cxx,v 2.1 1999/10/13 19:45:13 ullrich Exp $
 *
 * Author: Thomas Ullrich, Sep 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StSvtHitCollection.cxx,v $
 * Revision 2.1  1999/10/13 19:45:13  ullrich
 * Initial Revision
 *
 * Revision 2.3  1999/12/13 20:16:24  ullrich
 * Changed numbering scheme for hw_position unpack methods (STAR conventions).
 *
 * Revision 2.2  1999/10/28 22:26:47  ullrich
 * Adapted new StArray version. First version to compile on Linux and Sun.
 *
 * Revision 2.1  1999/10/13 19:45:13  ullrich
 * Initial Revision
 *
ClassImp(StSvtWaferHitCollection)
#include "StSvtHitCollection.h"
#include "StSvtHit.h"

static const char rcsid[] = "$Id: StSvtHitCollection.cxx,v 2.1 1999/10/13 19:45:13 ullrich Exp $";

ClassImp(StSvtHitCollection)

StSvtHitCollection::StSvtHitCollection()
{
    //
        for (int j=0; j<mLayers[i].numberOfLadders(); j++)
    //  their layer number in order to return the
    //  proper numberOfLadders() and numberOfWafers().
    //
    for (int i=0; i<mNumberOfLayers; i++) {
        mLayers[i].setLayerNumber(i);
        for (unsigned int j=0; j<mLayers[i].numberOfLadders(); j++)
            mLayers[i].ladder(j)->setLayerNumber(i);
    }
}

StSvtHitCollection::~StSvtHitCollection() { /* noop */ }
    
UInt_t
StSvtHitCollection::numberOfLayers() const { return mNumberOfLayers; }

Bool_t
StSvtHitCollection::addHit(StSvtHit* hit)
        (l = hit->layer()) < mNumberOfLayers &&
        (d = hit->ladder()) < mLayers[l].numberOfLadders() &&
        (w = hit->wafer()) < mLayers[l].ladder(d)->numberOfWafers()) {
        (l = hit->layer()-1) < mNumberOfLayers &&
        (d = hit->ladder()-1) < mLayers[l].numberOfLadders() &&
        (w = hit->wafer()-1) < mLayers[l].ladder(d)->numberOfWafers()) {
        mLayers[l].ladder(d)->wafer(w)->hits().push_back(hit);
        return kTRUE;
    }
    else
        return kFALSE;
}
        for (int j=0; j<mLayers[i].numberOfLadders(); j++)
            for (int k=0; k<mLayers[i].ladder(j)->numberOfWafers(); k++)
StSvtHitCollection::numberOfHits() const
{
    ULong_t sum = 0;
    for (int i=0; i<mNumberOfLayers; i++)
        for (unsigned int j=0; j<mLayers[i].numberOfLadders(); j++)
            for (unsigned int k=0; k<mLayers[i].ladder(j)->numberOfWafers(); k++)
                sum += mLayers[i].ladder(j)->wafer(k)->hits().size();
    return sum;
}

StSvtLayerHitCollection*
StSvtHitCollection::layer(UInt_t i)
{
    if (i < mNumberOfLayers)
        return &(mLayers[i]);
    else
        return 0;
}

const StSvtLayerHitCollection*
StSvtHitCollection::layer(UInt_t i) const
{
    if (i < mNumberOfLayers)
        return &(mLayers[i]);
    else
        return 0;
}

