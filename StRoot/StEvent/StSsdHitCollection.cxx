/***************************************************************************
 *
 * $Id: StSsdHitCollection.cxx,v 2.1 2000/01/05 16:00:04 ullrich Exp $
 *
 * Author: Lilian Martin, Thomas Ullrich, Dec 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StSsdHitCollection.cxx,v $
 * Revision 2.1  2000/01/05 16:00:04  ullrich
 * Initial Revision.
 *
 **************************************************************************/
#include "StSsdHitCollection.h"
#include "StSsdLadderHitCollection.h"
#include "StSsdHit.h"

static const char rcsid[] = "$Id: StSsdHitCollection.cxx,v 2.1 2000/01/05 16:00:04 ullrich Exp $";

ClassImp(StSsdHitCollection)

StSsdHitCollection::StSsdHitCollection() { /* noop */ }

StSsdHitCollection::~StSsdHitCollection() { /* noop */ }
    
UInt_t
StSsdHitCollection::numberOfLadders() const { return mNumberOfLadders; }

Bool_t
StSsdHitCollection::addHit(StSsdHit* hit)
{
    unsigned int l, w;
    if (hit &&
        (l = hit->ladder()-1) < mNumberOfLadders &&
        (w = hit->wafer()-1) < mLadders[l].numberOfWafers()) {
        mLadders[l].wafer(w)->hits().push_back(hit);
        return kTRUE;
    }
    else {
        return kFALSE;
    }
}

ULong_t
StSsdHitCollection::numberOfHits() const
{
    ULong_t sum = 0;
    for (int i=0; i<mNumberOfLadders; i++)
        for (unsigned int j=0; j<mLadders[i].numberOfWafers(); j++)
                sum += mLadders[i].wafer(j)->hits().size();
    return sum;
}

StSsdLadderHitCollection*
StSsdHitCollection::ladder(UInt_t i)
{
    if (i < mNumberOfLadders)
        return &(mLadders[i]);
    else
        return 0;
}

const StSsdLadderHitCollection*
StSsdHitCollection::ladder(UInt_t i) const
{
    if (i < mNumberOfLadders)
        return &(mLadders[i]);
    else
        return 0;
}

