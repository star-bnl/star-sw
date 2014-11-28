/***************************************************************************
 *
 * $Id: StSsdHitCollection.cxx,v 2.2 2001/04/05 04:00:54 ullrich Exp $
 *
 * Author: Lilian Martin, Thomas Ullrich, Dec 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StSsdHitCollection.cxx,v $
 * Revision 2.2  2001/04/05 04:00:54  ullrich
 * Replaced all (U)Long_t by (U)Int_t and all redundant ROOT typedefs.
 *
 * Revision 2.1  2000/01/05 16:00:04  ullrich
 * Initial Revision.
 *
 **************************************************************************/
#include "StSsdHitCollection.h"
#include "StSsdLadderHitCollection.h"
#include "StSsdHit.h"

static const char rcsid[] = "$Id: StSsdHitCollection.cxx,v 2.2 2001/04/05 04:00:54 ullrich Exp $";

ClassImp(StSsdHitCollection)

StSsdHitCollection::StSsdHitCollection() { /* noop */ }

StSsdHitCollection::~StSsdHitCollection() { /* noop */ }
    
unsigned int
StSsdHitCollection::numberOfLadders() const { return mNumberOfLadders; }

bool
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

unsigned int
StSsdHitCollection::numberOfHits() const
{
    unsigned int sum = 0;
    for (int i=0; i<mNumberOfLadders; i++)
        for (unsigned int j=0; j<mLadders[i].numberOfWafers(); j++)
                sum += mLadders[i].wafer(j)->hits().size();
    return sum;
}

StSsdLadderHitCollection*
StSsdHitCollection::ladder(unsigned int i)
{
    if (i < mNumberOfLadders)
        return &(mLadders[i]);
    else
        return 0;
}

const StSsdLadderHitCollection*
StSsdHitCollection::ladder(unsigned int i) const
{
    if (i < mNumberOfLadders)
        return &(mLadders[i]);
    else
        return 0;
}

