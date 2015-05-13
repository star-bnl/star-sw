/***************************************************************************
 *
 * $Id: StSstHitCollection.cxx,v 2.1 2015/05/13 16:50:59 ullrich Exp $
 *
 * Author: Jonathan Bouchet, Thomas Ullrich, May 2015
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StSstHitCollection.cxx,v $
 * Revision 2.1  2015/05/13 16:50:59  ullrich
 * Initial Revision.
 *
 **************************************************************************/
#include "StSstHitCollection.h"
#include "StSstLadderHitCollection.h"
#include "StSstHit.h"

static const char rcsid[] = "$Id: StSstHitCollection.cxx,v 2.1 2015/05/13 16:50:59 ullrich Exp $";

ClassImp(StSstHitCollection)

StSstHitCollection::StSstHitCollection() { /* no op */ }

StSstHitCollection::~StSstHitCollection() { /* no op */ }
    
unsigned int
StSstHitCollection::numberOfLadders() const { return mNumberOfLadders; }

bool
StSstHitCollection::addHit(StSstHit* hit)
{
    unsigned int l, w;
    if (hit &&
        (l = hit->ladder()-1) < mNumberOfLadders &&
        (w = hit->wafer()-1) < mLadders[l].numberOfWafers()) {
        mLadders[l].wafer(w)->hits().push_back(hit);
        return true;
    }
    else {
        return false;
    }
}

unsigned int
StSstHitCollection::numberOfHits() const
{
    unsigned int sum = 0;
    for (int i=0; i<mNumberOfLadders; i++)
        for (unsigned int j=0; j<mLadders[i].numberOfWafers(); j++)
                sum += mLadders[i].wafer(j)->hits().size();
    return sum;
}

StSstLadderHitCollection*
StSstHitCollection::ladder(unsigned int i)
{
    if (i < mNumberOfLadders)
        return &(mLadders[i]);
    else
        return 0;
}

const StSstLadderHitCollection*
StSstHitCollection::ladder(unsigned int i) const
{
    if (i < mNumberOfLadders)
        return &(mLadders[i]);
    else
        return 0;
}

