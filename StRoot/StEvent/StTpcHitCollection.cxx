/***************************************************************************
 *
 * $Id: StTpcHitCollection.cxx,v 2.3 1999/12/13 20:16:29 ullrich Exp $
 *
 * Author: Thomas Ullrich, July 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StTpcHitCollection.cxx,v $
 * Revision 2.3  1999/12/13 20:16:29  ullrich
 * Changed numbering scheme for hw_position unpack methods (STAR conventions).
 *
 * Revision 2.2  1999/10/28 22:27:13  ullrich
 * Adapted new StArray version. First version to compile on Linux and Sun.
 *
 * Revision 2.1  1999/10/13 19:45:27  ullrich
 * Initial Revision
 *
 **************************************************************************/
#include "StTpcHitCollection.h"
#include "StTpcPadrowHitCollection.h"
#include "StTpcHit.h"

static const char rcsid[] = "$Id: StTpcHitCollection.cxx,v 2.3 1999/12/13 20:16:29 ullrich Exp $";

ClassImp(StTpcHitCollection)

StTpcHitCollection::StTpcHitCollection() { /* noop */ }

StTpcHitCollection::~StTpcHitCollection() { /* noop */ }
    
Bool_t
StTpcHitCollection::addHit(StTpcHit* hit)
{
    unsigned int s, r;
    if (hit &&
        (s = hit->sector()-1) < mNumberOfSectors &&
        (r = hit->padrow()-1) < mSectors[s].numberOfPadrows()) {
        mSectors[s].padrow(r)->hits().push_back(hit);
        return kTRUE;
    }
    else
        return kFALSE;
}

UInt_t
StTpcHitCollection::numberOfSectors() const { return mNumberOfSectors; }

ULong_t
StTpcHitCollection::numberOfHits() const
{
    ULong_t sum = 0;
    for (int i=0; i<mNumberOfSectors; i++) {
        for (unsigned int j=0; j<mSectors[i].numberOfPadrows(); j++) {
            sum += mSectors[i].padrow(j)->hits().size();
        }
    }
    return sum;
}

StTpcSectorHitCollection*
StTpcHitCollection::sector(UInt_t i)
{
    if (i < mNumberOfSectors)
        return &(mSectors[i]);
    else
        return 0;
}

const StTpcSectorHitCollection*
StTpcHitCollection::sector(UInt_t i) const
{
    if (i < mNumberOfSectors)
        return &(mSectors[i]);
    else
        return 0;
}
