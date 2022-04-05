/***************************************************************************
 *
 * $Id: StTpcHitCollection.cxx,v 2.7 2019/04/02 15:32:49 smirnovd Exp $
 *
 * Author: Thomas Ullrich, July 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StTpcHitCollection.cxx,v $
 * Revision 2.7  2019/04/02 15:32:49  smirnovd
 * Add iterator to loop over StTpcHits in StTpcHitContainer
 *
 * Revision 2.6  2019/04/02 15:32:42  smirnovd
 * Add accessors to StTpcHitContainer
 *
 * Revision 2.5  2009/11/23 16:34:07  fisyak
 * Cleanup, remove dependence on dst tables, clean up software monitors
 *
 * Revision 2.4  2001/04/05 04:00:57  ullrich
 * Replaced all (U)Long_t by (U)Int_t and all redundant ROOT typedefs.
 *
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

static const char rcsid[] = "$Id: StTpcHitCollection.cxx,v 2.7 2019/04/02 15:32:49 smirnovd Exp $";

ClassImp(StTpcHitCollection)

StTpcHitCollection::StTpcHitCollection() { /* noop */ }

StTpcHitCollection::~StTpcHitCollection() { /* noop */ }
    
bool
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
unsigned int
StTpcHitCollection::numberOfHits() const
{
    unsigned int sum = 0;
    for (int i=0; i<mNumberOfSectors; i++) {
        for (unsigned int j=0; j<mSectors[i].numberOfPadrows(); j++) {
            sum += mSectors[i].padrow(j)->hits().size();
        }
    }
    return sum;
}

StTpcSectorHitCollection*
StTpcHitCollection::sector(unsigned int i)
{
    if (i < mNumberOfSectors)
        return &(mSectors[i]);
    else
        return 0;
}

const StTpcSectorHitCollection*
StTpcHitCollection::sector(unsigned int i) const
{
    if (i < mNumberOfSectors)
        return &(mSectors[i]);
    else
        return 0;
}

const StSPtrVecTpcHit*
StTpcHitCollection::hits(int sectorId, int padrowId) const
{
    const StTpcSectorHitCollection* sc = (sectorId < mNumberOfSectors ? sector(sectorId) : nullptr);
    const StTpcPadrowHitCollection* pc = (sc && padrowId < sc->numberOfPadrows() ? sc->padrow(padrowId) : nullptr);
    return pc ? &pc->hits() : nullptr;
}

StTpcHitCollection::StTpcHitIter
StTpcHitCollection::StTpcHitIter::begin(StTpcHitCollection& c)
{
    // Skip to first non-empty vector
    for (int l1 = 0; l1 < c.numberOfSectors(); ++l1)
        for (int l2 = 0; l2 < c.numberOfPadrows(l1); ++l2)
            if ( !(*c.hits(l1, l2)).empty() ) return StTpcHitCollection::StTpcHitIter(c, l1, l2);
    return end(c);
}

StTpcHitCollection::StTpcHitIter
StTpcHitCollection::StTpcHitIter::end(StTpcHitCollection& c)
{
    return StTpcHitCollection::StTpcHitIter(c, c.numberOfSectors());
}

StTpcHitCollection::StTpcHitIter&
StTpcHitCollection::StTpcHitIter::operator++()
{
    ++iHit;

    // Reset counters when this level maximum reached
    const StSPtrVecTpcHit& currVector = *coll.hits(iSector, iPadrow);
    if (iHit >= currVector.size()) {
        ++iPadrow;
        iHit = 0;
    }

    // Reset counters when this level maximum reached
    if (iPadrow >= coll.numberOfPadrows(iSector)) {
        ++iSector;
        iPadrow = 0;
        iHit = 0;
    }

    if (*this != end(coll)) {
        // Skip next vector if empty and increase indices
        const StSPtrVecTpcHit& nextVector = *coll.hits(iSector, iPadrow);
        if (nextVector.empty()) {
            iHit = 0;
            (*this).operator++();
        }
    }

    return *this;
}

bool
StTpcHitCollection::StTpcHitIter::operator==(const StTpcHitCollection::StTpcHitIter &other) const
{
    return &other.coll == &coll && other.iSector == iSector && other.iPadrow == iPadrow && other.iHit == iHit;
}

bool
StTpcHitCollection::StTpcHitIter::operator!=(const StTpcHitCollection::StTpcHitIter &other) const
{
    return !(*this == other);
}

const StTpcHit*
StTpcHitCollection::StTpcHitIter::operator*() const
{
    return (*coll.hits(iSector, iPadrow))[iHit];
}
