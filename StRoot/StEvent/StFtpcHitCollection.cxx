/***************************************************************************
 *
 * $Id: StFtpcHitCollection.cxx,v 2.1 1999/10/13 19:44:38 ullrich Exp $
 *
 * Author: Thomas Ullrich, July 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StFtpcHitCollection.cxx,v $
 * Revision 2.1  1999/10/13 19:44:38  ullrich
 * Initial Revision
 *
 * Revision 2.2  1999/10/28 22:25:22  ullrich
 * Adapted new StArray version. First version to compile on Linux and Sun.
 *
 * Revision 2.1  1999/10/13 19:44:38  ullrich
 * Initial Revision
 *
 **************************************************************************/
#include "StFtpcHitCollection.h"
#include "StFtpcSectorHitCollection.h"
#include "StFtpcHit.h"

static const char rcsid[] = "$Id: StFtpcHitCollection.cxx,v 2.1 1999/10/13 19:44:38 ullrich Exp $";

ClassImp(StFtpcHitCollection)

StFtpcHitCollection::StFtpcHitCollection() { /* noop */ }

StFtpcHitCollection::~StFtpcHitCollection() { /* noop */ }
    
Bool_t
StFtpcHitCollection::addHit(StFtpcHit* hit)
{
    unsigned int p, s;
    if (hit &&
        (p = hit->plane()) < mNumberOfPlanes &&
        (s = hit->sector()) < mPlanes[p].numberOfSectors()) {
        mPlanes[p].sector(s)->hits().push_back(hit);
        return kTRUE;
    }
    else
        return kFALSE;
}

UInt_t
StFtpcHitCollection::numberOfPlanes() const { return mNumberOfPlanes; }

ULong_t
    for (int i=0; i<mNumberOfPlanes; i++) {
        for (int j=0; j<mPlanes[i].numberOfSectors(); j++) {
    ULong_t sum = 0;
    for (unsigned int i=0; i<mNumberOfPlanes; i++) {
        for (unsigned int j=0; j<mPlanes[i].numberOfSectors(); j++) {
            sum += mPlanes[i].sector(j)->hits().size();
        }
    }
    return sum;
}

StFtpcPlaneHitCollection*
StFtpcHitCollection::plane(UInt_t i)
{
    if (i < mNumberOfPlanes)
        return &(mPlanes[i]);
    else
        return 0;
}

const StFtpcPlaneHitCollection*
StFtpcHitCollection::plane(UInt_t i) const
{
    if (i < mNumberOfPlanes)
        return &(mPlanes[i]);
    else
        return 0;
}
