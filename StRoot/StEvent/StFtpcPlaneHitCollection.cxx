/***************************************************************************
 *
 * $Id: StFtpcPlaneHitCollection.cxx,v 2.1 1999/10/13 19:44:40 ullrich Exp $
 *
 * Author: Thomas Ullrich, July 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StFtpcPlaneHitCollection.cxx,v $
 * Revision 2.1  1999/10/13 19:44:40  ullrich
 * Initial Revision
 *
 **************************************************************************/
#include "StFtpcPlaneHitCollection.h"

static const char rcsid[] = "$Id: StFtpcPlaneHitCollection.cxx,v 2.1 1999/10/13 19:44:40 ullrich Exp $";

ClassImp(StFtpcPlaneHitCollection)

StFtpcPlaneHitCollection::StFtpcPlaneHitCollection() { /* noop */ }

StFtpcPlaneHitCollection::~StFtpcPlaneHitCollection() { /* noop */ }
    
UInt_t
StFtpcPlaneHitCollection::numberOfSectors() const { return mNumberOfSectors; }

StFtpcSectorHitCollection* StFtpcPlaneHitCollection::sector(UInt_t i)
{
    if (i < mNumberOfSectors)
        return &(mSectors[i]);
    else
        return 0;
}

const StFtpcSectorHitCollection*
StFtpcPlaneHitCollection::sector(UInt_t i) const
{
    if (i < mNumberOfSectors)
        return &(mSectors[i]);
    else
        return 0;
}

ULong_t StFtpcPlaneHitCollection::numberOfHits() const
{
    ULong_t sum = 0;
    for (int i=0; i < mNumberOfSectors; i++)
        sum += mSectors[i].hits().size();
    return sum;
}
