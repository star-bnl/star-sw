/***************************************************************************
 *
 * $Id: StTpcSectorHitCollection.cxx,v 2.1 1999/10/13 19:45:34 ullrich Exp $
 *
 * Author: Thomas Ullrich, July 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StTpcSectorHitCollection.cxx,v $
 * Revision 2.1  1999/10/13 19:45:34  ullrich
 * Initial Revision
 *
 **************************************************************************/
#include "StTpcSectorHitCollection.h"

static const char rcsid[] = "$Id: StTpcSectorHitCollection.cxx,v 2.1 1999/10/13 19:45:34 ullrich Exp $";

ClassImp(StTpcSectorHitCollection)

StTpcSectorHitCollection::StTpcSectorHitCollection() { /* noop */ }

StTpcSectorHitCollection::~StTpcSectorHitCollection() { /* noop */ }
    
UInt_t
StTpcSectorHitCollection::numberOfPadrows() const { return mNumberOfPadrows; }

StTpcPadrowHitCollection* StTpcSectorHitCollection::padrow(UInt_t i)
{
    if (i < mNumberOfPadrows)
        return &(mPadrows[i]);
    else
        return 0;
}

const StTpcPadrowHitCollection*
StTpcSectorHitCollection::padrow(UInt_t i) const
{
    if (i < mNumberOfPadrows)
        return &(mPadrows[i]);
    else
        return 0;
}

ULong_t StTpcSectorHitCollection::numberOfHits() const
{
    ULong_t sum = 0;
    for (int i=0; i < mNumberOfPadrows; i++)
        sum += mPadrows[i].hits().size();
    return sum;
}
