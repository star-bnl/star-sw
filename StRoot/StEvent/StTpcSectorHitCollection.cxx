/***************************************************************************
 *
 * $Id: StTpcSectorHitCollection.cxx,v 2.2 2001/04/05 04:00:57 ullrich Exp $
 *
 * Author: Thomas Ullrich, July 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StTpcSectorHitCollection.cxx,v $
 * Revision 2.2  2001/04/05 04:00:57  ullrich
 * Replaced all (U)Long_t by (U)Int_t and all redundant ROOT typedefs.
 *
 * Revision 2.1  1999/10/13 19:45:34  ullrich
 * Initial Revision
 *
 **************************************************************************/
#include "StTpcSectorHitCollection.h"

static const char rcsid[] = "$Id: StTpcSectorHitCollection.cxx,v 2.2 2001/04/05 04:00:57 ullrich Exp $";

ClassImp(StTpcSectorHitCollection)

StTpcSectorHitCollection::StTpcSectorHitCollection() { /* noop */ }

StTpcSectorHitCollection::~StTpcSectorHitCollection() { /* noop */ }
    
unsigned int
StTpcSectorHitCollection::numberOfPadrows() const { return mNumberOfPadrows; }

StTpcPadrowHitCollection* StTpcSectorHitCollection::padrow(unsigned int i)
{
    if (i < mNumberOfPadrows)
        return &(mPadrows[i]);
    else
        return 0;
}

const StTpcPadrowHitCollection*
StTpcSectorHitCollection::padrow(unsigned int i) const
{
    if (i < mNumberOfPadrows)
        return &(mPadrows[i]);
    else
        return 0;
}

unsigned int StTpcSectorHitCollection::numberOfHits() const
{
    unsigned int sum = 0;
    for (int i=0; i < mNumberOfPadrows; i++)
        sum += mPadrows[i].hits().size();
    return sum;
}
