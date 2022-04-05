/***************************************************************************
 *
 * $Id: StFtpcPlaneHitCollection.cxx,v 2.2 2001/04/05 04:00:50 ullrich Exp $
 *
 * Author: Thomas Ullrich, July 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StFtpcPlaneHitCollection.cxx,v $
 * Revision 2.2  2001/04/05 04:00:50  ullrich
 * Replaced all (U)Long_t by (U)Int_t and all redundant ROOT typedefs.
 *
 * Revision 2.1  1999/10/13 19:44:40  ullrich
 * Initial Revision
 *
 **************************************************************************/
#include "StFtpcPlaneHitCollection.h"

static const char rcsid[] = "$Id: StFtpcPlaneHitCollection.cxx,v 2.2 2001/04/05 04:00:50 ullrich Exp $";

ClassImp(StFtpcPlaneHitCollection)

StFtpcPlaneHitCollection::StFtpcPlaneHitCollection() { /* noop */ }

StFtpcPlaneHitCollection::~StFtpcPlaneHitCollection() { /* noop */ }
    
unsigned int
StFtpcPlaneHitCollection::numberOfSectors() const { return mNumberOfSectors; }

StFtpcSectorHitCollection* StFtpcPlaneHitCollection::sector(unsigned int i)
{
    if (i < mNumberOfSectors)
        return &(mSectors[i]);
    else
        return 0;
}

const StFtpcSectorHitCollection*
StFtpcPlaneHitCollection::sector(unsigned int i) const
{
    if (i < mNumberOfSectors)
        return &(mSectors[i]);
    else
        return 0;
}

unsigned int StFtpcPlaneHitCollection::numberOfHits() const
{
    unsigned int sum = 0;
    for (int i=0; i < mNumberOfSectors; i++)
        sum += mSectors[i].hits().size();
    return sum;
}
