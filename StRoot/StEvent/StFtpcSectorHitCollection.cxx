/***************************************************************************
 *
 * $Id: StFtpcSectorHitCollection.cxx,v 2.3 2001/04/05 04:00:50 ullrich Exp $
 *
 * Author: Thomas Ullrich, July 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StFtpcSectorHitCollection.cxx,v $
 * Revision 2.3  2001/04/05 04:00:50  ullrich
 * Replaced all (U)Long_t by (U)Int_t and all redundant ROOT typedefs.
 *
 * Revision 2.2  1999/11/11 21:19:33  ullrich
 * Delete hits explicitly in destructor
 *
 * Revision 2.1  1999/10/13 19:44:42  ullrich
 * Initial Revision
 *
 **************************************************************************/
#include "StFtpcSectorHitCollection.h"
#include "StFtpcHit.h"

static const char rcsid[] = "$Id: StFtpcSectorHitCollection.cxx,v 2.3 2001/04/05 04:00:50 ullrich Exp $";

ClassImp(StFtpcSectorHitCollection)

StFtpcSectorHitCollection::StFtpcSectorHitCollection() { /* noop */ }

StFtpcSectorHitCollection::~StFtpcSectorHitCollection()
{
    //
    // Usually this wouldn't be necessary but mHits
    // is a polymorphic container and StTpcHit
    // provides its own new/delete operator.
    //
    for (unsigned int i=0; i<mHits.size(); i++) {
        delete mHits[i];
        mHits[i] = 0;
    }
}

const StSPtrVecFtpcHit&
StFtpcSectorHitCollection::hits() const { return mHits; }

StSPtrVecFtpcHit&
StFtpcSectorHitCollection::hits() { return mHits; }
