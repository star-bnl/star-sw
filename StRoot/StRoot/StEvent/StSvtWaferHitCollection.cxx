/***************************************************************************
 *
 * $Id: StSvtWaferHitCollection.cxx,v 2.3 2001/04/05 04:00:56 ullrich Exp $
 *
 * Author: Thomas Ullrich, Sep 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StSvtWaferHitCollection.cxx,v $
 * Revision 2.3  2001/04/05 04:00:56  ullrich
 * Replaced all (U)Long_t by (U)Int_t and all redundant ROOT typedefs.
 *
 * Revision 2.2  1999/11/11 21:19:35  ullrich
 * Delete hits explicitly in destructor
 *
 * Revision 2.1  1999/10/13 19:45:22  ullrich
 * Initial Revision
 *
 **************************************************************************/
#include "StSvtWaferHitCollection.h"
#include "StSvtHit.h"

static const char rcsid[] = "$Id: StSvtWaferHitCollection.cxx,v 2.3 2001/04/05 04:00:56 ullrich Exp $";

ClassImp(StSvtWaferHitCollection)

StSvtWaferHitCollection::StSvtWaferHitCollection() { /* noop */ }

StSvtWaferHitCollection::~StSvtWaferHitCollection()
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


const StSPtrVecSvtHit&
StSvtWaferHitCollection::hits() const { return mHits; }

StSPtrVecSvtHit&
StSvtWaferHitCollection::hits() { return mHits; }
