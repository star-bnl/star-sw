/***************************************************************************
 *
 * $Id: StTpcPadrowHitCollection.h,v 2.1 1999/10/13 19:44:02 ullrich Exp $
 *
 * Author: Thomas Ullrich, July 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StTpcPadrowHitCollection.h,v $
 * Revision 2.1  1999/10/13 19:44:02  ullrich
 * Initial Revision
 *
 * Revision 2.1  1999/10/13 19:44:02  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef StTpcPadrowHitCollection_hh
#define StTpcPadrowHitCollection_hh

#include "StObject.h"
#include "StArray.h"

class StTpcHit;

class StTpcPadrowHitCollection : public StObject {
public:
    StTpcPadrowHitCollection();
    ~StTpcPadrowHitCollection();
    // StTpcPadrowHitCollection(const StTpcPadrowHitCollection&); use default
    // const StTpcPadrowHitCollection&
    // operator=(const StTpcPadrowHitCollection&);                use default
    
    StSPtrVecTpcHit&       hits();
    const StSPtrVecTpcHit& hits() const;

private:
    StSPtrVecTpcHit mHits;
    
    ClassDef(StTpcPadrowHitCollection,1)
};
#endif
