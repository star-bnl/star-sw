/***************************************************************************
 *
 * $Id: StSsdWaferHitCollection.h,v 2.1 2000/01/05 16:00:13 ullrich Exp $
 *
 * Author: Lilian Martin, Thomas Ullrich, Dec 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StSsdWaferHitCollection.h,v $
 * Revision 2.1  2000/01/05 16:00:13  ullrich
 * Initial Revision.
 *
 **************************************************************************/
#ifndef StSsdWaferHitCollection_hh
#define StSsdWaferHitCollection_hh

#include "StObject.h"
#include "StContainers.h"

class StSsdHit;

class StSsdWaferHitCollection : public StObject {
public:
    StSsdWaferHitCollection();
    // StSsdWaferHitCollection(const StSsdWaferHitCollection&); use default
    // const StSsdWaferHitCollection&
    // operator=(const StSsdWaferHitCollection&);               use default
    ~StSsdWaferHitCollection();
    
    StSPtrVecSsdHit&       hits();
    const StSPtrVecSsdHit& hits() const;

private:
    StSPtrVecSsdHit mHits;
    
    ClassDef(StSsdWaferHitCollection,1)
};
#endif
