/***************************************************************************
 *
 * $Id: StSvtWaferHitCollection.h,v 2.1 1999/10/13 19:43:53 ullrich Exp $
 *
 * Author: Thomas Ullrich, Sep 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StSvtWaferHitCollection.h,v $
 * Revision 2.1  1999/10/13 19:43:53  ullrich
 * Initial Revision
 *
 * Revision 2.1  1999/10/13 19:43:53  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef StSvtWaferHitCollection_hh
#define StSvtWaferHitCollection_hh

#include "StObject.h"
#include "StArray.h"

class StSvtHit;

class StSvtWaferHitCollection : public StObject {
public:
    StSvtWaferHitCollection();
    // StSvtWaferHitCollection(const StSvtWaferHitCollection&); use default
    // const StSvtWaferHitCollection&
    // operator=(const StSvtWaferHitCollection&);               use default
    ~StSvtWaferHitCollection();
    
    StSPtrVecSvtHit&       hits();
    const StSPtrVecSvtHit& hits() const;

private:
    StSPtrVecSvtHit mHits;
    
    ClassDef(StSvtWaferHitCollection,1)
};
#endif
