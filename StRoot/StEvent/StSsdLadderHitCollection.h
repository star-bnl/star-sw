/***************************************************************************
 *
 * $Id: StSsdLadderHitCollection.h,v 2.1 2000/01/05 16:00:10 ullrich Exp $
 *
 * Author: Lilian Martin, Thomas Ullrich, Dec 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StSsdLadderHitCollection.h,v $
 * Revision 2.1  2000/01/05 16:00:10  ullrich
 * Initial Revision.
 *
 **************************************************************************/
#ifndef StSsdLadderHitCollection_hh
#define StSsdLadderHitCollection_hh

#include "StObject.h"
#include "StSsdWaferHitCollection.h"

class StSsdLadderHitCollection : public StObject {
public:
    StSsdLadderHitCollection();
    ~StSsdLadderHitCollection();
    // StSsdLadderHitCollection(const StSsdLadderHitCollection&); use default
    // const StSsdLadderHitCollection&
    // operator=(const StSsdLadderHitCollection&);                use default
    
    ULong_t numberOfHits() const;
    UInt_t  numberOfWafers() const;
    
    StSsdWaferHitCollection*       wafer(UInt_t);
    const StSsdWaferHitCollection* wafer(UInt_t) const;

private:
    enum { mMaxNumberOfWafers = 16 };
    StSsdWaferHitCollection  mWafers[mMaxNumberOfWafers];
    
    ClassDef(StSsdLadderHitCollection,1)
};
#endif
