/***************************************************************************
 *
 * $Id: StSsdHitCollection.h,v 2.1 2000/01/05 16:00:07 ullrich Exp $
 *
 * Author: Lilian Martin, Thomas Ullrich, Dec 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StSsdHitCollection.h,v $
 * Revision 2.1  2000/01/05 16:00:07  ullrich
 * Initial Revision.
 *
 **************************************************************************/
#ifndef StSsdHitCollection_hh
#define StSsdHitCollection_hh

#include "StObject.h"
#include "StSsdLadderHitCollection.h"
class StSsdHit;

class StSsdHitCollection : public StObject {
public:
    StSsdHitCollection();
    ~StSsdHitCollection();
    // StSsdHitCollection(const StSsdHitCollection&);            use default
    // StSsdHitCollection& operator=(const StSsdHitCollection&); use default
    
    Bool_t  addHit(StSsdHit*);
    ULong_t numberOfHits() const;
    UInt_t  numberOfLadders() const;
    
    StSsdLadderHitCollection*       ladder(UInt_t);
    const StSsdLadderHitCollection* ladder(UInt_t) const;

private:
    enum { mNumberOfLadders = 20 };
    StSsdLadderHitCollection mLadders[mNumberOfLadders];
    
    ClassDef(StSsdHitCollection,1)
};
#endif
