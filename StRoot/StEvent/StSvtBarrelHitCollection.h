/***************************************************************************
 *
 * $Id: StSvtBarrelHitCollection.h,v 2.1 2000/02/17 18:15:11 ullrich Exp $
 *
 * Author: Thomas Ullrich, Feb 2000
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StSvtBarrelHitCollection.h,v $
 * Revision 2.1  2000/02/17 18:15:11  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef StSvtBarrelHitCollection_hh
#define StSvtBarrelHitCollection_hh

#include "StObject.h"
#include "StSvtLadderHitCollection.h"

class StSvtBarrelHitCollection : public StObject {
public:
    StSvtBarrelHitCollection();
    ~StSvtBarrelHitCollection();
    // StSvtBarrelHitCollection(const StSvtBarrelHitCollection&); use default
    // const StSvtBarrelHitCollection&
    // operator=(const StSvtBarrelHitCollection&);                use default
    
    ULong_t numberOfHits() const;
    UInt_t  numberOfLadders() const;
    
    StSvtLadderHitCollection*       ladder(UInt_t);
    const StSvtLadderHitCollection* ladder(UInt_t) const;

    void setBarrelNumber(Int_t);
    
private:
    enum { mMaxNumberOfLadders = 16 };
    Int_t                    mBarrelNumber;
    StSvtLadderHitCollection mLadders[mMaxNumberOfLadders];
    
    ClassDef(StSvtBarrelHitCollection,1)
};
#endif
