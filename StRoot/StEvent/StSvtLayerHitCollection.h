/***************************************************************************
 *
 * $Id: StSvtLayerHitCollection.h,v 2.1 1999/10/13 19:43:48 ullrich Exp $
 *
 * Author: Thomas Ullrich, Sep 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StSvtLayerHitCollection.h,v $
 * Revision 2.1  1999/10/13 19:43:48  ullrich
 * Initial Revision
 *
 * Revision 2.1  1999/10/13 19:43:48  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef StSvtLayerHitCollection_hh
#define StSvtLayerHitCollection_hh

#include "StObject.h"
#include "StSvtLadderHitCollection.h"

class StSvtLayerHitCollection : public StObject {
public:
    StSvtLayerHitCollection();
    ~StSvtLayerHitCollection();
    // StSvtLayerHitCollection(const StSvtLayerHitCollection&); use default
    // const StSvtLayerHitCollection&
    // operator=(const StSvtLayerHitCollection&);               use default
    
    ULong_t numberOfHits() const;
    UInt_t  numberOfLadders() const;
    
    StSvtLadderHitCollection*       ladder(UInt_t);
    const StSvtLadderHitCollection* ladder(UInt_t) const;

    void setLayerNumber(Int_t);
    
private:
    enum { mMaxNumberOfLadders = 8 };
    Int_t                    mLayerNumber;
    StSvtLadderHitCollection mLadders[mMaxNumberOfLadders];
    
    ClassDef(StSvtLayerHitCollection,1)
};
#endif
