/***************************************************************************
 *
 * $Id: StSvtLadderHitCollection.h,v 2.1 1999/10/13 19:43:46 ullrich Exp $
 *
 * Author: Thomas Ullrich, Sep 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StSvtLadderHitCollection.h,v $
 * Revision 2.1  1999/10/13 19:43:46  ullrich
 * Initial Revision
 *
 * Revision 2.1  1999/10/13 19:43:46  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef StSvtLadderHitCollection_hh
#define StSvtLadderHitCollection_hh

#include "StObject.h"
#include "StSvtWaferHitCollection.h"

class StSvtLadderHitCollection : public StObject {
public:
    StSvtLadderHitCollection();
    ~StSvtLadderHitCollection();
    // StSvtLadderHitCollection(const StSvtLadderHitCollection&); use default
    // const StSvtLadderHitCollection&
    // operator=(const StSvtLadderHitCollection&);                use default
    
    ULong_t numberOfHits() const;
    UInt_t  numberOfWafers() const;
    
    StSvtWaferHitCollection*       wafer(UInt_t);
    const StSvtWaferHitCollection* wafer(UInt_t) const;

    void setLayerNumber(Int_t);
    
private:
    enum { mMaxNumberOfWafers = 7 };
    Int_t                    mLayerNumber;
    StSvtWaferHitCollection  mWafers[mMaxNumberOfWafers];
    
    ClassDef(StSvtLadderHitCollection,1)
};
#endif
