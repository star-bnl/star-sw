/***************************************************************************
 *
 * $Id: StSvtHitCollection.h,v 2.1 1999/10/13 19:43:44 ullrich Exp $
 *
 * Author: Thomas Ullrich, Sep 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StSvtHitCollection.h,v $
 * Revision 2.1  1999/10/13 19:43:44  ullrich
 * Initial Revision
 *
 * Revision 2.1  1999/10/13 19:43:44  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef StSvtHitCollection_hh
#define StSvtHitCollection_hh

#include "StObject.h"
#include "StSvtLayerHitCollection.h"
class StSvtHit;

class StSvtHitCollection : public StObject {
public:
    StSvtHitCollection();
    ~StSvtHitCollection();
    // StSvtHitCollection(const StSvtHitCollection&);            use default
    // StSvtHitCollection& operator=(const StSvtHitCollection&); use default
    
    Bool_t  addHit(StSvtHit*);
    ULong_t numberOfHits() const;
    UInt_t  numberOfLayers() const;
    
    StSvtLayerHitCollection*       layer(UInt_t);
    const StSvtLayerHitCollection* layer(UInt_t) const;

private:
    enum { mNumberOfLayers = 6 };
    StSvtLayerHitCollection mLayers[mNumberOfLayers];
    
    ClassDef(StSvtHitCollection,1)
};
#endif
