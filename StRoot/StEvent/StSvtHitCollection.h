/***************************************************************************
 *
 * $Id: StSvtHitCollection.h,v 2.2 2000/02/17 18:13:14 ullrich Exp $
 *
 * Author: Thomas Ullrich, Sep 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StSvtHitCollection.h,v $
 * Revision 2.2  2000/02/17 18:13:14  ullrich
 * Changed the SVT hit storage model. Hits are now stored according
 * to barrel/ladder/wafer not by layer/ladder/wafer.
 *
 * Revision 2.1  1999/10/13 19:43:44  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef StSvtHitCollection_hh
#define StSvtHitCollection_hh

#include "StObject.h"
#include "StSvtBarrelHitCollection.h"
class StSvtHit;

class StSvtHitCollection : public StObject {
public:
    StSvtHitCollection();
    ~StSvtHitCollection();
    // StSvtHitCollection(const StSvtHitCollection&);            use default
    // StSvtHitCollection& operator=(const StSvtHitCollection&); use default
    
    Bool_t  addHit(StSvtHit*);
    ULong_t numberOfHits() const;
    UInt_t  numberOfBarrels() const;
    
    StSvtBarrelHitCollection*       barrel(UInt_t);
    const StSvtBarrelHitCollection* barrel(UInt_t) const;

private:
    enum { mNumberOfBarrels = 3 };
    StSvtBarrelHitCollection mBarrels[mNumberOfBarrels];
    
    ClassDef(StSvtHitCollection,1)
};
#endif
