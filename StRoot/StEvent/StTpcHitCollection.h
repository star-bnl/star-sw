/***************************************************************************
 *
 * $Id: StTpcHitCollection.h,v 2.1 1999/10/13 19:44:00 ullrich Exp $
 *
 * Author: Thomas Ullrich, July 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StTpcHitCollection.h,v $
 * Revision 2.1  1999/10/13 19:44:00  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef StTpcHitCollection_hh
#define StTpcHitCollection_hh

#include "StObject.h"
#include "StTpcSectorHitCollection.h"

class StTpcHit;

class StTpcHitCollection : public StObject {
public:
    StTpcHitCollection();
    ~StTpcHitCollection();
    // StTpcHitCollection(const StTpcHitCollection&);            use default
    // StTpcHitCollection& operator=(const StTpcHitCollection&); use default
    
    Bool_t  addHit(StTpcHit*);
    ULong_t numberOfHits() const;
    UInt_t  numberOfSectors() const;
    
    StTpcSectorHitCollection*       sector(UInt_t);
    const StTpcSectorHitCollection* sector(UInt_t) const;

private:
    enum { mNumberOfSectors = 24 };
    StTpcSectorHitCollection mSectors[mNumberOfSectors];
    
    ClassDef(StTpcHitCollection,1)
};
#endif
