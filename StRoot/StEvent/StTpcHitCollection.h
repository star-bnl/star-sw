/***************************************************************************
 *
 * $Id: StTpcHitCollection.h,v 2.2 2001/04/05 04:00:44 ullrich Exp $
 *
 * Author: Thomas Ullrich, July 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StTpcHitCollection.h,v $
 * Revision 2.2  2001/04/05 04:00:44  ullrich
 * Replaced all (U)Long_t by (U)Int_t and all redundant ROOT typedefs.
 *
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
    
    bool          addHit(StTpcHit*);
    unsigned int  numberOfHits() const;
    unsigned int  numberOfSectors() const;
    
    StTpcSectorHitCollection*       sector(unsigned int);
    const StTpcSectorHitCollection* sector(unsigned int) const;

private:
    enum { mNumberOfSectors = 24 };
    StTpcSectorHitCollection mSectors[mNumberOfSectors];
    
    ClassDef(StTpcHitCollection,1)
};
#endif
