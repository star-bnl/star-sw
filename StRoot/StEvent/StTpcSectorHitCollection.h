/***************************************************************************
 *
 * $Id: StTpcSectorHitCollection.h,v 2.1 1999/10/13 19:44:06 ullrich Exp $
 *
 * Author: Thomas Ullrich, July 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StTpcSectorHitCollection.h,v $
 * Revision 2.1  1999/10/13 19:44:06  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef StTpcSectorHitCollection_hh
#define StTpcSectorHitCollection_hh

#include "StObject.h"
#include "StTpcPadrowHitCollection.h"

class StTpcSectorHitCollection : public StObject {
public:
    StTpcSectorHitCollection();
    ~StTpcSectorHitCollection();
    // StTpcSectorHitCollection(const StTpcSectorHitCollection&);            use default
    // StTpcSectorHitCollection& operator=(const StTpcSectorHitCollection&); use default
    
    ULong_t numberOfHits() const;
    UInt_t  numberOfPadrows() const;
    
    StTpcPadrowHitCollection*       padrow(UInt_t);
    const StTpcPadrowHitCollection* padrow(UInt_t) const;

private:
    enum { mNumberOfPadrows = 45 };
    StTpcPadrowHitCollection mPadrows[mNumberOfPadrows];
    
    ClassDef(StTpcSectorHitCollection,1)
};
#endif
