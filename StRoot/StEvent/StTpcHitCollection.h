/*!
 * \class StTpcHitCollection 
 * \author Thomas Ullrich, July 1999
 */
/***************************************************************************
 *
 * $Id: StTpcHitCollection.h,v 2.4 2009/11/23 16:34:07 fisyak Exp $
 *
 * Author: Thomas Ullrich, July 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StTpcHitCollection.h,v $
 * Revision 2.4  2009/11/23 16:34:07  fisyak
 * Cleanup, remove dependence on dst tables, clean up software monitors
 *
 * Revision 2.3  2002/02/22 22:56:52  jeromel
 * Doxygen basic documentation in all header files. None of this is required
 * for QM production.
 *
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
  StTpcHitCollection() {}
  ~StTpcHitCollection() {}
    // StTpcHitCollection(const StTpcHitCollection&);            use default
    // StTpcHitCollection& operator=(const StTpcHitCollection&); use default
    
    Bool_t         addHit(StTpcHit*);
    UInt_t         numberOfHits() const;
    static UInt_t  numberOfSectors() {return mNumberOfSectors; }
    
    StTpcSectorHitCollection*       sector(UInt_t);
    const StTpcSectorHitCollection* sector(UInt_t) const;

private:
    enum { mNumberOfSectors = 24 };
    StTpcSectorHitCollection mSectors[mNumberOfSectors];
    
    ClassDef(StTpcHitCollection,1)
};
#endif
