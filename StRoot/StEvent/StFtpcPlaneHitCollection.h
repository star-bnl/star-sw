/***************************************************************************
 *
 * $Id: StFtpcPlaneHitCollection.h,v 2.1 1999/10/13 19:43:09 ullrich Exp $
 *
 * Author: Thomas Ullrich, July 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StFtpcPlaneHitCollection.h,v $
 * Revision 2.1  1999/10/13 19:43:09  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef StFtpcPlaneHitCollection_hh
#define StFtpcPlaneHitCollection_hh

#include "StObject.h"
#include "StFtpcSectorHitCollection.h"

class StFtpcPlaneHitCollection : public StObject {
public:
    StFtpcPlaneHitCollection();
    // StFtpcPlaneHitCollection(const StFtpcPlaneHitCollection&);            use default
    // StFtpcPlaneHitCollection& operator=(const StFtpcPlaneHitCollection&); use default
    ~StFtpcPlaneHitCollection();
    
    ULong_t numberOfHits() const;
    UInt_t  numberOfSectors() const;
    
    StFtpcSectorHitCollection*       sector(UInt_t);
    const StFtpcSectorHitCollection* sector(UInt_t) const;

private:
    enum { mNumberOfSectors = 6 };
    StFtpcSectorHitCollection mSectors[mNumberOfSectors];
    
    ClassDef(StFtpcPlaneHitCollection,1)
};
#endif
