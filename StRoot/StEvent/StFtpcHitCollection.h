/***************************************************************************
 *
 * $Id: StFtpcHitCollection.h,v 2.1 1999/10/13 19:43:06 ullrich Exp $
 *
 * Author: Thomas Ullrich, Aug 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StFtpcHitCollection.h,v $
 * Revision 2.1  1999/10/13 19:43:06  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef StFtpcHitCollection_hh
#define StFtpcHitCollection_hh

#include "StObject.h"
#include "StFtpcPlaneHitCollection.h"
class StFtpcHit;

class StFtpcHitCollection : public StObject {
public:
    StFtpcHitCollection();
    // StFtpcHitCollection(const StFtpcHitCollection&);            use default
    // StFtpcHitCollection& operator=(const StFtpcHitCollection&); use default
    ~StFtpcHitCollection();
    
    Bool_t  addHit(StFtpcHit*);
    ULong_t numberOfHits() const;
    UInt_t  numberOfPlanes() const;
    
    StFtpcPlaneHitCollection*       plane(UInt_t);
    const StFtpcPlaneHitCollection* plane(UInt_t) const;

private:
    enum { mNumberOfPlanes = 20 };
    StFtpcPlaneHitCollection mPlanes[mNumberOfPlanes];
    
    ClassDef(StFtpcHitCollection,1)
};
#endif
