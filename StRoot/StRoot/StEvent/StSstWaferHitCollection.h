/*!
 * \class StSstWaferHitCollection 
 * \author Jonathan Bouchet, Thomas Ullrich, May 2015
 */
/***************************************************************************
 *
 * $Id: StSstWaferHitCollection.h,v 2.1 2015/05/13 16:50:59 ullrich Exp $
 *
 * Author: Jonathan Bouchet, Thomas Ullrich, May 2015
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StSstWaferHitCollection.h,v $
 * Revision 2.1  2015/05/13 16:50:59  ullrich
 * Initial Revision.
 *
 **************************************************************************/
#ifndef StSstWaferHitCollection_hh
#define StSstWaferHitCollection_hh

#include "StObject.h"
#include "StContainers.h"

class StSstHit;

class StSstWaferHitCollection : public StObject {
public:
    StSstWaferHitCollection();
    // StSstWaferHitCollection(const StSstWaferHitCollection&); use default
    // const StSstWaferHitCollection&
    // operator=(const StSstWaferHitCollection&);               use default
    ~StSstWaferHitCollection();
    
    StSPtrVecSstHit&       hits();
    const StSPtrVecSstHit& hits() const;

private:
    StSPtrVecSstHit mHits;
    
    ClassDef(StSstWaferHitCollection,1)
};
#endif
