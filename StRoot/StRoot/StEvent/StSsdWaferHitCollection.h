/*!
 * \class StSsdWaferHitCollection 
 * \author Lilian Martin, Thomas Ullrich, Dec 1999
 */
/***************************************************************************
 *
 * $Id: StSsdWaferHitCollection.h,v 2.2 2002/02/22 22:56:50 jeromel Exp $
 *
 * Author: Lilian Martin, Thomas Ullrich, Dec 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StSsdWaferHitCollection.h,v $
 * Revision 2.2  2002/02/22 22:56:50  jeromel
 * Doxygen basic documentation in all header files. None of this is required
 * for QM production.
 *
 * Revision 2.1  2000/01/05 16:00:13  ullrich
 * Initial Revision.
 *
 **************************************************************************/
#ifndef StSsdWaferHitCollection_hh
#define StSsdWaferHitCollection_hh

#include "StObject.h"
#include "StContainers.h"

class StSsdHit;

class StSsdWaferHitCollection : public StObject {
public:
    StSsdWaferHitCollection();
    // StSsdWaferHitCollection(const StSsdWaferHitCollection&); use default
    // const StSsdWaferHitCollection&
    // operator=(const StSsdWaferHitCollection&);               use default
    ~StSsdWaferHitCollection();
    
    StSPtrVecSsdHit&       hits();
    const StSPtrVecSsdHit& hits() const;

private:
    StSPtrVecSsdHit mHits;
    
    ClassDef(StSsdWaferHitCollection,1)
};
#endif
