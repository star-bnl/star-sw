/*!
 * \class StTpcPadrowHitCollection 
 * \author Thomas Ullrich, July 1999
 */
/***************************************************************************
 *
 * $Id: StTpcPadrowHitCollection.h,v 2.3 2002/02/22 22:56:52 jeromel Exp $
 *
 * Author: Thomas Ullrich, July 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StTpcPadrowHitCollection.h,v $
 * Revision 2.3  2002/02/22 22:56:52  jeromel
 * Doxygen basic documentation in all header files. None of this is required
 * for QM production.
 *
 * Revision 2.2  1999/10/28 22:27:15  ullrich
 * Adapted new StArray version. First version to compile on Linux and Sun.
 *
 * Revision 2.1  1999/10/13 19:44:02  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef StTpcPadrowHitCollection_hh
#define StTpcPadrowHitCollection_hh

#include "StObject.h"
#include "StContainers.h"

class StTpcHit;

class StTpcPadrowHitCollection : public StObject {
public:
    StTpcPadrowHitCollection();
    ~StTpcPadrowHitCollection();
    // StTpcPadrowHitCollection(const StTpcPadrowHitCollection&); use default
    // const StTpcPadrowHitCollection&
    // operator=(const StTpcPadrowHitCollection&);                use default
    
    StSPtrVecTpcHit&       hits();
    const StSPtrVecTpcHit& hits() const;

private:
    StSPtrVecTpcHit mHits;
    
    ClassDef(StTpcPadrowHitCollection,1)
};
#endif
