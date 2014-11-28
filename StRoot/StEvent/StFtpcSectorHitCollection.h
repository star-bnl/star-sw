/*!
 * \class StFtpcSectorHitCollection 
 * \author Thomas Ullrich, July 1999
 */
/***************************************************************************
 *
 * $Id: StFtpcSectorHitCollection.h,v 2.3 2002/02/22 22:56:48 jeromel Exp $
 *
 * Author: Thomas Ullrich, July 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StFtpcSectorHitCollection.h,v $
 * Revision 2.3  2002/02/22 22:56:48  jeromel
 * Doxygen basic documentation in all header files. None of this is required
 * for QM production.
 *
 * Revision 2.2  1999/10/28 22:25:24  ullrich
 * Adapted new StArray version. First version to compile on Linux and Sun.
 *
 * Revision 2.1  1999/10/13 19:43:11  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef StFtpcSectorHitCollection_hh
#define StFtpcSectorHitCollection_hh

#include "StObject.h"
#include "StContainers.h"

class StFtpcHit;

class StFtpcSectorHitCollection : public StObject {
public:
    StFtpcSectorHitCollection();
    // StFtpcSectorHitCollection(const StFtpcSectorHitCollection&);            use default
    // StFtpcSectorHitCollection& operator=(const StFtpcSectorHitCollection&); use default
    ~StFtpcSectorHitCollection();
    
    StSPtrVecFtpcHit&       hits();
    const StSPtrVecFtpcHit& hits() const;

private:
    StSPtrVecFtpcHit mHits;
    
    ClassDef(StFtpcSectorHitCollection,1)
};
#endif
