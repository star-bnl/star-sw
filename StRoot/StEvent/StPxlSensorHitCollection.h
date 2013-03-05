/*!
 * \class StPxlSensorHitCollection 
 * \author X. Dong, Jan 2013
 */
/***************************************************************************
 *
 * $Id: StPxlSensorHitCollection.h,v 2.1 2013/03/05 14:40:41 ullrich Exp $
 *
 * Author: X. Dong, Jan 2013
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StPxlSensorHitCollection.h,v $
 * Revision 2.1  2013/03/05 14:40:41  ullrich
 * Initial Revision.
 *
 **************************************************************************/
#ifndef StPxlSensorHitCollection_hh
#define StPxlSensorHitCollection_hh

#include "StObject.h"
#include "StContainers.h"

class StPxlHit;

class StPxlSensorHitCollection : public StObject {
public:
    StPxlSensorHitCollection();
    ~StPxlSensorHitCollection();

    StSPtrVecPxlHit&       hits();
    const StSPtrVecPxlHit& hits() const;
    
private:
    StSPtrVecPxlHit mHits;
    
    ClassDef(StPxlSensorHitCollection,1)
};
#endif
