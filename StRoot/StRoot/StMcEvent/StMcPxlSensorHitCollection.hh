/***************************************************************************
 * $Id: StMcPxlSensorHitCollection.hh,v 2.1 2013/03/25 23:50:36 perev Exp $
 * $Log: StMcPxlSensorHitCollection.hh,v $
 * Revision 2.1  2013/03/25 23:50:36  perev
 * Mustafa.Pxl add
 *
 *
 *
 **************************************************************************/
#ifndef StMcPxlSensorHitCollection_hh
#define StMcPxlSensorHitCollection_hh

#include "StObject.h"
#include "StMcContainers.hh"

class StMcPxlHit;

class StMcPxlSensorHitCollection : public StObject {
public:
    StMcPxlSensorHitCollection();
    ~StMcPxlSensorHitCollection();

    StSPtrVecMcPxlHit&       hits();
    const StSPtrVecMcPxlHit& hits() const;
    
private:
    StSPtrVecMcPxlHit mHits;
    
    ClassDef(StMcPxlSensorHitCollection,1)
};
#endif
