/***************************************************************************
 * First version of StMcHpdLayerHitColleciton.hh
 **************************************************************************/
#ifndef StMcHpdLayerHitCollection_hh
#define StMcHpdLayerHitCollection_hh

#include "StMcContainers.hh"
#include "StObject.h"

class StMcHpdHit;

class StMcHpdLayerHitCollection : public StObject {
public:
    StMcHpdLayerHitCollection();
    ~StMcHpdLayerHitCollection();
    
    unsigned long numberOfHits() const;

    StSPtrVecMcHpdHit&       hits();
    const StSPtrVecMcHpdHit& hits() const; 

private:
    StSPtrVecMcHpdHit mHits;
    ClassDef(StMcHpdLayerHitCollection,1)
};
#endif
