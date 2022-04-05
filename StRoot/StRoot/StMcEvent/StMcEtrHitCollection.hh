/***************************************************************************
 *
 ***************************************************************************/
#ifndef StMcEtrHitCollection_hh
#define StMcEtrHitCollection_hh

#include "StMcContainers.hh"
#include "StObject.h"

class StMcEtrHit;

class StMcEtrHitCollection : public StObject {
public:
    
    StMcEtrHitCollection();
    ~StMcEtrHitCollection();
    
    bool addHit(StMcEtrHit*);
    unsigned long numberOfHits() const;

    StSPtrVecMcEtrHit&       hits();
    const StSPtrVecMcEtrHit& hits() const;

    unsigned int  numberOfLayers() const;
    unsigned int  numberOfSectors() const;
    
protected:
    StSPtrVecMcEtrHit mHits;

    enum { mNumberOfLayers = 3 }; // 
    enum { mNumberOfSectors = 12 }; // 
    ClassDef(StMcEtrHitCollection,1)
};
#endif
