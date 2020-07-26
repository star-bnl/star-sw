
#ifndef StStgcCollection_hh
#define StStgcCollection_hh

#include "Stiostream.h"
#include "StObject.h"
#include "StEnumerations.h"
#include "StContainers.h"

class StFtsStgcHit;

class StStgcCollection : public StObject {
public:
    StStgcCollection();
    ~StStgcCollection();
    
    void addHit(unsigned int det, StFtsStgcHit*);            // Add a hit 
    StSPtrVecFtsStgcHit& hits(unsigned int det);             // Return the hit list
    const StSPtrVecFtsStgcHit& hits(unsigned int det) const; // Return the hit list
    unsigned int numberOfHits(unsigned int det) const;   // Return the number of hits

    void print(int option=1);
    
private:
    StSPtrVecFtsStgcHit     mHits[kStgcNDet+1];
    ClassDef(StStgcCollection,0)

};

#endif
