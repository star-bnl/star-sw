/***************************************************************************
 *
 * $Id: StMuFstCollection.h
 *
 * Author: tchuang, 2022
 ***************************************************************************
 *
 * Description: Fst data interface to StMuFstHit
 *
 ***************************************************************************
 *
 *
 **************************************************************************/
#ifndef StMuFstCollection_hh
#define StMuFstCollection_hh

#include <TObject.h>
#include "TClonesArray.h"

class StMuFstHit;

class StMuFstCollection : public TObject {
public:
    StMuFstCollection();
    ~StMuFstCollection();
    
    void            init();
    StMuFstHit*   addHit();

    unsigned int    numberOfHits() const;

    void            setFstHitArray(TClonesArray* array) {mHits=array;}

    StMuFstHit*   getHit(int index);

    TClonesArray*   getHitArray() { return mHits; }

private:
    TClonesArray* mHits=0;

    ClassDef(StMuFstCollection,1)
};
#endif
