/***************************************************************************
 *
 * $Id: StMuFstCollection.h
 *
 * Author: tchuang, 2022
 ***************************************************************************
 *
 * Description: Fst data interface to StMuFstRawHit and StMuFstHit
 *
 ***************************************************************************
 *
 *
 **************************************************************************/
#ifndef StMuFstCollection_hh
#define StMuFstCollection_hh

#include <TObject.h>
#include "TClonesArray.h"

class StMuFstRawHit;
class StMuFstHit;

class StMuFstCollection : public TObject {
public:
    StMuFstCollection();
    ~StMuFstCollection();
    
    void            init();
    StMuFstRawHit*   addRawHit();
    StMuFstHit*   addHit();

    unsigned int    numberOfRawHits() const;
    unsigned int    numberOfHits() const;

    void            setFstRawHitArray(TClonesArray* array) {mRawHits=array;}
    void            setFstHitArray(TClonesArray* array) {mHits=array;}

    StMuFstRawHit*   getRawHit(int index);
    StMuFstHit*   getHit(int index);

    TClonesArray*   getRawHitArray() { return mRawHits; }
    TClonesArray*   getHitArray() { return mHits; }

private:
    TClonesArray* mRawHits=0;
    TClonesArray* mHits=0;

    ClassDef(StMuFstCollection,1)
};
#endif
