/***************************************************************************
 *
 * $Id: StMuFstCollection.cxx
 *
 * Author: tchuang, 2022
 ***************************************************************************
 *
 * Description: Fst data interface to StMuFstHit
 *
 ***************************************************************************/

#include "StMuDSTMaker/COMMON/StMuFstCollection.h"
#include "StMuDSTMaker/COMMON/StMuFstHit.h"

#include "St_base/StMessMgr.h"

ClassImp(StMuFstCollection)

StMuFstCollection::StMuFstCollection() { mHits = 0;  }

StMuFstCollection::~StMuFstCollection() {
    delete mHits;
    mHits = nullptr;
}

void StMuFstCollection::init() {
    mHits   = new TClonesArray("StMuFstHit", 0);
}

StMuFstHit* StMuFstCollection::addHit() {
    if (!mHits) init();
    int counter = mHits->GetEntriesFast();
    return new ((*mHits)[counter]) StMuFstHit;
}

unsigned int StMuFstCollection::numberOfHits() const {
    if (!mHits) return 0;
    return mHits->GetEntriesFast();
}

StMuFstHit* StMuFstCollection::getHit(int index) {
    if (!mHits) return NULL;
    return static_cast<StMuFstHit*>(mHits->At(index));
}
