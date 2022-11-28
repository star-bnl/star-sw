/***************************************************************************
 *
 * $Id: StMuFstCollection.cxx
 *
 * Author: tchuang, 2022
 ***************************************************************************
 *
 * Description: Fst data interface to StMuFstRawHit and StMuFstHit
 *
 ***************************************************************************/

#include "StMuDSTMaker/COMMON/StMuFstCollection.h"
#include "StMuDSTMaker/COMMON/StMuFstRawHit.h"
#include "StMuDSTMaker/COMMON/StMuFstHit.h"

#include "St_base/StMessMgr.h"

ClassImp(StMuFstCollection)

StMuFstCollection::StMuFstCollection() { mRawHits = 0; mHits = 0;  }

StMuFstCollection::~StMuFstCollection() {
    delete mRawHits;
    delete mHits;
    mRawHits = nullptr;
    mHits = nullptr;
}

void StMuFstCollection::init() {
    mRawHits   = new TClonesArray("StMuFstRawHit", 0);
    mHits   = new TClonesArray("StMuFstHit", 0);
}

StMuFstRawHit* StMuFstCollection::addRawHit() {
    if (!mRawHits) init();
    int counter = mRawHits->GetEntriesFast();
    return new ((*mRawHits)[counter]) StMuFstRawHit;
}

StMuFstHit* StMuFstCollection::addHit() {
    if (!mHits) init();
    int counter = mHits->GetEntriesFast();
    return new ((*mHits)[counter]) StMuFstHit;
}

unsigned int StMuFstCollection::numberOfRawHits() const {
    if (!mRawHits) return 0;
    return mRawHits->GetEntriesFast();
}

unsigned int StMuFstCollection::numberOfHits() const {
    if (!mHits) return 0;
    return mHits->GetEntriesFast();
}

StMuFstRawHit* StMuFstCollection::getRawHit(int index) {
    if (!mRawHits) return NULL;
    return static_cast<StMuFstRawHit*>(mRawHits->At(index));
}

StMuFstHit* StMuFstCollection::getHit(int index) {
    if (!mHits) return NULL;
    return static_cast<StMuFstHit*>(mHits->At(index));
}
