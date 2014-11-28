/****************************************************************
 *
 * $Id: StBTofRawHitCollection.cxx,v 1.1 2009/02/02 21:57:21 dongx Exp $
 *
 * Author: Xin Dong
 *
 *****************************************************************
 *
 * Description:
 * Local TOF raw hit colletion
 *
 *****************************************************************
 *
 * $Log: StBTofRawHitCollection.cxx,v $
 * Revision 1.1  2009/02/02 21:57:21  dongx
 * first release
 *
 *
 ****************************************************************/
#include "StBTofRawHitCollection.h"
#include "StBTofRawHit.h"

StBTofRawHitCollection::StBTofRawHitCollection() {/* nope */}
StBTofRawHitCollection::~StBTofRawHitCollection(){/* nope */}

void StBTofRawHitCollection::clear() {mHitVector.clear();}

bool StBTofRawHitCollection::push_back(StBTofRawHit* hit) {
        mHitVector.push_back(hit);
        return true;
}

StBTofRawHit* StBTofRawHitCollection::front() const {
    return mHitVector.front();
}

StBTofRawHit* StBTofRawHitCollection::getRawHit(size_t index) const {
    return mHitVector[index];
}

StBTofRawHit* StBTofRawHitCollection::back() const {
    return mHitVector.back();
}

size_t StBTofRawHitCollection::size() const {
    return mHitVector.size();
}
