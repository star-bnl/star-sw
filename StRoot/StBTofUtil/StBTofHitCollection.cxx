/****************************************************************
 * $Id: StBTofHitCollection.cxx,v 1.1 2009/02/02 21:57:09 dongx Exp $
 *
 * Author: Xin Dong, Dec 2008
 *
 *****************************************************************
 * Description:
 * Local Barrel TOF hits collection
 *
 *****************************************************************
 *
 * $Log: StBTofHitCollection.cxx,v $
 * Revision 1.1  2009/02/02 21:57:09  dongx
 * first release
 *
 *
 ****************************************************************/
#include "StBTofHitCollection.h"
#include "StBTofHit.h"

StBTofHitCollection::StBTofHitCollection() {/* nope */}
StBTofHitCollection::~StBTofHitCollection(){/* nope */}

void StBTofHitCollection::clear() {mHitVector.clear(); }

bool StBTofHitCollection::push_back(StBTofHit* hit){
  mHitVector.push_back(hit);
  return true;
}

StBTofHit* StBTofHitCollection::front() const {
  return mHitVector.front();
}

StBTofHit* StBTofHitCollection::getHit(size_t index) const {
  return mHitVector[index];
}

StBTofHit* StBTofHitCollection::back() const {
  return mHitVector.back();
}

size_t StBTofHitCollection::size() const {
  return mHitVector.size();
}
