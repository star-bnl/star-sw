/****************************************************************
 * $Id: StTofHitCollection.cxx,v 1.1 2003/08/08 00:18:26 geurts Exp $
 *
 * Author: Wei-Ming Zhang, April 2001
 *
 *****************************************************************
 * Description:
 * Local TOF hits collection
 *
 *****************************************************************
 *
 * $Log: StTofHitCollection.cxx,v $
 * Revision 1.1  2003/08/08 00:18:26  geurts
 * moved from StTofMaker to StTofUtil
 *
 * Revision 1.3  2001/09/28 18:40:02  llope
 * first release
 *
 * Revision 1.1  2001/04/24 20:27:08  wzhang
 * First release
 *
 *
 ****************************************************************/
#include "StTofHitCollection.h"
#include "StTofHit.h"

StTofHitCollection::StTofHitCollection() {/* nope */}
StTofHitCollection::~StTofHitCollection(){/* nope */}

void StTofHitCollection::clear() {mHitVector.clear(); }

bool StTofHitCollection::push_back(StTofHit* hit){
  mHitVector.push_back(hit);
  return true;
}

StTofHit* StTofHitCollection::front() const {
  return mHitVector.front();
}

StTofHit* StTofHitCollection::getHit(size_t index) const {
  return mHitVector[index];
}

StTofHit* StTofHitCollection::back() const {
  return mHitVector.back();
}

size_t StTofHitCollection::size() const {
  return mHitVector.size();
}
