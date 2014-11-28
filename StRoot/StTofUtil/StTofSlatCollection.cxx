/****************************************************************
 * $Id: StTofSlatCollection.cxx,v 1.1 2003/08/08 00:18:25 geurts Exp $
 *
 * Author: Wei-Ming Zhang, April 2001
 *
 *****************************************************************
 * Description:
 * Local TOF slats collection
 *
 *****************************************************************
 *
 * $Log: StTofSlatCollection.cxx,v $
 * Revision 1.1  2003/08/08 00:18:25  geurts
 * moved from StTofMaker to StTofUtil
 *
 *
 ****************************************************************/
#include "StTofSlatCollection.h"
#include "StTofSlat.h"

StTofSlatCollection::StTofSlatCollection() {/* nope */}
StTofSlatCollection::~StTofSlatCollection(){/* nope */}

void StTofSlatCollection::clear(){mSlatVector.clear();}

bool StTofSlatCollection::push_back(StTofSlat* slat){
  mSlatVector.push_back(slat);
  return true;
}

StTofSlat* StTofSlatCollection::front() const {
  return mSlatVector.front();
}

StTofSlat* StTofSlatCollection::getSlat(size_t index) const {
  return mSlatVector[index];
}

StTofSlat* StTofSlatCollection::back() const {
  return mSlatVector.back();
}

size_t StTofSlatCollection::size() const {
  return mSlatVector.size();
}

