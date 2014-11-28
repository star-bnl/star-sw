/****************************************************************
 * $Id: StTofCellCollection.cxx,v 1.1 2003/08/06 22:59:36 geurts Exp $
 *****************************************************************
 * Description: Local TOF cells collection
 *
 *****************************************************************
 * $Log: StTofCellCollection.cxx,v $
 * Revision 1.1  2003/08/06 22:59:36  geurts
 * First Release
 *  - used by TOF MatchMakers
 *
 *
 ****************************************************************/
#include "StTofCellCollection.h"
#include "StTofCell.h"

StTofCellCollection::StTofCellCollection() {/* nope */ }
StTofCellCollection::~StTofCellCollection(){/* nope */ }

void StTofCellCollection::clear() {mCellVector.clear();}

bool StTofCellCollection::push_back(StTofCell* cell) {
  mCellVector.push_back(cell);
  return true;
}

StTofCell* StTofCellCollection::front() const {
  return mCellVector.front();
}

StTofCell* StTofCellCollection::getCell(size_t index) const {
  return mCellVector[index];
}

StTofCell* StTofCellCollection::back() const {
  return mCellVector.back();
}

size_t StTofCellCollection::size() const {
    return mCellVector.size();
}
