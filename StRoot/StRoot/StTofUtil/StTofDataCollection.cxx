/****************************************************************
 *
 * $Id: StTofDataCollection.cxx,v 1.1 2003/08/08 00:18:25 geurts Exp $
 *
 * Author: Bill Llope
 *
 *****************************************************************
 *
 * Description:
 * Local TOF raw data colletion
 *
 *****************************************************************
 *
 * $Log: StTofDataCollection.cxx,v $
 * Revision 1.1  2003/08/08 00:18:25  geurts
 * moved from StTofMaker to StTofUtil
 *
 * Revision 1.2  2002/01/22 06:52:52  geurts
 * cvs tags added
 *
 *
 ****************************************************************/
#include "StTofDataCollection.h"
#include "StTofData.h"

StTofDataCollection::StTofDataCollection() {/* nope */}
StTofDataCollection::~StTofDataCollection(){/* nope */}

void StTofDataCollection::clear() {mDataVector.clear();}

bool StTofDataCollection::push_back(StTofData* data) {
        mDataVector.push_back(data);
        return true;
}

StTofData* StTofDataCollection::front() const {
    return mDataVector.front();
}

StTofData* StTofDataCollection::getData(size_t index) const {
    return mDataVector[index];
}

StTofData* StTofDataCollection::back() const {
    return mDataVector.back();
}

size_t StTofDataCollection::size() const {
    return mDataVector.size();
}
