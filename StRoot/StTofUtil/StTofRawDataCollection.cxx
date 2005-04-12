/****************************************************************
 *
 * $Id: StTofRawDataCollection.cxx,v 1.1 2005/04/12 17:29:29 dongx Exp $
 *
 * Author: Xin Dong
 *
 *****************************************************************
 *
 * Description:
 * Local TOF raw data colletion - only valid hits
 *
 *****************************************************************
 *
 * $Log: StTofRawDataCollection.cxx,v $
 * Revision 1.1  2005/04/12 17:29:29  dongx
 * first release, a new data format in StEvent for year 5
 *
 *
 ****************************************************************/
#include "StTofRawDataCollection.h"
#include "StTofRawData.h"

StTofRawDataCollection::StTofRawDataCollection() {/* nope */}
StTofRawDataCollection::~StTofRawDataCollection(){/* nope */}

void StTofRawDataCollection::clear() {mDataVector.clear();}

bool StTofRawDataCollection::push_back(StTofRawData* data) {
        mDataVector.push_back(data);
        return true;
}

StTofRawData* StTofRawDataCollection::front() const {
    return mDataVector.front();
}

StTofRawData* StTofRawDataCollection::getRawData(size_t index) const {
    return mDataVector[index];
}

StTofRawData* StTofRawDataCollection::back() const {
    return mDataVector.back();
}

size_t StTofRawDataCollection::size() const {
    return mDataVector.size();
}
