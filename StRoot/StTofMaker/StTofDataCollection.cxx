/****************************************************************
 *
 * $Id: StTofDataCollection.cxx,v 1.2 2002/01/22 06:52:52 geurts Exp $
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
 * Revision 1.2  2002/01/22 06:52:52  geurts
 * cvs tags added
 *
 *
 ****************************************************************/

#include <memory>
#include "StGlobals.hh"

#include "StTofDataCollection.h"

StTofDataCollection::StTofDataCollection()
{ }

StTofDataCollection::~StTofDataCollection()
{ }

void StTofDataCollection::clear()
{
    mDataVector.clear();
}

bool StTofDataCollection::push_back(StTofData* data)
{
        mDataVector.push_back(data);
        return true;
}

StTofData* 
StTofDataCollection::front() const
{
    return mDataVector.front();
}

StTofData* 
StTofDataCollection::getData(size_t index) const 
{
    return mDataVector[index];
}

StTofData* 
StTofDataCollection::back() const
{
    return mDataVector.back();
}

size_t StTofDataCollection::size() const
{
    return mDataVector.size();
}

