/****************************************************************
 * $Id: StTofSlatCollection.cxx,v 1.3 2001/09/28 18:40:03 llope Exp $
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
 * Revision 1.3  2001/09/28 18:40:03  llope
 * first release
 *
 * Revision 1.1  2001/04/24 20:27:09  wzhang
 * First release
 *
 *
 ****************************************************************/

#include <memory>

#include "StGlobals.hh"

#include "StTofSlatCollection.h"

StTofSlatCollection::StTofSlatCollection()
{ }

StTofSlatCollection::~StTofSlatCollection()
{ }

void
StTofSlatCollection::clear()
{
    mSlatVector.clear();
}

bool
StTofSlatCollection::push_back(StTofSlat* slat)
{
        mSlatVector.push_back(slat);
        return true;
}

StTofSlat*
StTofSlatCollection::front() const
{
    return mSlatVector.front();
}

StTofSlat*
StTofSlatCollection::getSlat(size_t index) const 
{
    return mSlatVector[index];
}

StTofSlat*
StTofSlatCollection::back() const
{
    return mSlatVector.back();
}

size_t
StTofSlatCollection::size() const
{
    return mSlatVector.size();
}

