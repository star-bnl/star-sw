/****************************************************************
 * $Id: StTofHitCollection.cxx,v 1.1 2001/04/24 20:27:08 wzhang Exp $
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
 * Revision 1.1  2001/04/24 20:27:08  wzhang
 * First release
 *
 *
 ****************************************************************/

#include <memory>

#include "StGlobals.hh"

#include "StTofHitCollection.h"

StTofHitCollection::StTofHitCollection()
{ }

StTofHitCollection::~StTofHitCollection()
{ }

void
StTofHitCollection::clear()
{
    mHitVector.clear();
}

bool
StTofHitCollection::push_back(StTofHit* hit)
{
        mHitVector.push_back(hit);
        return true;
}

StTofHit*
StTofHitCollection::front() const
{
    return mHitVector.front();
}

StTofHit*
StTofHitCollection::getHit(size_t index) const
{
    return mHitVector[index];
}

StTofHit*
StTofHitCollection::back() const
{
    return mHitVector.back();
}

size_t
StTofHitCollection::size() const
{
    return mHitVector.size();
}

