/****************************************************************
 *
 * Author: Mike Lisa
 *
 *****************************************************************
 *
 * Description:
 *  Patterned after StMuBTofCollection
 *****************************************************************
 *
 * 
 ****************************************************************/

#include <memory>
#include "StGlobals.hh"
#include "StMuEpdHitCollection.h"

StMuEpdHitCollection::StMuEpdHitCollection()
{ }

StMuEpdHitCollection::~StMuEpdHitCollection()
{
  clear();
}

void
StMuEpdHitCollection::clear()
{
    int n = mHitVector.size();
    for (int i=0;i<n;i++) { delete mHitVector[i];}
    mHitVector.clear();
}

bool
StMuEpdHitCollection::push_back(StMuEpdHit* hit)
{
        mHitVector.push_back(hit);
        return true;
}

StMuEpdHit*
StMuEpdHitCollection::front() const
{
    return mHitVector.front();
}

StMuEpdHit*
StMuEpdHitCollection::getHit(size_t index) const
{
    return mHitVector[index];
}

StMuEpdHit*
StMuEpdHitCollection::back() const
{
    return mHitVector.back();
}

size_t
StMuEpdHitCollection::size() const
{
    return mHitVector.size();
}

