/****************************************************************
 * $Id: StMuTofHitCollection.cxx,v 1.2 2004/04/06 01:48:09 perev Exp $
 *
 * Author: Xin Dong, April 2004
 *
 *****************************************************************
 * Description:
 * TOF hits collection for Micro Dst
 *
 *****************************************************************
 *
 * $Log: StMuTofHitCollection.cxx,v $
 * Revision 1.2  2004/04/06 01:48:09  perev
 * Small leak + incorrect filing StMuTofHitCollection
 *
 * Revision 1.1  2004/04/02 03:39:12  jeromel
 * New files for TTOF
 *
 ****************************************************************/

#include <memory>

#include "StGlobals.hh"

#include "StMuTofHitCollection.h"

StMuTofHitCollection::StMuTofHitCollection()
{ }

StMuTofHitCollection::~StMuTofHitCollection()
{
  clear();
}

void
StMuTofHitCollection::clear()
{
    int n = mHitVector.size();
    for (int i=0;i<n;i++) { delete mHitVector[i];}
    mHitVector.clear();
}

bool
StMuTofHitCollection::push_back(StMuTofHit* hit)
{
        mHitVector.push_back(hit);
        return true;
}

StMuTofHit*
StMuTofHitCollection::front() const
{
    return mHitVector.front();
}

StMuTofHit*
StMuTofHitCollection::getHit(size_t index) const
{
    return mHitVector[index];
}

StMuTofHit*
StMuTofHitCollection::back() const
{
    return mHitVector.back();
}

size_t
StMuTofHitCollection::size() const
{
    return mHitVector.size();
}

