/****************************************************************
 * $Id: StMuBTofHitCollection.cxx,v 1.1 2009/02/20 17:05:59 tone421 Exp $
 *
 * Author: Xin Dong, Feb. 2009
 *
 *****************************************************************
 *
 * Description:
 * TOF hits collection for Micro Dst
 *
 *****************************************************************
 *
 * $Log: StMuBTofHitCollection.cxx,v $
 * Revision 1.1  2009/02/20 17:05:59  tone421
 * *** empty log message ***
 *
 * 
 ****************************************************************/

#include <memory>
#include "StGlobals.hh"
#include "StMuBTofHitCollection.h"

StMuBTofHitCollection::StMuBTofHitCollection()
{ }

StMuBTofHitCollection::~StMuBTofHitCollection()
{
  clear();
}

void
StMuBTofHitCollection::clear()
{
    int n = mHitVector.size();
    for (int i=0;i<n;i++) { delete mHitVector[i];}
    mHitVector.clear();
}

bool
StMuBTofHitCollection::push_back(StMuBTofHit* hit)
{
        mHitVector.push_back(hit);
        return true;
}

StMuBTofHit*
StMuBTofHitCollection::front() const
{
    return mHitVector.front();
}

StMuBTofHit*
StMuBTofHitCollection::getHit(size_t index) const
{
    return mHitVector[index];
}

StMuBTofHit*
StMuBTofHitCollection::back() const
{
    return mHitVector.back();
}

size_t
StMuBTofHitCollection::size() const
{
    return mHitVector.size();
}

