/***************************************************************************
 *
 * $Id: StBTofCollection.cxx,v 2.1 2008/12/22 20:30:53 ullrich Exp $
 *
 * Author: Xin Dong, Nov 2008
 ***************************************************************************
 *
 * Description:
 *
 * Persistent data which is written into StEvent
 * directly from the reco chain. All Barrel ToF stuff goes here
 * except the StBTofPidTraits.
 *
 ***************************************************************************
 *
 * $Log: StBTofCollection.cxx,v $
 * Revision 2.1  2008/12/22 20:30:53  ullrich
 * Initial Revision.
 *
 *
 **************************************************************************/
#include "StBTofCollection.h"

static const char rcsid[] = "$Id: StBTofCollection.cxx,v 2.1 2008/12/22 20:30:53 ullrich Exp $";

ClassImp(StBTofCollection)
    
StBTofCollection::StBTofCollection()
{
    mBTofHeader = 0;
}

StBTofCollection::~StBTofCollection()
{
    if(mBTofHeader) delete mBTofHeader;
}

StBTofHeader*
StBTofCollection::tofHeader() { return mBTofHeader; }

const StBTofHeader*
StBTofCollection::tofHeader() const { return mBTofHeader; }

const StSPtrVecBTofHit&
StBTofCollection::tofHits() const { return mBTofHits; }

StSPtrVecBTofHit&
StBTofCollection::tofHits() { return mBTofHits; }

const StSPtrVecBTofRawHit&
StBTofCollection::tofRawHits() const { return mBTofRawHits; }

StSPtrVecBTofRawHit&
StBTofCollection::tofRawHits() { return mBTofRawHits; }

void
StBTofCollection::setHeader(StBTofHeader* val) { mBTofHeader = val; }

void
StBTofCollection::addHit(const StBTofHit* aHit)
{
    if (aHit) mBTofHits.push_back(aHit);
}

void
StBTofCollection::addRawHit(const StBTofRawHit* aRawHit)
{
    if (aRawHit) mBTofRawHits.push_back(aRawHit);
}

bool
StBTofCollection::hitsPresent() const { return mBTofHits.size(); }

bool
StBTofCollection::rawHitsPresent() const { return mBTofRawHits.size(); }
