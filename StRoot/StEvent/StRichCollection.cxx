/***************************************************************************
 *
 * $Id: StRichCollection.cxx,v 2.1 2000/05/22 21:48:14 ullrich Exp $
 *
 * Author: Brian Lasiuk, May 2000
 ***************************************************************************
 *
 * Description:
 *   Persistent data which is written into StEvent
 *   directly from the reco chain
 ***************************************************************************
 *
 * $Log: StRichCollection.cxx,v $
 * Revision 2.1  2000/05/22 21:48:14  ullrich
 * Initial Revision.
 *
 **************************************************************************/
#include "StRichCollection.h"

static const char rcsid[] = "$Id: StRichCollection.cxx,v 2.1 2000/05/22 21:48:14 ullrich Exp $";

ClassImp(StRichCollection)
    
StRichCollection::StRichCollection() { /* noop */ }

StRichCollection::~StRichCollection() { /* noop */ }


const StSPtrVecRichPixel&
StRichCollection::getRichPixels() const
{
    return mRichPixels;
}

StSPtrVecRichPixel&
StRichCollection::getRichPixels()
{
    return mRichPixels;
}

const StSPtrVecRichCluster&
StRichCollection::getRichClusters() const
{
    return mRichClusters;
}

StSPtrVecRichCluster&
StRichCollection::getRichClusters()
{
    return mRichClusters;
}

const StSPtrVecRichHit&
StRichCollection::getRichHits() const
{
    return mRichHits;
}

StSPtrVecRichHit&
StRichCollection::getRichHits()
{
    return mRichHits;
}

void
StRichCollection::addPixel(const StRichPixel* aPix)
{
    mRichPixels.push_back(aPix);
}

void
StRichCollection::addCluster(const StRichCluster* aClus)
{
    mRichClusters.push_back(aClus);
}

void
StRichCollection::addHit(const StRichHit* aHit)
{
    mRichHits.push_back(aHit);
}

Bool_t
StRichCollection::pixelsPresent() const
{
    if(mRichPixels.size() > 0)
	return kTRUE;
    else
	return kFALSE;
}

Bool_t
StRichCollection::clustersPresent() const
{
    if(mRichClusters.size() > 0)
	return kTRUE;
    else
	return kFALSE;
}

Bool_t
StRichCollection::hitsPresent() const
{
        if(mRichHits.size() > 0)
	return kTRUE;
    else
	return kFALSE;
}
