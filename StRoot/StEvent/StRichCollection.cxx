/***************************************************************************
 *
 * $Id: StRichCollection.cxx,v 2.3 2001/04/05 04:00:52 ullrich Exp $
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
 * Revision 2.3  2001/04/05 04:00:52  ullrich
 * Replaced all (U)Long_t by (U)Int_t and all redundant ROOT typedefs.
 *
 * Revision 2.2  2001/02/22 21:04:16  lasiuk
 * keep the tracks that fly through the RICH in
 * the collection
 *
 * Revision 2.1  2000/05/22 21:48:14  ullrich
 * Initial Revision.
 *
 **************************************************************************/
#include "StRichCollection.h"

static const char rcsid[] = "$Id: StRichCollection.cxx,v 2.3 2001/04/05 04:00:52 ullrich Exp $";

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

const StPtrVecTrack&
StRichCollection::getTracks() const
{
    return mTracks;
}

StPtrVecTrack&
StRichCollection::getTracks()
{
    return mTracks;
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

void
StRichCollection::addTrack(const StTrack* track)
{
    mTracks.push_back(track);
}

bool
StRichCollection::pixelsPresent() const
{
    if(mRichPixels.size() > 0)
        return kTRUE;
    else
        return kFALSE;
}

bool
StRichCollection::clustersPresent() const
{
    if(mRichClusters.size() > 0)
        return kTRUE;
    else
        return kFALSE;
}

bool
StRichCollection::hitsPresent() const
{
        if(mRichHits.size() > 0)
        return kTRUE;
    else
        return kFALSE;
}
