/***************************************************************************
 *
 * $Id: StGlobalTrack.cxx,v 1.2 1999/02/10 02:17:35 fisyak Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 *
 * History:
 * 15/01/1999 T. Wenaus  Add table-based constructor
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StGlobalTrack.cxx,v $
 * Revision 1.2  1999/02/10 02:17:35  fisyak
 * Merging with new Torre stuff
 *
 * Revision 1.4  1999/02/12 02:01:16  wenaus
 * New track constructor to load helix params independently of table
 *
 * Revision 1.3  1999/02/10 21:50:30  wenaus
 * Plug memory leaks
 *
static const Char_t rcsid[] = "$Id: StGlobalTrack.cxx,v 1.2 1999/02/10 02:17:35 fisyak Exp $";

#ifdef __ROOT__
 *
static const Char_t rcsid[] = "$Id: StGlobalTrack.cxx,v 1.2 1999/02/10 02:17:35 fisyak Exp $";
#endif
#include "StGlobalTrack.h"
static const char rcsid[] = "$Id: StGlobalTrack.cxx,v 1.2 1999/02/10 02:17:35 fisyak Exp $";
    mEmcHit = 0;  
    mSmdHit = 0;  
StCollectionImp(GlobalTrack)
StGlobalTrack::StGlobalTrack()
{
    mSvtHits = 0;
    mFtpcHits = 0;
StGlobalTrack::StGlobalTrack(dst_track_st* trk) : StTrack(trk)
                             double curvature,
    mSvtHits = 0;
    mFtpcHits = 0;
StGlobalTrack::~StGlobalTrack() { /* noop */ }

void StGlobalTrack::setEmcHit(StEmcHit* val) { mEmcHit = val; }

void StGlobalTrack::setSmdHit(StSmdHit* val) { mSmdHit = val; }
    SafeDelete(mFtpcHits);
    SafeDelete(mTpcDedx);
    SafeDelete(mPidTraits);
    SafeDelete(mFtpcDedx);
    SafeDelete(mSvtDedx);
}
StGlobalTrack::StGlobalTrack() {/* noop */}
void StGlobalTrack::setTpcDedx(StDedx* val) { mTpcDedx = val; }   
StGlobalTrack::StGlobalTrack(const dst_track_st& track) : StTrack(track) {/* noop */}
    mTpcHits.push_back(hit);
void StGlobalTrack::setSvtDedx(StDedx* val) { mSvtDedx = val; }  

void StGlobalTrack::addTpcHit(StTpcHit* hit)

    mTpcHits->push_back(hit);
    mFtpcHits.push_back(hit);
    if (this != &track)
        static_cast<StTrack&>(*this) = track;
void StGlobalTrack::addFtpcHit(StFtpcHit* hit)
{
    mFtpcHits->push_back(hit);
    mSvtHits.push_back(hit);
}
}
void StGlobalTrack::addSvtHit(StSvtHit* hit)
{
    mSvtHits->push_back(hit);
    StVecPtrTpcHitIterator iter = find(mTpcHits.begin(), mTpcHits.end(), hit);
    if (iter != mTpcHits.end()) mTpcHits.erase(iter);
}
  while (mTpcHits->Contains(hit)) {
    Int_t i = hit->trackReferenceCount();
    hit->setTrackReferenceCount(i > 0 ? i-1 : 0);
  }
	hit->setTrackReferenceCount(i > 0 ? i-1 : 0);
    StVecPtrFtpcHitIterator iter = find(mFtpcHits.begin(), mFtpcHits.end(), hit);
    if (iter != mFtpcHits.end()) {
	mFtpcHits.erase(iter);
	Int_t i = hit->trackReferenceCount();
	hit->setTrackReferenceCount(i > 0 ? i-1 : 0);
    }
    Int_t i = hit->trackReferenceCount();
    hit->setTrackReferenceCount(i > 0 ? i-1 : 0);
  }
	hit->setTrackReferenceCount(i > 0 ? i-1 : 0);
    StVecPtrSvtHitIterator iter = find(mSvtHits.begin(), mSvtHits.end(), hit);
    if (iter != mSvtHits.end()) {
	mSvtHits.erase(iter);
	Int_t i = hit->trackReferenceCount();
	hit->setTrackReferenceCount(i > 0 ? i-1 : 0);
    }
    Int_t i = hit->trackReferenceCount();
    hit->setTrackReferenceCount(i > 0 ? i-1 : 0);
  }
	hit->setTrackReferenceCount(i > 0 ? i-1 : 0);
	mNumberOfSvtHits--;
{
    mNumberOfFtpcHits = n;
}

const StVertex*
StGlobalTrack::vertex() const { return 0; }
