/***************************************************************************
 *
 * $Id: StGlobalTrack.cc,v 1.1 1999/01/15 20:39:48 wenaus Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StGlobalTrack.cc,v $
 * Revision 1.1  1999/01/15 20:39:48  wenaus
 * Commit Thomas' original code
 *
 * Revision 1.5  1999/02/15 16:17:02  wenaus
#include "StGlobalTrack.hh"
 * New track constructor to load helix params independently of table
 *
 * Revision 1.3  1999/02/10 21:50:30  wenaus
    mEmcHit = 0;  
    mSmdHit = 0;  

 *
 * Revision 1.2  1999/01/15 22:53:44  wenaus

StGlobalTrack::StGlobalTrack(dst_track_st* trk,
StGlobalTrack::~StGlobalTrack() { /* noop */ }

void StGlobalTrack::setEmcHit(StEmcHit* val) { mEmcHit = val; }

void StGlobalTrack::setSmdHit(StSmdHit* val) { mSmdHit = val; }
    StTrack(trk, curvature, dip, phase, origin, h), mPidTraits(*this)
{  
  if (mTpcDedx) { delete mTpcDedx; mTpcDedx=0; }
  if (mFtpcDedx) { delete mFtpcDedx; mFtpcDedx=0; }
  if (mSvtDedx) { delete mSvtDedx; mSvtDedx=0; }
	(*k)->setTrackReferenceCount(counter > 0 ? counter-1 : 0);
    }
    
    if (mTpcDedx) { delete mTpcDedx; mTpcDedx=0; }
    if (mFtpcDedx) { delete mFtpcDedx; mFtpcDedx=0; }
    if (mSvtDedx) { delete mSvtDedx; mSvtDedx=0; }
}

void StGlobalTrack::setTpcDedx(StDedx* val) { mTpcDedx = val; }   

void StGlobalTrack::setFtpcDedx(StDedx* val) { mFtpcDedx = val; } 

void StGlobalTrack::setSvtDedx(StDedx* val) { mSvtDedx = val; }  

void StGlobalTrack::addTpcHit(StTpcHit* hit)
{
    mTpcHits.push_back(hit);
    hit->setTrackReferenceCount(hit->trackReferenceCount()+1);
}

void StGlobalTrack::addFtpcHit(StFtpcHit* hit)
{
    mFtpcHits.push_back(hit);
    hit->setTrackReferenceCount(hit->trackReferenceCount()+1);
}

void StGlobalTrack::addSvtHit(StSvtHit* hit)
{
    mSvtHits.push_back(hit);
    hit->setTrackReferenceCount(hit->trackReferenceCount()+1);
}

void StGlobalTrack::removeTpcHit(StTpcHit* hit)
{
    StVecPtrTpcHitIterator iter = find(mTpcHits.begin(), mTpcHits.end(), hit);
    if (iter != mTpcHits.end()) mTpcHits.erase(iter);
    int i = hit->trackReferenceCount();
    hit->setTrackReferenceCount(i > 0 ? i-1 : 0);
}

void StGlobalTrack::removeFtpcHit(StFtpcHit* hit)
{
    StVecPtrFtpcHitIterator iter = find(mFtpcHits.begin(), mFtpcHits.end(), hit);
    if (iter != mFtpcHits.end()) {
	mFtpcHits.erase(iter);
	int i = hit->trackReferenceCount();
	hit->setTrackReferenceCount(i > 0 ? i-1 : 0);
    }
}

void StGlobalTrack::removeSvtHit(StSvtHit* hit)
{
    StVecPtrSvtHitIterator iter = find(mSvtHits.begin(), mSvtHits.end(), hit);
    if (iter != mSvtHits.end()) {
	mSvtHits.erase(iter);
	int i = hit->trackReferenceCount();
	hit->setTrackReferenceCount(i > 0 ? i-1 : 0);
    }
}

